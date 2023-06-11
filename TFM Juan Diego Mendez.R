library(rvest)
library(pdftools)
library(tidyverse)
library(xml2)
library(lubridate)
library(stringr)


# Define base URL and number of pages to scrape
base_url <- "https://www.aepd.es/es/informes-y-resoluciones/resoluciones?f%5B0%5D=tipo_de_procedimiento%3AProcedimiento%20sancionador%20%28PS%29&search_api_fulltext=&page="
num_pages <- 680

# Initialize empty vectors to store links, dates, and procedure numbers
all_links <- c()
all_dates <- c()
all_procedure_nums <- c()

# Loop over pages and extract links, dates, and procedure numbers
for (i in 0:num_pages) {
  # Construct URL for current page
  page_url <- paste0(base_url, i)
  
  # Retrieve HTML content of page
  page <- read_html(page_url)
  
  # Extract links, dates, and procedure numbers from page
  rows <- page %>%
    xml_find_all("//div[@class='views-row']")
  
  for (row in rows) {
    link <- row %>%
      xml_find_first(".//a[@href and @target='_blank']") %>%
      xml_attr("href") %>%
      trimws()
    date <- row %>%
      xml_find_first(".//span[@class='field-content']/time[@class='datetime']") %>%
      xml_attr("datetime") %>%
      trimws()
    
    procedure_num <- row %>%
      xml_find_first(".//span[@class='field-content']") %>%
      xml_text()
    
    # Check whether both link and date were found
    if (!is.na(link) && !is.na(date)) {
      all_links <- c(all_links, paste0("https://www.aepd.es", link))
      all_dates <- c(all_dates, date)
      all_procedure_nums <- c(all_procedure_nums, procedure_num)
    } else if (!is.na(link)) {
      all_links <- c(all_links, paste0("https://www.aepd.es", link))
      all_dates <- c(all_dates, NA)
      all_procedure_nums <- c(all_procedure_nums, procedure_num)
    } else if (!is.na(date)) {
      all_links <- c(all_links, NA)
      all_dates <- c(all_dates, date)
      all_procedure_nums <- c(all_procedure_nums, procedure_num)
    } else {
      all_links <- c(all_links, NA)
      all_dates <- c(all_dates, NA)
      all_procedure_nums <- c(all_procedure_nums, NA)
    }
  }
  
  # Pause for a short time to avoid overwhelming the server
  Sys.sleep(1)
}

# Create a data frame with links, dates, and procedure numbers
df <- data.frame(links = all_links, dates = all_dates, procedure_nums = all_procedure_nums)

df1 <- df %>%
  mutate(
    links = if_else(is.na(links), paste0("https://www.aepd.es/es/documento/", procedure_nums, ".pdf"), links),
    dates = parse_date_time(dates, "ymd_HMS")
  )

# Load necessary libraries
library(pdftools)

# Create a new column to store the text of the pdfs
df1$text <- NA

# Loop over the links in the "links" column
for (i in 1:nrow(df1)) {
  # Download the pdf
  tmp_pdf <- tempfile()
  download.file(df1$links[i], tmp_pdf, mode = "wb")
  
  # Convert the pdf to plain text
  text <- pdf_text(tmp_pdf)
  
  # Flatten the text into a single string
  text_flat <- paste(text, collapse = " ")
  
  # Save the text in the new column
  df1$text[i] <- text_flat
  
  # Remove the temporary file
  unlink(tmp_pdf)
}

write.csv2(df1, "C:/Users/juanm/Box/archivos/Documentos Sync 2017/Viajes/Viaje 2022/Madrid/UC3M/TFM seminar/data_base.csv")


data_base <- read_delim("data_base.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Replace consecutive spaces and newlines with single spaces
df2 <- data_base %>% mutate(text = gsub("\\s+|\n", " ", text))

# Remove specific text patterns
df2 <- df2 %>% mutate(text = gsub("sedeagpd.gob.es|www.aepd.es|C/ Jorge Juan, 6|28001 – Madrid", "", text))

# Remove specific number patterns
df2 <- df2 %>% mutate(text = gsub("\\b\\d+/\\d+\\b", "", text))

# Extract relevant text sections
df2 <- df2 %>%
  mutate(text_extract = str_extract(text, "(?<=RESUELVE).*?(?=SEGUNDO)")) %>%
  mutate(text_extract = ifelse(is.na(text_extract), str_extract(text, "(?<=ACUERDA).*?(?=NOTIFICAR)"), text_extract)) %>%
  mutate(text_extract = ifelse(is.na(text_extract), str_extract(text, "(?<=RESUELVE).*?(?=NOTIFICAR)"), text_extract)) %>%
  mutate(text_extract = ifelse(is.na(text_extract), str_extract(text, "(?<=PROCEDER AL ARCHIVO).*?(?=NOTIFICAR)"), text_extract)) %>%
  mutate(text_extract = ifelse(is.na(text_extract), str_extract(text, "(?<=PRIMERO).*?(?=NOTIFICAR)"), text_extract)) %>%
  mutate(text_extract = ifelse(is.na(text_extract), str_extract(text, "(?<=RESUELVE).*?(?=Notificar)"), text_extract)) %>%
  mutate(text_extract = ifelse(is.na(text_extract), str_extract(text, "(?<=PRIMERO).*?(?=SEGUNDO)"), text_extract))

#actualizado 11 junio
write.csv2(df2, "C:/Users/juanm/Box/archivos/Documentos Sync 2017/Viajes/Viaje 2022/Madrid/UC3M/TFM seminar/data_base_extract1.csv")


# Classify decisions based on the presence of specific words
df3 <- df2 %>%
  mutate(decision = case_when(
    str_detect(text_extract, "\\bARCHIVO\\b|\\bARCHIVAR\\b|\\barchivo\\b|\\bArchivo\\b|\\bArchivar\\b|\\bno existencia de\\b") ~ "dismissed",
    str_detect(text_extract, "\\bAPERCIBIR\\b|\\bapercibimiento\\b|\\bApercibimiento\\b|\\bApercibir\\b|\\bAPERCIBIMIENTO\\b") ~ "warning",
    str_detect(text_extract, "\\bSANCIONAR\\b|\\bIMPONER\\b|\\bImponer\\b|\\bmulta de\\b|\\binfracción de\\b|\\binfringido el artículo\\b|\\bha infringido\\b") ~ "sanction",
    str_detect(text_extract, "\\bDECLARAR la terminación\\b") ~ "termination",
    str_detect(text_extract, "\\bEXONERAR\\b") ~ "exoneration",
    TRUE ~ "other"
  )) 

# Extract entity names, article numbers, and fines
df4 <- df3 %>%
  mutate(name = str_extract(text_extract, "(?<=a la entidad).*?(?= con )")) %>%
  mutate(name = ifelse(is.na(name), str_extract(text_extract, "(?<=a la entidad).*?(?=por)"), name)) %>%
  mutate(name = ifelse(is.na(name), str_extract(text_extract, "(?<= a ).*?(?= con )"), name)) %>% 
  mutate(articulo_num = str_extract(text_extract, "(?<=artículo\\s)\\d+(\\.\\d+)*[a-z]*")) %>% 
  mutate(articulo_num = ifelse(is.na(articulo_num), str_extract(text_extract, "(?<=Artículo\\s)\\d+(\\.\\d+)*[a-z]*"), articulo_num)) %>% 
  mutate(multa = str_extract(text_extract, "(?<=una multa\\s)\\d{1,3}(\\.\\d{3})*(\\.\\d+)?")) %>% 
  mutate(multa = ifelse(is.na(multa), str_extract(text_extract, "(?<=multa de\\s)\\d{1,3}(\\.\\d{3})*(\\.\\d+)?"), multa)) %>% 
  mutate(multa = ifelse(is.na(multa), str_extract(text_extract, "(?<=sanción de\\s)\\d{1,3}(\\.\\d{3})*(\\.\\d+)?"), multa)) %>% 
  mutate(LeyApl = case_when(
    str_detect(text_extract, "RGPD") ~ "RGPD",
    str_detect(text_extract, "LSSI") ~ "LSSI",
    str_detect(text_extract, "LOPD") ~ "LOPD",
    TRUE ~ NA_character_
  ))

# Function to check for the specified words and return the found word
find_provider <- function(text) {
  words <- c("telefónica", "telefonica", "vodafone", "orange", "masmovil", "mas movil")
  for (word in words) {
    if (grepl(tolower(word), tolower(text))) {
      return(word)
    }
  }
  return(NA)
}

# Apply the function only to rows where the "name" column is NA
df4 <- df4 %>% 
  mutate(name = ifelse(is.na(name), sapply(text, find_provider), name))


# Filter for telecommunication companies
telecos <- df4 %>%
  filter(str_detect(str_to_lower(name), "telefónica|telefonica|vodafone|orange|masmovil|mas movil")) %>%
  mutate(teleco = if_else(str_detect(str_to_lower(name), "telefónica|telefonica|vodafone|orange|masmovil|mas movil"),
                          str_extract(str_to_lower(name), "telefónica|telefonica|vodafone|orange|masmovil|mas movil"),
                          NA_character_)) %>% select(dates, procedure_nums, teleco, decision, articulo_num, LeyApl, multa, text_extract) %>% 
  distinct(procedure_nums, .keep_all = TRUE) %>%
  mutate(dates = as.Date(dates))

# Standardize telecommunication company names
telecos$teleco <- str_replace_all(telecos$teleco, c("telefonica" = "telefónica"))
telecos$teleco <- str_replace_all(telecos$teleco, c("mas movil" = "masmovil"))


#Plot1

# Grouping the data by decision and counting the number of occurrences
decision_counts <- df4 %>%
  group_by(decision) %>%
  summarise(decision_count = n())

# Creating the plot
ggplot(decision_counts, aes(x = decision, y = decision_count, fill = decision)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = decision_count), vjust = -0.5, color = "black", size = 4) +  # Add text labels
  labs(x = "Decision", y = "Number of Resolutions", title = "Number of resolution by decisions") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Plot2

df4$year <- year(df4$dates)
df4$trimester <- quarter(df4$dates)

# Grouping the data by year, trimester, and counting the number of resolutions
resolution_counts <- df4 %>%
  group_by(year, trimester) %>%
  summarise(resolution_count = n())

# Creating a new column to define the order of trimesters within each year
resolution_counts$trimester_order <- factor(
  paste0("T", resolution_counts$trimester, " ", resolution_counts$year),
  levels = unique(paste0("T", resolution_counts$trimester, " ", resolution_counts$year))
)

# Creating the plot
ggplot(resolution_counts, aes(x = trimester_order, y = resolution_count)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(aes(label = resolution_count), vjust = -0.5, color = "black", size = 3) +
  labs(x = "Trimester", y = "Number of Resolutions", title = "Number of Resolutions by Trimester and Year") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = FALSE)

#plot3


# Creating a new column based on text_extract
telecos <- df4 %>%
  mutate(teleco = if_else(str_detect(str_to_lower(name), "telefónica|telefonica|vodafone|orange|masmovil|mas movil"),
                          "telecomunication",
                          "other")) %>%
  select(dates, procedure_nums, teleco, decision, articulo_num, LeyApl, multa) %>%
  mutate(dates = as.Date(dates))%>%
  mutate(teleco = ifelse(is.na(teleco), "other", teleco))


# Calculating the count for each category
sanction_counts <- telecos %>%
  group_by(teleco) %>%
  summarise(count = n())

# Creating the plot
ggplot(sanction_counts, aes(x = teleco, y = count, fill = teleco)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5, color = "black") +
  labs(x = "Resolutions", y = "Count", title = "Resolutions: Telecomunication vs Other") +
  scale_fill_manual(values = c("telecomunication" = "steelblue", "other" = "darkorange")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill = FALSE)

#plot 4

# Filter the data for telecommunication decisions
teleco_decisions <- telecos %>%
  filter(teleco == "telecomunication")

# Count the types of decisions for telecommunication
decision_counts <- teleco_decisions %>%
  group_by(decision) %>%
  summarise(count = n())

# Creating the plot
ggplot(decision_counts, aes(x = decision, y = count, fill = decision)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5, color = "black") +
  labs(x = "Decision Type", y = "Count", title = "Telecommunication Decisions by Type") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E99", "#F0E442", "#009E00", "#9252E0")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill = FALSE)


#plot 5

# Group dates by years
sanction_counts <- telecos %>%
  group_by(year = year(dates), decision) %>%
  summarise(count = n())

# Creating the plot
ggplot(sanction_counts, aes(x = year, y = count, color = decision)) +
  geom_line(size = 1.5) +
  labs(x = "Year", y = "Count", title = "Types of Sanctions Over Time") +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#FF00FF", "#F0E442", "#009E00", "#9252E0")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x = element_line()) +
  scale_x_continuous(breaks = seq(2000, 2023, 1))


# Plot 6
telecos <- telecos %>% filter(teleco == "telecomunication" & decision == "sanction") %>% 
  mutate(art = paste0(sub("\\..*", "", as.character(articulo_num)), "-", LeyApl))

telecos <- na.omit(telecos)

telecos$multa <- as.numeric(gsub("\\.", "", telecos$multa))

sanctions_count <- telecos %>%
  group_by(art) %>%
  summarise(count = n())

# Calculate the count of sanctions per art
sanctions_count <- sanctions_count %>%
  mutate(order = case_when(
    str_detect(art, "LOPD") ~ 3,
    str_detect(art, "RGPD") ~ 2,
    str_detect(art, "LSSI") ~ 1
  )) %>%
  mutate(number = as.numeric(str_extract(art, "\\d+"))) %>%
  arrange(order, number)

# Create the bar plot
ggplot(sanctions_count, aes(x = count, y = reorder(art, order))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Number of Sanctions", y = "Art") +
  ggtitle("Number of Sanctions per Art") +
  theme_minimal()

#plot 7

# Filter the dataset for the desired categories
filtered_data <- telecos %>%
  filter(art %in% c("6-LOPD", "4-LOPD", "11-LOPD", "21-LSSI"))

# Group dates by years and art
sanction_counts <- filtered_data %>%
  mutate(year = year(dates)) %>%
  group_by(year, art) %>%
  summarise(count = n())

# Creating the plot
ggplot(sanction_counts, aes(x = year, y = count, color = art)) +
  geom_line(size = 1.5) +
  labs(x = "Year", y = "Count", title = "Sanctions Evolution by Category") +
  scale_color_manual(values = c("6-LOPD" = "#E69F00", "4-LOPD" = "#56B4E9", "11-LOPD" = "#009E73", "21-LSSI" ="yellow")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x = element_line()) +
  scale_x_continuous(breaks = seq(2000, 2023, 1))

