# Code for Scraping and Analyzing AEPD Resolutions

This repository contains R code for scraping and analyzing resolutions from the Spanish Data Protection Agency (AEPD) website. The code collects information about the resolutions, including links, dates, procedure numbers, and decision texts. It also classifies the decisions based on specific words and extracts entity names, article numbers, and fines.

## Prerequisites
Before running the code, make sure you have the following libraries installed:

- rvest
- pdftools
- tidyverse
- xml2
- lubridate
- stringr

You can install these libraries using the following command:

```R
install.packages(c("rvest", "pdftools", "tidyverse", "xml2", "lubridate", "stringr"))
```

## Code Overview
The code performs the following steps:

1. Define the base URL and the number of pages to scrape.
2. Initialize empty vectors to store links, dates, and procedure numbers.
3. Loop over the pages and extract links, dates, and procedure numbers.
4. Pause for a short time between requests to avoid overwhelming the server.
5. Create a data frame with the collected information.
6. Download and extract the text from the PDFs linked in the data frame.
7. Clean and preprocess the text data.
8. Extract relevant text sections from the data.
9. Classify the decisions based on the presence of specific words.
10. Extract entity names, article numbers, and fines.
11. Save the results as a CSV file.
12. Statistical analysis using ggplot2.

## Usage
To use this code, follow these steps:

1. Install the required libraries if you haven't already.
2. Download the code into an R script or an R Markdown document.
3. Modify the `base_url` and `num_pages` variables if needed.
4. Run the code in R to scrape the AEPD resolutions and perform the analysis.
5. The final results will be saved as a CSV file named `data_base_extract_final.csv`.

Note: Make sure you have an internet connection to access the AEPD website and download the PDF files.
Note 2: Additionally, the dataset is available on Google Drive in two versions: the first version contains the raw, unmodified dataset, from the Scraper activity,  while the second version includes all the necessary cleaning and modifications that were performed to extract key insights. https://drive.google.com/drive/folders/1qF1YhnOF1L9AATlVdILnKSsn0bZxO-oY?usp=sharing

