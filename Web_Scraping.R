# Web Scraping using R: Extracting and Analyzing Journal Article Data
# Project Title : Molecular Brain

# Task 1: install packages for web scraping

install.packages("rvest")
library(rvest)


# -----------------*************----------------------

# Task 2: Scraping Article Data : Title, Authors, Correspondence Author, Correspondence Author's Email, Publish Date,
# Abstract, Keywords.

# The URL for the Molecular Brain journals
base_url <- "https://molecularbrain.biomedcentral.com" # home page

# list of journals with pagination based on input year.
base_url_articles <-"https://molecularbrain.biomedcentral.com/articles" 

# list of journals with pagination based on input year.
base_url_articles_year <- "https://molecularbrain.biomedcentral.com/articles?query=&searchType=&tab=keyword&volume=1"

# line of code reads the HTML content of a webpage from a specified URL 
# and stores it in the variable document
document <- read_html(base_url_articles)

# Extract volume numbers
volume_numbers <- document %>%
  html_nodes("#volume-from > option") %>%
  html_text() %>%
  gsub("Volume (\\d+) \\(\\d+\\)", "\\1", .) %>%
  as.numeric() %>% na.omit()

# Extract volume years
volume_years <- document %>%
  html_nodes("#volume-from > option") %>%
  html_text() %>%
  gsub("Volume \\d+ \\((\\d+)\\)", "\\1", .) %>%
  as.numeric() %>% na.omit()

volume_df <- data.frame(
  volume_numbers = volume_numbers,
  volume_years = volume_years
)
volume_df <- volume_df[order(volume_df$volume_numbers), ]
# Print the data frame
print(volume_df)


# Function to scrape list of journals for molecular brain topic

scrape_page <- function(url) {
  webpage <- read_html(url)
  # Extract article information using CSS selectors
  title <- webpage %>% html_nodes(".c-listing__title") %>% html_text()
  authors <- webpage %>% html_nodes(".c-listing__authors") %>% html_text()
  link <- webpage %>% html_nodes(xpath = '//*/a[@data-test="title-link"]') %>% html_attr("href")

  # Create a data frame to store the scraped data
  article_data <- data.frame(
    Title = title,
    Authors = authors,
    link = link
  )
}


# logic is for the the web-scraping to fetch the all the journal in molecular brain(can be any topic) 
# we need to use the below implementation

page_link <- "/articles?searchType=journalSearch&page=1"
page_link
all_data <- data.frame()
while(1){
  url <- paste0(base_url, page_link)
  # page_data <- scrape_page(url, year)
  page_data <- scrape_page(url)
  all_data <- rbind(all_data, page_data)
  webpage <- read_html(url)
  page_link <- webpage %>% html_node("li.c-pagination__item a[data-test='next-page']") %>% html_attr("href")
  next_page_disabled <- webpage %>% html_nodes(xpath = '//*[@class="c-pagination__item"]/span[@data-test="next-page-disabled"]')
  if(length(next_page_disabled) == 1){
    break
  }
}


# -----------------*************----------------------

# logic is for the the web-scraping to fetch the all the journal based on input year in molecular brain(can be any topic)
# we need to use the below implementation

# Prompt the user to enter an year
# user_year_input <- as.integer(readline("Please enter an input year for journals: "))

# Check if the entered volume number exists in volume_df
#if (user_year_input %in% volume_df$volume_years) {
  # Get the corresponding year
#  volume_number <- volume_df[volume_df$volume_years == user_year_input, "volume_numbers"]
#  cat("Volume", volume_number, "exists in year", user_year_input, "\n")
#  page_link_year <- paste("/articles?tab=keyword&searchType=journalSearch&sort=PubDate&volume=",volume_number,"&page=1")
#  page_link_year <- gsub(" ", "", page_link_year)
# } else {
#  cat("Invalid year is specified",user_year_input,"please pick a valid year.\n")
#  page_link_year <- ""
# }


# page_link_year <- "/articles?tab=keyword&searchType=journalSearch&sort=PubDate&volume=12&page=1"
# page_link_year
# all_data_year <- data.frame()
# if(page_link_year != ""){
#  while(1){
#    url <- paste0(base_url, page_link_year)
#    # page_data <- scrape_page(url, year)
#    page_data <- scrape_page(url)
#    all_data_year <- rbind(all_data_year, page_data)
#    webpage <- read_html(url)
#    page_link_year <- webpage %>% html_node("li.c-pagination__item a[data-test='next-page']") %>% html_attr("href")
#    next_page_disabled <- webpage %>% html_nodes(xpath = '//*[@class="c-pagination__item"]/span[@data-test="next-page-disabled"]')
#    if(length(next_page_disabled) == 1){
#      break
#    }
#  }  
#}

# printing first 5 rows of the data
# head(all_data_year)

# all_data <- all_data_year

# -----------------*************----------------------

# Task 3: Data Cleaning and Preprocessing
# After scraping the data, clean and preprocess it to remove any irrelevant information,
# handle missing values, and format the data for analysis
# Data pre-processing data for title , authors and url of journal for list of journals
# Function to clean author strings and list of extract individual author names
clean_authors <- function(author_string) {
  # Remove leading and trailing whitespaces
  author_string <- trimws(author_string)
  
  # Remove "Authors:" and "\n" from the string
  author_string <- gsub("Authors:|\\n", "", author_string)
  
  # Replacing "and:" with "," in the string
  author_string <- gsub(" and", ",", author_string)
  author_string <- trimws(author_string)
  # Split the string by comma to get individual author names
  authors <- strsplit(author_string, ", ")
  
  return(authors)
}

# Function to clean title strings and list of extract individual title names
clean_title <- function(title_string) {
  # Remove leading and trailing whitespaces
  title_string <- trimws(title_string)
  # Remove "Authors:" and "\n" from the string
  title_string <- gsub("\\n", "", title_string)
  title_string <- trimws(title_string)
  return(title_string)
}

# Function to process the link for the journal.
process_link <- function(link_string) {
  # Remove leading and trailing white-spaces
  link_string <- trimws(link_string)
  # Remove "Authors:" and "\n" from the string
  link_string <- paste(base_url, link_string)
  return(link_string)
}

# Apply the clean_authors function to each authors string
all_data$Authors <- lapply(all_data$Authors, clean_authors)

# Apply the clean_title function to each title string
all_data$Title <- lapply(all_data$Title, clean_title)
head(all_data)

# pre-process the link for the journal.
all_data$link <- paste(base_url,all_data$link, sep ="")
head(all_data)

# Stil, we need to fetch the below information from the individual journals.
# correspondence_authors 
# correspondence_emails
# publish_dates
# abstracts
# keywords

# Correspondence Author, Correspondence Author's Email, Publish Date, Abstract, Keywords.

# Function to scrape data from a single URL
extract_article_data <- function(url) {
  document <- read_html(url)
  
  # Extract abstract
  abstract_doc <- document %>% html_node(xpath = "//div[@class='c-article-section__content']/p") %>% html_text()
  abstract <- ifelse(length(abstract_doc) != 0, abstract_doc, NA)
  
  # Extract keywords
  keywords_doc <- list(document %>% html_nodes(".c-article-subject-list__subject") %>% sapply(html_text, simplify = TRUE))
  keywords <- ifelse(length(keywords_doc) != 0, keywords_doc, NA)
  keywords <- gsub("[\n[:space:]]+|[\\\\n[:space:]]+", " ", keywords)
  
  # Extract correspondence authors and their email
  correspondence_authors_doc <- list(document %>% html_nodes(xpath = "//p[@id='corresponding-author-list']/a") %>% sapply(html_text, simplify = TRUE))
  correspondence_authors_email_doc <- list(document %>% html_nodes(xpath = "//p[@id='corresponding-author-list']/a") %>% html_attr("href"))
  correspondence_authors <- ifelse(length(correspondence_authors_doc) != 0, correspondence_authors_doc, NA)
  correspondence_authors_email <- ifelse(length(correspondence_authors_doc) != 0, correspondence_authors_email_doc, NA) 
  correspondence_authors_email <- unlist(gsub("mailto:", "", correspondence_authors_email))
  correspondence_authors_email <- strsplit(gsub("[c\\(\\)]", "", correspondence_authors_email), ", ")[[1]]
  
  # Extract publish date
  publish_date_doc <- document %>% html_node(xpath = "//p[contains(text(), 'Published')]/span/time") %>% html_text()
  publish_date <- ifelse(length(publish_date_doc) != 0, publish_date_doc, NA)
  
  # Return a list containing the scraped data
  return(list(abstract = abstract, keywords = keywords, correspondence_authors = correspondence_authors,
              correspondence_authors_email = correspondence_authors_email, publish_date = publish_date))
}

# Use lapply to extract data for each article
article_data <- lapply(all_data$link, extract_article_data)

# Combine the article data into a data frame
article_df <- as.data.frame(do.call(rbind, article_data))

# Add the article data to the original data frame
all_data <- cbind(all_data, article_df)

head(all_data)
View(all_data)

# -----------------*************----------------------


# Write the data frame to a CSV file we are transforming the data as dataset contains a list in cell and normal write.csv will be throwing a error
df <- all_data

# Convert list columns to strings
df$Title <- sapply(df$Title, function(x) if(length(x) > 0) paste(x, collapse = ", ") else "")
df$Authors <- sapply(df$Authors, function(x) if(length(x) > 0) paste(x, collapse = ", ") else "")
df$link <- sapply(df$link, function(x) if(length(x) > 0) paste(x, collapse = ", ") else "")
df$abstract <- sapply(df$abstract, function(x) if(length(x) > 0) paste(x, collapse = ", ") else "")
df$keywords <- sapply(df$keywords, function(x) if(length(x) > 0) paste(x, collapse = ", ") else "")
df$correspondence_authors <- sapply(df$correspondence_authors, function(x) if(length(x) > 0) paste(x, collapse = ", ") else "")
df$correspondence_authors_email <- sapply(df$correspondence_authors_email, function(x) if(length(x) > 0) paste(x, collapse = ", ") else "")
df$publish_date <- sapply(df$publish_date, function(x) if(length(x) > 0) paste(x, collapse = ", ") else "")

# Write the data frame to a CSV file
write.csv(df, file = "molecular_brain_journal_all_years.csv", row.names = FALSE)

# -----------------*************----------------------

# Task 4: Data Analysis and Visualization

# installing the visualization package
# install.packages(ggplot2)
library(ggplot2)
library(dplyr)

# -----------------*************----------------------

# visualization 1.
# creating a bar chart that will visually display which top 10 keywords appear most
# frequently in the molecular Brain journals we have scrapped


# Function to extract values within double quotes from a string
extract_keywords <- function(keyword_string) {
  keywords <- unlist(regmatches(keyword_string, gregexpr("\"([^\"]*)\"", keyword_string)))
  return(keywords)
}

# Apply the function to each element in the "keywords" column
# Create a table of keyword frequencies
extracted_keywords <- table(unlist(lapply(all_data$keywords, extract_keywords)))
extracted_keywords

# Converting the table to a data frame
keyword_freq_df <- data.frame(keyword = names(extracted_keywords),
                              frequency = as.numeric(extracted_keywords))

# Group the data by Author
keyword_freq_df_final <- keyword_freq_df %>% group_by(keyword) %>% summarize(Total_frequency = sum(frequency))

# Sorting authors by the number of articles they've contributed to
keyword_freq_df_final <- keyword_freq_df_final[order(-keyword_freq_df_final$Total_frequency), ]
head(keyword_freq_df_final)

# write.csv(keyword_freq_df_final, file = "keywords_frequency.csv", row.names = FALSE)
# Taking top N keywords (e.g., top 10)
top_n_keywords <- keyword_freq_df_final[1:10, ]

# Creating a bar plot
ggplot(top_n_keywords, aes(x = reorder(keyword, -Total_frequency), y = Total_frequency)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Top 10 Keywords in Scraped Articles",
       x = "Keyword",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# -----------------*************----------------------


# Visualization 2.

# Counting of journals written by a author in molecular brain.

# Extract values within double quotes from the 'strings' column
author_counts <- table(unlist(strsplit(as.character(all_data$Authors), ", ")))
author_counts <- as.data.frame(author_counts, stringsAsFactors = FALSE)
colnames(author_counts) <- c("Author", "Article_Count")
author_counts$Author <- gsub("\\)\\)|list\\(c\\(|\\)|c\\(|\\)|list\\(", "", author_counts$Author)

# Group the data by Author
author_counts_final <- author_counts %>% group_by(Author) %>% summarize(Total_Articles_Count = sum(Article_Count))

# Sorting authors by the number of articles they've contributed to
author_counts_final <- author_counts_final[order(-author_counts_final$Total_Articles_Count), ]

# write.csv(author_counts_final, file = "authors_value.csv", row.names = FALSE)


# Bar Chart
bar_chart_authors <- ggplot(head(author_counts_final, 10), aes(x = reorder(Author, Total_Articles_Count), y = Total_Articles_Count)) +
  geom_bar(stat = "identity", fill = "pink") +
  coord_flip() +
  labs(x = "Author", y = "Number of Articles", title = "Top 10 Authors by Number of Articles")
print(bar_chart_authors)

# -----------------*************----------------------


# Visualization 3.

# Unlist the list of character strings
publish_dates <- unlist(all_data$publish_date)

# Convert to Date format
publish_dates <- as.Date(publish_dates, format = "%d %B %Y")

# Assign back to the dataframe
all_data$publish_date <- publish_dates

# Create a histogram of publish dates
histogram_publish_date <- ggplot(all_data, aes(x = publish_date)) +
  geom_histogram(binwidth = 30, fill = "yellow", color = "black") +  # Adjust binwidth as needed
  labs(x = "Publish Date", y = "Frequency", title = "Histogram of Publish Dates")

print(histogram_publish_date)


# -----------------*************----------------------









