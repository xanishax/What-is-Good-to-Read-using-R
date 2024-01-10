rm(list = ls())

library(rvest)
library(stringr)

urlSciFi <- "https://www.goodreads.com/shelf/show/science-fiction"
urlRomance <- "https://www.goodreads.com/shelf/show/romance"

extractBooks <- function(url, genre) {
  htmlBooks <- read_html(url)
  
  # Title of book
  htmlBooks %>% html_elements(".elementList .bookTitle") %>% html_text() -> book.title
  
  # Author of book
  htmlBooks %>% html_elements('span[itemprop = "name"]') %>%
    html_text() -> book.author
  
  # Average rating
  htmlBooks %>%
    html_elements(".elementList .greyText.smallText") %>%
    html_text() -> subText
  
  subText <- gsub("\\s+|\\n", " ", subText)
  
  # Regular expressions to extract ratings
  book.averageRating <- as.numeric(str_extract(subText, "\\d+\\.\\d+"))  # Extracts the average ratings
  book.rating <- as.numeric(gsub(",", "", str_extract(subText, "\\d{1,3}(,\\d{3})*(?= ratings)")))  # Extracts the number of ratings
  
  # Extract book links
  htmlBooks %>%
    html_elements('.elementList .bookTitle') %>% 
    html_attr("href") %>%
    paste("https://www.goodreads.com", ., sep = "") -> bookLinks
  
  totalPages <- c()
  totalYears <- c()
  
  for (link in bookLinks) {
    bookPage <- read_html(link)
    
    # Scrape total pages
    pages <- bookPage %>%
      html_elements(".BookDetails p[data-testid='pagesFormat']") %>%
      html_text()
    
    pages <- as.numeric(str_extract(pages, "\\d+"))
    totalPages <- c(totalPages, pages)
    
    # Scrape published year
    years <- bookPage %>%
      html_elements(".BookDetails p[data-testid='publicationInfo']") %>%
      html_text()
    
    years <- as.numeric(regmatches(years, regexpr("\\b\\d{4}\\b", years)))
    totalYears <- c(totalYears, years)
  }
  
  return(data.frame(Title = book.title,
                    Author = book.author,
                    Genre = genre,
                    PublishingYear = totalYears,
                    AverageRating = book.averageRating,
                    NumberOfRating = book.rating,
                    Pages = totalPages
                    
  ))
}

booksSciFi <- extractBooks(urlSciFi, "Science Fiction")
booksRomance <- extractBooks(urlRomance, "Romance")

# Calculate the decades for Science Fiction books
booksSciFi$Decade <- as.integer(floor(booksSciFi$PublishingYear / 10) * 10)

# Calculate the decades for Romance books
booksRomance$Decade <- as.integer(floor(booksRomance$PublishingYear / 10) * 10)

allBooks <- rbind(booksSciFi, booksRomance)

save(booksSciFi, booksRomance, allBooks, file = "examdata.RData")


