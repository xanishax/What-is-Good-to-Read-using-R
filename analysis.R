library(tidyverse)

load("examdata.RData")

colnames(booksSciFi)
colnames(booksRomance)

dim(booksSciFi)
dim(booksRomance)

# Most popular, least popular and average number of ratings
maxRating <- max(booksSciFi$NumberOfRating)
maxRatingIndex <- which.max(booksSciFi$NumberOfRating)

minRating <- min(booksSciFi$NumberOfRating)
minRatingIndex <- which.min(booksSciFi$NumberOfRating)

cat("Most popular Science Fiction book is", booksSciFi$Title[maxRatingIndex], "by", booksSciFi$Author[maxRatingIndex], "with", maxRating, "ratings.")
cat("Least popular Science Fiction book is", booksSciFi$Title[minRatingIndex], "by", booksSciFi$Author[minRatingIndex], "with", minRating, "ratings.")

maxRating <- max(booksRomance$NumberOfRating)
maxRatingIndex <- which.max(booksRomance$NumberOfRating)

minRating <- min(booksRomance$NumberOfRating)
minRatingIndex <- which.min(booksRomance$NumberOfRating)

cat("Most popular Romance book is", booksRomance$Title[maxRatingIndex], "by", booksRomance$Author[maxRatingIndex], "with", maxRating, "ratings.")
cat("Least popular Romance book is", booksRomance$Title[minRatingIndex], "by", booksRomance$Author[minRatingIndex], "with", minRating, "ratings.")

cat("Average number of ratings for Science Fiction books are", mean(booksSciFi$NumberOfRating))
cat("Average number of ratings for Romance books are", mean(booksRomance$NumberOfRating))


# Most and least number of pages
maxPages <- max(booksSciFi$Pages)
maxPagesIndex <- which.max(booksSciFi$Pages)

minPages <- min(booksSciFi$Pages)
minPagesIndex <- which.min(booksSciFi$Pages)

cat("Longest Science Fiction book is", booksSciFi$Title[maxPagesIndex], "by", booksSciFi$Author[maxPagesIndex], "with", maxPages, "pages.")
cat("Shortest Science Fiction book is", booksSciFi$Title[minPagesIndex], "by", booksSciFi$Author[minPagesIndex], "with", minPages, "pages.")

maxPages <- max(booksRomance$Pages)
maxPagesIndex <- which.max(booksRomance$Pages)

minPages <- min(booksRomance$Pages)
minPagesIndex <- which.min(booksRomance$Pages)

cat("Longest Romance book is", booksRomance$Title[maxPagesIndex], "by", booksRomance$Author[maxPagesIndex], "with", maxPages, "pages.")
cat("Shortest Romance book is", booksRomance$Title[minPagesIndex], "by", booksRomance$Author[minPagesIndex], "with", minPages, "pages.")

cat("Average number of pages for Science Fiction books is", mean(booksSciFi$Pages))
cat("Average number of pages for Romance books is", mean(booksRomance$Pages))

# Oldest and latest publication
maxYear <- max(booksSciFi$PublishingYear)
maxYearIndex <- which.max(booksSciFi$PublishingYear)

minYear <- min(booksSciFi$PublishingYear)
minYearIndex <- which.min(booksSciFi$PublishingYear)

cat("Latest Science Fiction book is", booksSciFi$Title[maxYearIndex], "by", booksSciFi$Author[maxYearIndex], "published in", maxYear, ".")
cat("Oldest Science Fiction book is", booksSciFi$Title[minYearIndex], "by", booksSciFi$Author[minYearIndex], "published in", minYear, ".")

maxYear <- max(booksRomance$PublishingYear)
maxYearIndex <- which.max(booksRomance$PublishingYear)

minYear <- min(booksRomance$PublishingYear)
minYearIndex <- which.min(booksRomance$PublishingYear)

cat("Latest Romance book is", booksRomance$Title[maxYearIndex], "by", booksRomance$Author[maxYearIndex], "published in", maxYear, ".")
cat("Oldest Romance book is", booksRomance$Title[minYearIndex], "by", booksRomance$Author[minYearIndex], "published in", minYear, ".")


# Set up the plot area
par(mfrow = c(1, 2))  # Divide the plot area into 1 row and 2 columns

# Plot histogram for Science Fiction books
hist(booksSciFi$Pages, col = "skyblue", xlab = "Number of Pages", ylab = "Frequency",
     main = "Science Fiction Book Lengths", breaks = 30)

# Plot histogram for Romance books
hist(booksRomance$Pages, col = "pink", xlab = "Number of Pages", ylab = "Frequency",
     main = "Romance Book Lengths", breaks = 30)


par(mfrow = c(1, 2))  # Divide the plot area into 1 row and 2 columns

# Plot scatterplot for Science Fiction books
plot(booksSciFi$Pages, booksSciFi$NumberOfRating / 1e5, pch = 16, col = "skyblue",
     xlab = "Number of Pages", ylab = "Number of Ratings",
     main = "Science Fiction Book Length and Ratings")

# Plot scatterplot for Romance books
plot(booksRomance$Pages, booksRomance$NumberOfRating / 1e5, pch = 16, col = "pink",
     xlab = "Number of Pages", ylab = "Number of Ratings",
     main = "Romance Book Length and Ratings")

# Group by decade and genre, then summarize to get total number of ratings
ratings_summary <- allBooks %>%
  group_by(Decade, Genre) %>%
  summarise(TotalRatings = sum(NumberOfRating))

# Total Number of Ratings by Decade and Genre Per 100K
ratings_summary$TotalRatings_scaled <- ratings_summary$TotalRatings / 1e5

# Plotting
ggplot(ratings_summary, aes(x = as.factor(Decade), y = TotalRatings_scaled, fill = Genre)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Number of Ratings by Decade and Genre Per 100K", x = "Decade", y = "Total Number of Ratings") +
  scale_fill_manual(values = c("Science Fiction" = "blue", "Romance" = "red"), name = "Genre") +
  theme_classic() +
  theme(axis.text = element_text(size = 10),  # Adjust the size of axis text
        axis.title = element_text(size = 12),  # Adjust the size of axis titles
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold", margin = margin(b = 20, t=10)))  # Adjust the size of the plot title


save(booksSciFi, booksRomance, allBooks, ratings_summary, file = "examdata.RData")

