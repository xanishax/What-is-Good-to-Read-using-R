# What is Good to Read? using R
We will investigate reading recommendations from GoodReads.com. 
**Downloading data**
We will use rvest package to scrape data from https://www.goodreads.com/ .
We will focus on the most rated/popular books from two particular genres – Science Fiction and Romance, so your starting points for web scraping will be https://www.goodreads.com/shelf/show/science-fiction and https://www.goodreads.com/shelf/show/romance respectively. 

**Data analysis**
Study the distribution of the length of a book (in pages). Is it the same for Science Fiction and Romance books? Is there a relationship between the length of the book and its popularity (number of ratings)? Study the year of first publication. Combine years in decades and count number of books in each decade. What time period generated more/less popular books in each genre? 

**Interactive dashboard**
We create a dashboard using package shiny as an alternative delivery to the written report. The dashboard should cover the same topics as a report. Take care about a good design for the dashboard. One graph per screen is (probably) a bad idea (unless your graph is very detailed, and you need a lot of space for it). All graphs on one page could be a bad idea too (unless your graphs are very simple).


1.	R-script file download.R with all code used to scrape data from the internet, clean it and store on the hard drive ready for the analysis.
2.	R data file – examdata.RData or examdata.rds – where you store all information prepared by download.R script. If you have multiple variables to store, then you can use function save() or you can put all your variables in one list and use function saveRDS(). 
3.	R-script analysis.R with all code used to load and analyse the data file and to output all statistics and data visualisations for the written report.
4.	MSWord or PDF file with a written report. It should be well-organised and well-prepared report with introduction, conclusion, and all required discussions. Good report should have a title, headings and sub-heading, page numbers, titles for figures and tables. 
Report should NOT include R-code or R-outputs but results of your analysis. Don’t include R-code in the report, I can see your code in the submitted R-script. 
5.	R-script app.R with the code of your interactive dashboard. The dashboard will include mostly the same visualisations as your report, as the dashboard is an enhanced copy of the report but without discussions. However, data visualisations should be interactive, e.g., add/remove/change some parameters – number of bedrooms or car spaces. Table is a type of data visualisation too, so it can be included in the dashboard. Like the report, the dashboard should have appropriate titles and headings – it should be easy for the reader to understand what is what on the dashboard.


