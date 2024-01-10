library(tidyverse)
library(shiny)
library(DT)

load("examdata.RData")

ui <- navbarPage(
  title = div(
    tags$img(src = "https://s.gr-assets.com/assets/home/header_logo-8d96d7078a3d63f9f31d92282fd67cf4.png",
             height = 30)
  ),
  
  tabPanel("Compare Book Lengths",
           tags$style(HTML("
              body {
                margin-bottom: 40px;
                color: #382110;
              }
              .navbar-default {
                background-color: #F4F1EA;
              }
            ")),
           
        # Check box, Max pages book details, Min pages book details   
        fluidRow(
            column(4,
                checkboxGroupInput("checkGroup", label = h3("Select Genre"),
                choices = list("Science Fiction" = "sciFi", "Romance" = "romance"),
                selected = c("sciFi", "romance"))),
            column(4, 
                wellPanel(
                    h4("Book with Minimum Pages"),
                    textOutput("minPageBook"))),
            column(4, 
                wellPanel(
                    h4("Book with Maximum Pages"),
                    textOutput("maxPageBook")))),
        
        # Histograms for SciFi and Romance books
        conditionalPanel(
            condition = "input.checkGroup.length == 2",
                fluidRow(
                    column(6, plotOutput("sciFiHistogram")),
                    column(6, plotOutput("romanceHistogram"))
                ),
                dataTableOutput("singleDataTable")),

        conditionalPanel(
            condition = "input.checkGroup.length == 1",
            plotOutput("singleHistogram", width = "100%")),
        
        conditionalPanel(
            condition = "input.checkGroup.length == 1 && input.checkGroup.includes('sciFi')",
            dataTableOutput("sciFiDataTable")),

        conditionalPanel(
            condition = "input.checkGroup.length == 1 && input.checkGroup.includes('romance')",
            dataTableOutput("romanceDataTable"))
  ),
  
  tabPanel("Compare Popular Books, Every Decade",
           
        # Drop down, pie chart, bar plot and data table
        fluidRow(
            column(5,
                wellPanel(
                    selectInput("selectDecade", label = h3("Select Decade"), 
                        choices = c("All", "1810-1819", "1820-1829", "1830-1839", "1840-1849", 
                                    "1850-1859", "1860-1869", "1870-1879", "1880-1889", 
                                    "1890-1899", "1900-1909", "1910-1919", "1920-1929", 
                                    "1930-1939", "1940-1949", "1950-1959", "1960-1969", 
                                    "1970-1979", "1980-1989", "1990-1999", "2000-2009", 
                                    "2010-2019", "2020-2029"), selected = "All", multiple = TRUE),
                    plotOutput("pieChart", width = "100%"))),
             column(7,
                 wellPanel(
                    plotOutput("barplot", width = "100%", height = 515)))),
        dataTableOutput("decadeDataTable")
  ),
  
  tabPanel("Popular or Long Books?",
           
        # Check box, Max ratings book details, Min ratings book details 
        fluidRow(
            column(4,
                checkboxGroupInput("popularityCheckGroup", label = h3("Select Genre"),
                    choices = list("Science Fiction" = "sciFi", "Romance" = "romance"),
                    selected = c("sciFi", "romance"))),
            column(4, 
               wellPanel(
                 h4("Book with Minimum Ratings"),
                 textOutput("minBookRating"))),
            column(4, 
               wellPanel(
                 h4("Book with Maximum Ratings"),
                 textOutput("maxBookRating")))),
        
        # Scatter plots
        conditionalPanel(
            condition = "input.popularityCheckGroup.length == 2",
            fluidRow(
                column(6, 
                       plotOutput("sciFiScatterPlot")),
                column(6, 
                       plotOutput("romanceScatterPlot"))),
            dataTableOutput("singleDataTablePop")),
           
        conditionalPanel(
            condition = "input.popularityCheckGroup.length == 1",
                plotOutput("singleScatterPlot", width = "100%")),
        
        conditionalPanel(
            condition = "input.popularityCheckGroup.length == 1 && input.popularityCheckGroup.includes('sciFi')",
                dataTableOutput("sciFiDataTablePop")),
           
        conditionalPanel(
            condition = "input.popularityCheckGroup.length == 1 && input.popularityCheckGroup.includes('romance')",
                dataTableOutput("romanceDataTablePop")))
  )

# Server
server <- function(input, output) {
  
  # For Compare Book Lengths
  output$sciFiHistogram <- renderPlot({
    if ("sciFi" %in% input$checkGroup) {
      hist(booksSciFi$Pages, col = "skyblue", xlab = "Number of Pages", ylab = "Frequency",
           main = "Science Fiction Book Lengths", breaks = 30)
    }
  })

  output$romanceHistogram <- renderPlot({
    if ("romance" %in% input$checkGroup) {
      hist(booksRomance$Pages, col = "pink", xlab = "Number of Pages", ylab = "Frequency",
           main = "Romance Book Lengths", breaks = 30)
    }
  })

  output$singleHistogram <- renderPlot({
    if ("sciFi" %in% input$checkGroup) {
      hist(booksSciFi$Pages, col = "skyblue", xlab = "Number of Pages", ylab = "Frequency",
           main = "Science Fiction Book Lengths", breaks = 30)
    } else if ("romance" %in% input$checkGroup) {
      hist(booksRomance$Pages, col = "pink", xlab = "Number of Pages", ylab = "Frequency",
           main = "Romance Book Lengths", breaks = 30)
    }
  })

  output$sciFiDataTable <- renderDataTable({
    if ("sciFi" %in% input$checkGroup || "sciFi" %in% input$popularityCheckGroup) {
      datatable(booksSciFi[, c("Title", "Pages")], options = list(order = list(2, 'asc')))
    }
  })

  output$romanceDataTable <- renderDataTable({
    if ("romance" %in% input$checkGroup || "romance" %in% input$popularityCheckGroup) {
      datatable(booksRomance[, c("Title", "Pages")], options = list(order = list(2, 'asc')))
    }
  })

  output$singleDataTable <- renderDataTable({
    if (("sciFi" %in% input$checkGroup && "romance" %in% input$checkGroup) ||
        ("sciFi" %in% input$popularityCheckGroup && "romance" %in% input$popularityCheckGroup)) {
      datatable(
        allBooks[, c("Title", "Genre", "Pages")],
        options = list(order = list(3, 'asc'))
      )
    }
  })
  
  # For Compare Popular Books, Every Decade
  output$maxPageBook <- renderText({
    max_page_row <- allBooks[which.max(allBooks$Pages), ]
    max_page_title <- max_page_row$Title
    max_page_genre <- max_page_row$Genre
    max_page_pages <- max_page_row$Pages
    paste(max_page_title,"-",max_page_genre,"-",max_page_pages,"pages")
  })
  
  output$minPageBook <- renderText({
    min_page_row <- allBooks[which.min(allBooks$Pages), ]
    min_page_title <- min_page_row$Title
    min_page_genre <- min_page_row$Genre
    min_page_pages <- min_page_row$Pages
    paste(min_page_title,"-",min_page_genre,"-",min_page_pages,"pages")
  })
  
  output$maxBookRating <- renderText({
    max_rating_row <- allBooks[which.max(allBooks$NumberOfRating), ]
    max_rating_title <- max_rating_row$Title
    max_rating_genre <- max_rating_row$Genre
    paste(max_rating_title,"-",max_rating_genre)
  })
  
  output$minBookRating <- renderText({
    min_rating_row <- allBooks[which.min(allBooks$NumberOfRating), ]
    min_rating_title <- min_rating_row$Title
    min_rating_genre <- min_rating_row$Genre
    paste(min_rating_title,"-",min_rating_genre)
  })
  
  output$sciFiScatterPlot <- renderPlot({
    if ("sciFi" %in% input$popularityCheckGroup) {
      plot(booksSciFi$Pages, booksSciFi$NumberOfRating / 1e5, pch = 16, col = "skyblue",
           xlab = "Number of Pages", ylab = "Number of Ratings",
           main = "Science Fiction Book Length and Ratings (Per 100K)")
      }
  })

  output$romanceScatterPlot <- renderPlot({
    if ("romance" %in% input$popularityCheckGroup) {
      plot(booksRomance$Pages, booksRomance$NumberOfRating / 1e5, pch = 16, col = "pink",
           xlab = "Number of Pages", ylab = "Number of Ratings",
           main = "Romance Book Length and Ratings (Per 100K)")
    }
  })

  output$singleScatterPlot <- renderPlot({
    if ("sciFi" %in% input$popularityCheckGroup) {
      plot(booksSciFi$Pages, booksSciFi$NumberOfRating / 1e5, pch = 16, col = "skyblue",
           xlab = "Number of Pages", ylab = "Number of Ratings",
           main = "Science Fiction Book Length and Ratings (Per 100K)")
    } else if ("romance" %in% input$popularityCheckGroup) {
      plot(booksRomance$Pages, booksRomance$NumberOfRating / 1e5, pch = 16, col = "pink",
           xlab = "Number of Pages", ylab = "Number of Ratings",
           main = "Romance Book Length and Ratings (Per 100K)")
    }
  })
  
  
  
  output$sciFiDataTablePop <- renderDataTable({
    if ("sciFi" %in% input$checkGroup || "sciFi" %in% input$popularityCheckGroup) {
      datatable(booksSciFi[, c("Title", "NumberOfRating", "Pages")], options = list(order = list(2, 'asc')))
    }
  })
  
  output$romanceDataTablePop <- renderDataTable({
    if ("romance" %in% input$checkGroup || "romance" %in% input$popularityCheckGroup) {
      datatable(booksRomance[, c("Title", "NumberOfRating", "Pages")], options = list(order = list(2, 'asc')))
    }
  })
  
  output$singleDataTablePop <- renderDataTable({
    if (("sciFi" %in% input$checkGroup && "romance" %in% input$checkGroup) ||
        ("sciFi" %in% input$popularityCheckGroup && "romance" %in% input$popularityCheckGroup)) {
      datatable(
        allBooks[, c("Title", "Genre", "NumberOfRating", "Pages")],
        options = list(order = list(3, 'asc'))
      )
    }
  })
  
  # For Popular or Long Books
  output$pieChart <- renderPlot({
    
    # Extract the selected decades from input
    selected_decades <- input$selectDecade
    
    if ("All" %in% selected_decades) {
      ratings_sum <- ratings_summary %>%
        group_by(Genre) %>%
        summarise(TotalRatings = sum(TotalRatings))
    } else {
      
      # Convert the selected decades to start and end years
      selected_years <- lapply(strsplit(selected_decades, "-"), as.numeric)
      start_years <- sapply(selected_years, `[`, 1)
      end_years <- sapply(selected_years, `[`, 2)
      
      # Initialize an empty data frame to store combined decade data
      ratings_decade <- data.frame()
      
      # Loop through each selected decade to filter and combine data
      for (i in seq_along(start_years)) {
        decade_data <- ratings_summary[ratings_summary$Decade >= start_years[i] & ratings_summary$Decade <= end_years[i], ]
        ratings_decade <- rbind(ratings_decade, decade_data)
      }
      
      # Summing up TotalRatings for each Genre across selected decades
      ratings_sum <- ratings_decade %>%
        group_by(Genre) %>%
        summarise(TotalRatings = sum(TotalRatings))
    }
    
    total_ratings_sf <- sum(ratings_sum$TotalRatings[ratings_sum$Genre == "Science Fiction"])
    total_ratings_romance <- sum(ratings_sum$TotalRatings[ratings_sum$Genre == "Romance"])
    
    # Calculate percentages for Science Fiction and Romance
    percentage_sf <- round((total_ratings_sf / sum(ratings_sum$TotalRatings)) * 100, 2)
    percentage_romance <- round((total_ratings_romance / sum(ratings_sum$TotalRatings)) * 100, 2)
    
    # Prepare labels for SF and Romance without Genre names
    labels <- c(paste(percentage_romance, "%"), paste(percentage_sf, "%"))
    
    # Plotting the pie chart
    ggplot(ratings_sum, aes(x = "", y = TotalRatings, fill = Genre)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      geom_text(aes(label = ifelse(Genre %in% c("Romance", "Science Fiction"), labels, "")), 
                position = position_stack(vjust = 0.5), size = 5) +
      coord_polar("y", start = 0) +
      labs(title = "Proportion of popularity by Decade",
           fill = "Genre",
           y = "Total Ratings (in 10^7)") +
      theme_void() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 12),
            plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 20, t = 10))) +
      scale_fill_manual(values = c("Science Fiction" = "skyblue", "Romance" = "pink"))
  })

  
  output$barplot <- renderPlot({
    ratings_summary$TotalRatings_scaled <- ratings_summary$TotalRatings / 1e5
    
    # Plotting
    ggplot(ratings_summary, aes(x = as.factor(Decade), y = TotalRatings_scaled, fill = Genre)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "\nTotal Number of Ratings by Decade and Genre Per 100K", x = "Decade", y = "Total Number of Ratings") +
      scale_fill_manual(values = c("Science Fiction" = "skyblue", "Romance" = "pink"), name = "Genre") +
      theme_classic() +
      theme(axis.text = element_text(size = 10),  # Adjust the size of axis text
            axis.title = element_text(size = 12),  # Adjust the size of axis titles
            plot.title = element_text(size = 14, hjust = 0.5, face = "bold", margin = margin(b = 10, t=30)))
  })
  
  output$decadeDataTable <- renderDataTable({
    selected_decades <- input$selectDecade
    
    if ("All" %in% selected_decades) {
      datatable(allBooks, options = list(order = list(6, 'asc')))
    } 
    else {
      decade_data <- data.frame()
      # Loop through each selected decade to filter and combine data
      for (i in seq_along(selected_decades)) {
        selected_years <- as.numeric(strsplit(selected_decades[i], "-")[[1]])
        start_year <- selected_years[1]
        end_year <- selected_years[2]
        
        # Filter the data for the selected range of years and rbind to decade_data
        filtered_data <- allBooks[allBooks$Decade >= start_year & allBooks$Decade <= end_year, ]
        decade_data <- rbind(decade_data, filtered_data)
      }
      
      datatable(decade_data, options = list(order = list(6, 'asc')))
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)






