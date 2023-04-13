## app.R ##
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(wordcloud2)
library(wordcloud)
library(ggplot2)
library(packcircles)
library(ggplot2)
library(viridis)
library(tibble)
library(tm)
library(htmltools)
library(knitr)
library(kableExtra)
library(tidytext)
library(syuzhet)
library(plotly)
library(dplyr)
library(magick)
library(waiter)
library(reticulate)
library(lubridate)
# py_install("pandas")
# py_install("openpyxl")
# py_install("pytz")
# py_install("instaloader")
library(shinyalert)




ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "InstaAnalytica"),
                    ## Sidebar content
                    dashboardSidebar(
                      collapsed = TRUE,
                      sidebarMenu(id = "tabs",
                                  menuItem("Username", tabName = "username", icon = icon("user")),
                                  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
                      )
                    ),
                    ## Body content
                    dashboardBody(useWaitress(color = "#C13584"),
                      tags$head(    tags$style(HTML("   
     .col-sm-4 { width: 100%;}
     .col-sm-8 { width: 100%;}
    ")),
                        tags$style(HTML("
      /* Change the font color of the tabPanel */
      .nav-tabs > li > a {
        color: white;
      }
    ")),
                        tags$style("
  .shiny-box {
    overflow: auto;
    max-height: calc(70vh - 100px);
  }
"),
                        tags$style("
  .shiny-box1 {
    overflow-y: auto;
    max-height: calc(80vh - 100px);
  }
")
                      ),
                      setBackgroundImage(
                        src = "https://lotofcolor.com/wp-content/uploads/2020/11/thumbnail-3.png",
                        shinydashboard = TRUE
                      ),
                      tabItems(
                        # First tab content
                        tabItem(
                          tabName = "username",
                          div(
                            fluidRow(
                              column(12, align = "center",
                              withSpinner(plotOutput("logo")),
                              title = "Inputs", background = "black",
                              textInput("text", "Enter the Username:"),
                              actionButton("submit_name", "Analyze"),
                              useShinyalert(), # Enable shinyalert library
                            )),
                            style = "display: flex; justify-content: center; align-items: center; height: 80vh; color:white;"
                          )
                        ),
                        tabItem(tabName = "dashboard",
                                mainPanel(
                                  tabsetPanel(
                                    id = "tabset1",
                                    tabPanel(title = "Profile Trends",
                                             selectInput("chart", label = tags$span(style="color: white;","Select your chart:"),
                                                         choices = c("Area Chart", "Line Chart")),
                                             withSpinner(plotlyOutput("plot"))),
                                    tabPanel(title = "WordCloud",
                                             tags$div(
                                               id = "center-container",
                                               style = "display: flex; justify-content: center; align-items: center;",
                                               withSpinner(htmlOutput("wordcloud"))
                                             ),
                                             withSpinner(htmlOutput("wordtext"))
                                    ),
                                    tabPanel("Top Hashtags",
                                             sliderInput("seleco", label = tags$span(style="color: white;","How many Hashtags do you want to see?"), min = 1, max = 30, value = 10),
                                             withSpinner(plotlyOutput("plot2",height = "auto", width = "100%"))
                                    ),
                                    tabPanel("Top Posts",
                                             tags$style("
  .radio-inline label {
    display: inline-block;
    margin-right: 10px;
  }
"),
                                             
                                             fluidRow(
                                               column(width = 6, 
                                                      radioButtons("five", label = tags$span(style="color: white;","Choose your sort:"),
                                                                   choices = c("Top 3","Bottom 3"),
                                                                   selected = "Top 3", inline = TRUE)),
                                               column(width = 6,
                                                      selectInput("engsen", label = tags$span(style="color: white;","Select your Metric:"),
                                                                  choices = c("Engagement Rate", "Sentiment Score")))
                                             ), 
                                             fluidRow(
                                               withSpinner(plotOutput("top1")),
                                               column(12,align = "center",withSpinner(htmlOutput("ttop1"))),
                                               withSpinner(plotOutput("top2")),
                                               column(12,align = "center",withSpinner(htmlOutput("ttop2"))),
                                               withSpinner(plotOutput("top3")),
                                               column(12,align = "center",withSpinner(htmlOutput("ttop3"))),
                                               class = "shiny-box"
                                             )
                                             
                                    ),
                                    tabPanel("Analysis on date and time",
                                             fluidRow(
                                               column(width = 6, 
                                                      radioButtons("dateanalysis", label = tags$span(style="color: white;","Choose your sort:"),
                                                                   choices = c("Day of the Week","Hour of the Day","Overall"),
                                                                   selected = "Day of the Week", inline = TRUE))),
                                             tags$div(
                                               id = "center-container",
                                               style = "display: flex; justify-content: center; align-items: center;",
                                               withSpinner(plotlyOutput("day"))
                                             ),
                                             
                                             fluidRow(
                                               column(width = 4,
                                                      withSpinner(valueBoxOutput("day1",width = 3)),
                                                      withSpinner(valueBoxOutput("hour1",width = 3)))
                                             )
                                    ),
                                    tabPanel("Overall Summary",
                                             fluidRow(
                                               column(width = 3, withSpinner(plotlyOutput("eng", height = "150px"))),
                                               column(width = 3, withSpinner(plotlyOutput("sen", height = "150px"))),
                                               column(width = 3, withSpinner(plotlyOutput("lr", height = "150px"))),
                                               column(width = 3, withSpinner(plotlyOutput("cr", height = "150px")))
                                             ),
                                             fluidRow(
                                               column(width = 4, withSpinner(valueBoxOutput("progressBox", width = 3)),
                                                      withSpinner(valueBoxOutput("progressBox3", width = 3)),
                                                      withSpinner(valueBoxOutput("progressBox1", width = 3)),
                                                      withSpinner(valueBoxOutput("progressBox2", width = 3))
                                               )
                                             ),
                                               withSpinner(htmlOutput("output"))
                                    )
                                  )
                                )
                        )
                      )
                    )
)



server <- function(input, output,session) {
  output$logo <- renderPlot({
    plot(image_read("https://cdn-icons-png.flaticon.com/512/7270/7270513.png"))
    title(main = "InstaAnalytica",
          cex.main = 2,   font.main= 50, col.main= "white")
  },bg = "transparent")
  
  observeEvent(input$submit_name, {
    waitress <- Waitress$
      new(theme = "overlay-percent")$
      start() # start
    for(i in 1:10){
      waitress$inc(5)# increase by 10%
      if(i == 1){
        #setwd("C:/Users/17347/Documents/Instalytics")
        source_python("MasterProject.py")
        result <- main(as.character(input$text))
        input_value <- result[[1]]
        res <- result[[2]]
      }
      Sys.sleep(3)
      }
    if(res == "valid username")
      {
    Post_info <- read.csv(file = 'Post_info.csv')
    Prof_info <- read.csv(file = 'Prof_info.csv')
    Post_info$hashtags_list <- gsub("\\[", "", Post_info$hashtags_list)
    Post_info$hashtags_list <- gsub("\\]", "", Post_info$hashtags_list)
    Post_info$hashtags_list <- gsub("\\'", "", Post_info$hashtags_list)
    Post_info$hashtags_list <- gsub("\\,","", Post_info$hashtags_list)
    Following <- as.integer(Prof_info[nrow(Prof_info), ])
    Followers <- as.integer(Prof_info[nrow(Prof_info)-1, ])
    Post_info$likecomment <- Post_info$likes_list + Post_info$Comments_list
    Post_info$date <- as.Date(substr(Post_info$date_list,1,10))
    Post_info$Engagement_Rate <- (((Post_info$likecomment) / Followers)*100)
    Post_info <- Post_info %>%
      mutate(Post_Number = nrow(.) - row_number() + 1)
    # Convert date column to datetime format
    Post_info$date_list <- ymd_hms(Post_info$date_list)
    
    # Create new columns for day of week and hour of day
    Post_info$day_of_week <- weekdays(Post_info$date_list)
    Post_info$hour_of_day <- hour(Post_info$date_list)
    
    # Group data by day of week and calculate mean engagement rate
    Post_info_summary_day <- Post_info %>%
      group_by(day_of_week) %>%
      summarize(mean_engagement_rate = mean(Engagement_Rate))
    
    # Find the day of week with the highest engagement rate
    best_day <- Post_info_summary_day %>%
      slice_max(mean_engagement_rate) %>%
      pull(day_of_week)
    # Group data by hour of day and calculate mean engagement rate
    Post_info_summary_hour <- Post_info %>%
      group_by(hour_of_day) %>%
      summarize(mean_engagement_rate = mean(Engagement_Rate))
    
    # Find the hour of day with the highest engagement rate
    best_hour <- Post_info_summary_hour %>%
      slice_max(mean_engagement_rate) %>%
      pull(hour_of_day)
    # Group data by day of week and hour of day, and calculate mean engagement rate
    Post_info_summary_both <- Post_info %>%
      group_by(day_of_week, hour_of_day) %>%
      summarize(mean_engagement_rate = mean(Engagement_Rate))
    
    # Find the day of week and hour of day with the highest engagement rate
    best_day_hour <- Post_info_summary_both %>%
      slice_max(mean_engagement_rate) %>%
      summarize(day_of_week = first(day_of_week),
                hour_of_day = first(hour_of_day),
                mean_engagement_rate = first(mean_engagement_rate))
    
    raw_text <- c()
    for (values in Post_info$hashtags_list){
      raw_text <- append(values,raw_text)
    }
    
    # Example text
    text <- c()
    for (i in Post_info$Caption_list){
      text <- append(values,raw_text)
    }
    # Convert the text to a corpus object
    corpus <- Corpus(VectorSource(text))
    
    # Perform some text preprocessing
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    # Create a term-document matrix
    tdm <- TermDocumentMatrix(corpus)
    
    # Convert the term-document matrix to a data frame
    m <- as.data.frame(as.matrix(tdm))
    m$frequency <- (rowSums(m[1:nrow(m),1:ncol(m)]))
    raw_text_df <- data.frame(word = tdm$dimnames[[1]],frequency = m$frequency)

    #Calculate the Sentiment Scores
    final <- list()
    # Create a sample sentence to analyze
    for (x in 1:nrow(Post_info)){
      sentence <- Post_info$Caption_list[x]
      
      # Tokenize the sentence using tidytext
      tokenized_text <- tibble(text = sentence) %>% 
        unnest_tokens(word, text)
      
      # Get the sentiment score for each word using syuzhet's AFINN method
      sentiment_scores <- get_sentiment(tokenized_text$word, method = "afinn")
      
      # Calculate the overall sentiment score for the sentence
      sentence_sentiment <- sum(sentiment_scores)
      final[[x]] <- sentence_sentiment
      # Display the resulting sentiment score for the sentence
    }
    Post_info$SentimentScores <- as.integer(final)
    newtab <- switch(input$tabs, "username" = "dashboard","dashboard" = "username")
    updateTabItems(session, "tabs", newtab)
    waitress$close() 

    output$plot <- renderPlotly({
      if (input$chart == "Area Chart") {
        # Code to create Area Chart
        fig <- plot_ly(Post_info, x = ~date, y = ~likecomment, type = 'scatter',mode = 'markers',fill = 'tozeroy',fillcolor = '#e763fa', name = 'Likes',marker = list(color = '#e763fa'),line = list(color = "white")) %>%
          layout(
            xaxis = list(
              rangeselector = list(
                buttons = list(
                  list(count = 7, label = "1w", step = "day", stepmode = "backward"),
                  list(count = 1, label = "1m", step = "month", stepmode = "backward"),
                  list(count = 3, label = "3m", step = "month", stepmode = "backward"),
                  list(count = 6, label = "6m", step = "month", stepmode = "backward"),
                  list(count = 1, label = "1y", step = "year", stepmode = "backward"),
                  list(step = "all")
                )
              ),
              rangeslider = list()
            ),
            plot_bgcolor = "transparent", paper_bgcolor = "black",font = list(color = '#e763fa'))
        
      } else {
        # Code to create Line Chart
        fig <- plot_ly(line = list(color = "#e763fa")) %>%
          add_lines(x = Post_info$date, y = Post_info$likecomment, name = "Trend on the Likes and Comment") %>%
          layout(
            xaxis = list(
              rangeselector = list(
                buttons = list(
                  list(count = 7, label = "1w", step = "day", stepmode = "backward"),
                  list(count = 1, label = "1m", step = "month", stepmode = "backward"),
                  list(count = 3, label = "3m", step = "month", stepmode = "backward"),
                  list(count = 6, label = "6m", step = "month", stepmode = "backward", selected = TRUE),
                  list(count = 1, label = "1y", step = "year", stepmode = "backward"),
                  list(step = "all")
                )
              ),
              rangeslider = list()
            ),
            plot_bgcolor = "transparent", paper_bgcolor = "black",font = list(color = '#e763fa'))
      }
      fig
      
    })
    
    output$plot2 <- renderPlotly({

      # Convert the text to a corpus object
      corpus <- Corpus(VectorSource(raw_text))
      
      # Perform some text preprocessing
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, stopwords("english"))
      
      # Create a term-document matrix
      tdm <- TermDocumentMatrix(corpus)
      
      # Convert the term-document matrix to a data frame
      m <- as.data.frame(as.matrix(tdm))
      m$frequency <- (rowSums(m[1:nrow(m),1:ncol(m)]))
      raw_text_df <- data.frame(word = tdm$dimnames[[1]],frequency = m$frequency)
      top10 <- head(raw_text_df[order(-raw_text_df$frequency),], input$seleco) %>% 
        group_by(word) %>% summarise(Percentage=frequency/nrow(.))
      top10$Percentage <- round(top10$Percentage,9)
      packing <- circleProgressiveLayout(top10$Percentage, sizetype='area')
      packing$radius <- 0.95*packing$radius
      data <- cbind(top10, packing)
      dat.gg <- circleLayoutVertices(packing, npoints=50)
      plt <- ggplot() + 
        geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
        scale_fill_viridis() +
        geom_text(data = data, aes(x,y, size=Percentage, label = word), color="black") +
        theme_void() + 
        theme(legend.position="none")+ 
        coord_equal()
      ggplotly(plt) %>% 
        layout(
          xaxis = list(showgrid = FALSE),
          yaxis = list(showgrid = FALSE),
          plot_bgcolor = "transparent", 
          paper_bgcolor = "transparent",
          font = list(color = '#FFFFFF'))
    })
    
    output$wordcloud <- renderUI({
      custom_palette <- function(n) {
        sample(rgb(runif(n, 0, 1), runif(n, 0, 1), runif(n, 0, 1)), n)
      }
      wordcloud2(raw_text_df,
                 size = 3.5,
                 color = custom_palette(nrow(raw_text_df)),
                 minRotation = -pi/6, 
                 maxRotation = -pi/6, 
                 rotateRatio = 1,
                 backgroundColor = "black")
    })
    output$top1 <- renderPlot({
      if(input$five == "Top 3"){
        if(input$engsen == "Engagement Rate"){
          plot(image_read(head(Post_info[order(-Post_info$Engagement_Rate), c("url_list", "Engagement_Rate")], 3)[[1]][[1]]))
        }
        else{
          plot(image_read(head(Post_info[order(-Post_info$SentimentScores), c("url_list", "SentimentScores")], 3)[[1]][[1]]))
        }
      }
      else{
        if(input$engsen == "Engagement Rate"){
          plot(image_read(head(Post_info[order(Post_info$Engagement_Rate), c("url_list", "Engagement_Rate")], 3)[[1]][[1]]))
        }
        else{
          plot(image_read(head(Post_info[order(Post_info$SentimentScores), c("url_list", "SentimentScores")], 3)[[1]][[1]]))
        }
      }  },bg = "transparent")
    output$top2 <- renderPlot({
      if(input$five == "Top 3"){
        if(input$engsen == "Engagement Rate"){
          plot(image_read(head(Post_info[order(-Post_info$Engagement_Rate), c("url_list", "Engagement_Rate")], 3)[[1]][[2]]))
        }
        else{
          plot(image_read(head(Post_info[order(-Post_info$SentimentScores), c("url_list", "SentimentScores")], 3)[[1]][[2]]))
        }
      }
      else{
        if(input$engsen == "Engagement Rate"){
          plot(image_read(head(Post_info[order(Post_info$Engagement_Rate), c("url_list", "Engagement_Rate")], 3)[[1]][[2]]))
        }
        else{
          plot(image_read(head(Post_info[order(Post_info$SentimentScores), c("url_list", "SentimentScores")], 3)[[1]][[2]]))
        }
      }  },bg = "transparent")
    output$top3 <- renderPlot({
      if(input$five == "Top 3"){
        if(input$engsen == "Engagement Rate"){
          plot(image_read(head(Post_info[order(-Post_info$Engagement_Rate), c("url_list", "Engagement_Rate")], 3)[[1]][[3]]))
        }
        else{
          plot(image_read(head(Post_info[order(-Post_info$SentimentScores), c("url_list", "SentimentScores")], 3)[[1]][[3]]))
        }
      }
      else{
        if(input$engsen == "Engagement Rate"){
          plot(image_read(head(Post_info[order(Post_info$Engagement_Rate), c("url_list", "Engagement_Rate")], 3)[[1]][[3]]))
        }
        else{
          plot(image_read(head(Post_info[order(Post_info$SentimentScores), c("url_list", "SentimentScores")], 3)[[1]][[3]]))
        }
      }  },bg = "transparent")
    output$ttop1 <- renderText({
      if(input$five == "Top 3"){
        if(input$engsen == "Engagement Rate"){
          paste("<font color=\"#FFFFFF\"><b>",head(Post_info[order(-Post_info$Engagement_Rate), c("Caption_list", "Engagement_Rate")], 3)[[1]][[1]], "</b></font>")
        }
        else{
          paste("<font color=\"#FFFFFF\"><b>",head(Post_info[order(-Post_info$SentimentScores), c("Caption_list", "SentimentScores")], 3)[[1]][[1]], "</b></font>")
        }
      }
      else{
        if(input$engsen == "Engagement Rate"){
          paste("<font color=\"#FFFFFF\"><b>",head(Post_info[order(Post_info$Engagement_Rate), c("Caption_list", "Engagement_Rate")], 3)[[1]][[1]], "</b></font>")
        }
        else{
          paste("<font color=\"#FFFFFF\"><b>",head(Post_info[order(Post_info$SentimentScores), c("Caption_list", "SentimentScores")], 3)[[1]][[1]], "</b></font>")
        }
      }  })
    output$ttop2 <- renderText({
      if(input$five == "Top 3"){
        if(input$engsen == "Engagement Rate"){
          paste("<font color=\"#FFFFFF\"><b>",head(Post_info[order(-Post_info$Engagement_Rate), c("Caption_list", "Engagement_Rate")], 3)[[1]][[2]], "</b></font>")
        }
        else{
          paste("<font color=\"#FFFFFF\"><b>",head(Post_info[order(-Post_info$SentimentScores), c("Caption_list", "SentimentScores")], 3)[[1]][[2]], "</b></font>")
        }
      }
      else{
        if(input$engsen == "Engagement Rate"){
          paste("<font color=\"#FFFFFF\"><b>",head(Post_info[order(Post_info$Engagement_Rate), c("Caption_list", "Engagement_Rate")], 3)[[1]][[2]], "</b></font>")
        }
        else{
          paste("<font color=\"#FFFFFF\"><b>",head(Post_info[order(Post_info$SentimentScores), c("Caption_list", "SentimentScores")], 3)[[1]][[2]], "</b></font>")
        }
      }  })
    output$ttop3 <- renderText({
      if(input$five == "Top 3"){
        if(input$engsen == "Engagement Rate"){
          paste("<font color=\"#FFFFFF\"><b>",head(Post_info[order(-Post_info$Engagement_Rate), c("Caption_list", "Engagement_Rate")], 3)[[1]][[3]], "</b></font>")
        }
        else{
          paste("<font color=\"#FFFFFF\"><b>",head(Post_info[order(-Post_info$SentimentScores), c("Caption_list", "SentimentScores")], 3)[[1]][[3]], "</b></font>")
        }
      }
      else{
        if(input$engsen == "Engagement Rate"){
          paste("<font color=\"#FFFFFF\"><b>",head(Post_info[order(Post_info$Engagement_Rate), c("Caption_list", "Engagement_Rate")], 3)[[1]][[3]], "</b></font>")
        }
        else{
          paste("<font color=\"#FFFFFF\"><b>",head(Post_info[order(Post_info$SentimentScores), c("Caption_list", "SentimentScores")], 3)[[1]][[3]], "</b></font>")
        }
      }  })
    
    output$eng <- renderPlotly({
      value <- (mean(Post_info$likecomment)/Followers) * 100 # the value to display on the speedometer
      if (value > 10){
        value <- 10
      }
      if((value < 2 && value > 0)) {
        heading <- "Overall Performance: Barely Breathing"
      }
      else if ((value < 4 && value > 2)){
        heading <- "Overall Performance: Rising to the Top"
      }
      else if ((value < 6 && value > 4)){
        heading <- "Overall Performance: Starting to Soar"
      }
      else if ((value < 8 && value > 6)){
        heading <- "Overall Performance: Rocking and Rolling"
      }
      else{
        heading <- "Overall Performance: Killing It"
      }
      range_min <- 0  # the minimum value of the scale
      range_max <- 10  # the maximum value of the scale
      
      # create the gauge chart with colored ranges
      chart <- plot_ly(
        type = "indicator",
        mode = "gauge+number",
        gauge = list(
          axis = list(range = list(range_min, range_max)),
          bar = list(color = "#1f77b4"),
          steps = list(
            list(range = c(0, 1), color = "#FF4136"),
            list(range = c(1, 4), color = "#FFDC00"),
            list(range = c(4, range_max), color = "#2ECC40")
          )
        ),
        value = value,
        number = list(suffix = "%"),
        domain = list(x = c(0, 1), y = c(0, 1))
      )%>% 
        layout(plot_bgcolor = "transparent", paper_bgcolor = "transparent",title = heading,font = list(color = '#FFFFFF'))
      
      # add a scatter trace with a single point representing the needle
      chart <- chart %>%
        add_trace(
          type = "scatter",
          x = c(0.5),
          y = c(0.5),
          mode = "markers",
          marker = list(
            symbol = "triangle-up",
            size = 20,
            color = "#FFFFFF"
          ),
          showlegend = FALSE
        )
      
      # update the layout to hide the axis labels and ticks
      chart <- chart %>%
        layout(
          xaxis = list(
            showticklabels = FALSE,
            zeroline = FALSE,
            showgrid = FALSE
          ),
          yaxis = list(
            showticklabels = FALSE,
            zeroline = FALSE,
            showgrid = FALSE
          ),
          margin = list(l = 60, r = 60, t = 60, b = 20)
        )
      
      chart
    })
    
    output$sen <- renderPlotly({
      value <- round(mean(Post_info$SentimentScores),2)# the value to display on the speedometer
      if(value > 0) {
        heading <- "Positive Account"
      }
      else if (value < 0){
        heading <- "Negative Account"
      }
      else{
        heading <- "Neither Hot nor Cold"
      }
      
      range_min <- -20  # the minimum value of the scale
      range_max <- 20  # the maximum value of the scale
      
      # create the gauge chart with colored ranges
      chart <- plot_ly(
        type = "indicator",
        mode = "gauge+number",
        gauge = list(
          axis = list(range = list(range_min, range_max)),
          bar = list(color = "#1f77b4"),
          steps = list(
            list(range = c(range_min, 0), color = "#FF4136"),
            list(range = c(0, 0), color = "#FFDC00"),
            list(range = c(0, range_max), color = "#2ECC40")
          )
        ),
        value = value,
        number = list(suffix = "%"),
        domain = list(x = c(0, 1), y = c(0, 1))
      )%>% 
        layout(plot_bgcolor = "transparent", paper_bgcolor = "transparent", title = heading,font = list(color = '#FFFFFF'))
      
      # add a scatter trace with a single point representing the needle
      chart <- chart %>%
        add_trace(
          type = "scatter",
          x = c(0.5),
          y = c(0.5),
          mode = "markers",
          marker = list(
            symbol = "triangle-up",
            size = 20,
            color = "#FFFFFF"
          ),
          showlegend = FALSE
        )
      
      # update the layout to hide the axis labels and ticks
      chart <- chart %>%
        layout(
          xaxis = list(
            showticklabels = FALSE,
            zeroline = FALSE,
            showgrid = FALSE
          ),
          yaxis = list(
            showticklabels = FALSE,
            zeroline = FALSE,
            showgrid = FALSE
          ),
          margin = list(l = 60, r = 60, t = 60, b = 20)
        )
      
      chart
    })
    
    output$lr <- renderPlotly({
      value <- round(mean(Post_info$likes_list)/Followers * 100,1)# the value to display on the speedometer
      heading <- "Like Rate"
      
      range_min <- 0  # the minimum value of the scale
      range_max <- 100  # the maximum value of the scale
      
      # create the gauge chart with colored ranges
      chart <- plot_ly(
        type = "indicator",
        mode = "gauge+number",
        gauge = list(
          axis = list(range = list(range_min, range_max)),
          bar = list(color = "#1f77b4"),
          steps = list(
            list(range = c(range_min,10), color = "#FF4136"),
            list(range = c(10, 30), color = "#FFDC00"),
            list(range = c(30, range_max), color = "#2ECC40")
          )
        ),
        value = value,
        number = list(suffix = "%"),
        domain = list(x = c(0, 1), y = c(0, 1))
      )%>% 
        layout(plot_bgcolor = "transparent", paper_bgcolor = "transparent", title = heading,font = list(color = '#FFFFFF'))
      
      # add a scatter trace with a single point representing the needle
      chart <- chart %>%
        add_trace(
          type = "scatter",
          x = c(0.5),
          y = c(0.5),
          mode = "markers",
          marker = list(
            symbol = "triangle-up",
            size = 20,
            color = "#FFFFFF"
          ),
          showlegend = FALSE
        )
      
      # update the layout to hide the axis labels and ticks
      chart <- chart %>%
        layout(
          xaxis = list(
            showticklabels = FALSE,
            zeroline = FALSE,
            showgrid = FALSE
          ),
          yaxis = list(
            showticklabels = FALSE,
            zeroline = FALSE,
            showgrid = FALSE
          ),
          margin = list(l = 60, r = 60, t = 60, b = 20)
        )
      
      chart
    })
    
    output$cr <- renderPlotly({
      value <- round(mean(Post_info$Comments_list)/Followers * 100,1)# the value to display on the speedometer
      heading <- "Comment Rate"
      
      range_min <- 0  # the minimum value of the scale
      range_max <- 100  # the maximum value of the scale
      
      # create the gauge chart with colored ranges
      chart <- plot_ly(
        type = "indicator",
        mode = "gauge+number",
        gauge = list(
          axis = list(range = list(range_min, range_max)),
          bar = list(color = "#1f77b4"),
          steps = list(
            list(range = c(range_min, 10), color = "#FF4136"),
            list(range = c(10, 30), color = "#FFDC00"),
            list(range = c(30, range_max), color = "#2ECC40")
          )
        ),
        value = value,
        number = list(suffix = "%"),
        domain = list(x = c(0, 1), y = c(0, 1))
      )%>% 
        layout(plot_bgcolor = "transparent", paper_bgcolor = "transparent", title = heading,font = list(color = '#FFFFFF'))
      
      # add a scatter trace with a single point representing the needle
      chart <- chart %>%
        add_trace(
          type = "scatter",
          x = c(0.5),
          y = c(0.5),
          mode = "markers",
          marker = list(
            symbol = "triangle-up",
            size = 20,
            color = "#FFFFFF"
          ),
          showlegend = FALSE
        )
      
      # update the layout to hide the axis labels and ticks
      chart <- chart %>%
        layout(
          xaxis = list(
            showticklabels = FALSE,
            zeroline = FALSE,
            showgrid = FALSE
          ),
          yaxis = list(
            showticklabels = FALSE,
            zeroline = FALSE,
            showgrid = FALSE
          ),
          margin = list(l = 60, r = 60, t = 60, b = 20)
        )
      
      chart
    })
    
    text <- as.character(head(raw_text_df[order(-raw_text_df$frequency),c("word")],5))
    text1 <- as.character(head(Post_info[order(-Post_info$SentimentScores), c("Caption_list")], 5))
    
    mytext <- paste("The Instagram profile has an overall performance of ",round(mean(Post_info$likecomment)/Followers * 100,0),"%, calculated based on the likes count and comments count of each post.",br(),
                    "The profile received an average of", round(mean(Post_info$likes_list),0), " likes per post, with a maximum of ",max(Post_info$likes_list), "likes on the top-performing post.",br(), 
                    "The average comments count was around ",round(mean(Post_info$Comments_list),0)," per post, with a maximum of ",max(Post_info$Comments_list)," comments on the top-performing post.",br(),
                    "The engagement rate is an important metric to assess how well the content is resonating with the audience.",br(),
                    "Sentiment analysis of the captions has been conducted and the average sentiment score is",round(mean(Post_info$SentimentScores),1),"%. This provides insights into the general tone of the content being posted and how it may be perceived by the audience.",br(),
                    "There is a (Pearson's correlation coefficient)", round(cor(Post_info$Engagement_Rate, Post_info$SentimentScores),3),"% positive correlation between the engagement rate and sentiment score, indicating that posts with more positive captions tend to have higher engagement rates.",br(),
                    "The profile has",Followers,"followers and is following ",Following,"accounts. This provides context for the size of the audience and how the account is leveraging its reach.",br(),
                    "The hashtags used in the posts have been analyzed and the top 5 hashtags used by the profile were #",text[1],", #",text[2],", #",text[3],", #",text[4],", #",text[5],br(),
                    ". A word cloud has been generated to visualize the most frequently used ones. This provides insights into the themes and topics that are popular among the audience.",br(),
                    "The top-performing post based on sentiment score was a photo with a caption that read ","'",text1[1],"' .",br(),
                    "Overall, the engagement rate and sentiment score are the most important metrics to assess the performance of the profile on Instagram, as they provide insights into how well the content is resonating with the audience and the general tone of the content being posted. The follower and following counts also provide context for the size of the audience and how the account is leveraging its reach. Finally, the analysis of the hashtags used provides insights into the themes and topics that are popular among the audience.")
    output$output <- renderText({
      paste("<font color=\"#FFFFFF\"><b>",mytext, "</b></font>")
    })
    
    wordtext1 <- paste(br(),br(),"The word cloud generated for the Instagram analytics dashboard displays the most frequently occurring words in the captions used by the user. The size of each word in the cloud is proportional to its frequency in the captions. By analyzing this word cloud, one can get an overall idea of the themes and topics that the user tends to discuss in their captions. The word cloud can also provide insights into the user's interests, preferences, and the types of content that their followers engage with the most.")
    output$wordtext <- renderText({
      paste("<font color=\"#FFFFFF\"><b>",wordtext1, "</b></font>")
    })
    output$progressBox <- renderValueBox({
      valueBox(
        paste0(round(mean(Post_info$likecomment),0), ""), "Average Engagement (Likes + Comment)", icon = icon("group-arrows-rotate",lib = "font-awesome"),
        color = "maroon"
      )
    })
    output$progressBox1 <- renderValueBox({
      valueBox(
        paste0(Followers), "Followers", icon = icon("users-viewfinder",lib = "font-awesome"),
        color = "fuchsia"
      )
    })
    output$progressBox2 <- renderValueBox({
      valueBox(
        paste0(Following), "Following", icon = icon("user-plus",lib = "font-awesome"),
        color = "purple"
      )
    })
    output$progressBox3 <- renderValueBox({
      valueBox(
        paste0(nrow(Post_info)), "Total Post", icon = icon("images",lib = "font-awesome"),
        color = "blue"
      )
    })
    output$day <- renderPlotly({
      if(input$dateanalysis == "Day of the Week"){
        plt <- ggplot(Post_info_summary_day, aes(x = day_of_week, y = mean_engagement_rate)) +
          geom_bar(stat = "identity", fill = "#e763fa",alpha = 0.8) +
          labs(title = "Engagement Rate by Day of Week",
               x = "Day of Week",
               y = "Engagement Rate")
        ggplotly(plt) %>% 
          layout(xaxis = list(showgrid = FALSE),
                 yaxis = list(showgrid = FALSE),
            plot_bgcolor = "transparent", 
            paper_bgcolor = "black",
            font = list(color = '#e763fa'))
      }
      else if(input$dateanalysis == "Hour of the Day"){
        plt <- ggplot(Post_info_summary_hour, aes(x = hour_of_day, y = mean_engagement_rate)) +
          geom_line(color = "#e763fa",alpha = 0.8) +
          labs(title = "Engagement Rate by Hour of Day",
               x = "Hour of Day",
               y = "Engagement Rate")
        ggplotly(plt) %>% 
          layout(xaxis = list(showgrid = FALSE),
                 yaxis = list(showgrid = FALSE),
            plot_bgcolor = "transparent", 
            paper_bgcolor = "black",
            font = list(color = '#e763fa'))
      }
      else{
        plot_ly(Post_info_summary_both, x = ~hour_of_day, y = ~day_of_week, z = ~mean_engagement_rate,
                type = "heatmap", colors = colorRamp(c("white", "#e763fa")),
                colorbar = list(title = "Engagement Rate")) %>%
          layout(title = "Engagement Rate by Day of Week and Hour of Day",
                 xaxis = list(title = "Hour of Day"),
                 yaxis = list(title = "Day of Week"),
                 xaxis = list(showgrid = FALSE),
                 yaxis = list(showgrid = FALSE),
                 plot_bgcolor = "transparent", 
                 paper_bgcolor = "black",
                 font = list(color = '#e763fa'))
      }

    })
    output$day1 <- renderValueBox({
      valueBox(
        paste0(head(best_day_hour[order(-best_day_hour$mean_engagement_rate),]$day_of_week, 1)), "Day of the Week", icon = icon("images",lib = "font-awesome"),
        color = "blue"
      )
    })
    output$hour1 <- renderValueBox({
      valueBox(
        paste0(head(best_day_hour[order(-best_day_hour$mean_engagement_rate),]$hour_of_day, 1)), "Hour of the Week", icon = icon("images",lib = "font-awesome"),
        color = "blue"
      )
    })
    }
    else{
      waitress$close()
      shinyalert(paste(res),type = 'info')
    }
    })
  
  # output$plot1 <- renderPlot({
  #   wordcloud(words = tdm$dimnames[[1]], freq = m$frequency,
  #             min.freq = input$freq, max.words = input$n, random.order = TRUE,
  #             colors = brewer.pal(8, "Spectral"),
  #             family = "serif", font = 15,scale = c(10,1))
  # },
  # height = 600,
  # width = 1400,
  # bg = "transparent")
  
}
shinyApp(ui, server)