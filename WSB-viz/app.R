###########
#Libraries#
###########

library(shiny) # for back end
library(tidyverse) # for 
library(bslib) # for theme
library(stringr) # for string manipulation
library(DT)
library(ggrepel)
library(viridis)
library(wordcloud) 
library(tidytext)
library(cluster)



######
#Data#
######

#Section to read in data

data <- read_csv("www/wsb_dd_submissions.csv") %>%
  mutate(created_utc = strptime(created_utc, format="%s"))

tickers <- read_csv("www/tickers.csv")

max_date <- as.Date(max(data$created_utc))
###########
#Functions#
###########


# clustering plot helper function
count_and_sent <- function(df){
  sentiment_of_stock <- function(ticker){
    filtered_rows <- df %>%
      filter(grepl(paste("\\b", ticker, "\\b", sep = ""), title_stocks))
    return(mean(filtered_rows$title_sentiment))
  }
  
  stocks <- df$title_stocks[!is.na(df$title_stocks)]
  stocks <- paste(stocks, sep = " ")
  stocks <- as.list(unlist(strsplit(stocks, '[[:space:]]')))
  stocks <- unlist(stocks)
  grouped <- tibble("stock" = stocks) %>%
    group_by(stock) %>%
    summarise(mentions=n())
  grouped$sentiment <- map_dbl(grouped$stock, sentiment_of_stock)
  return(grouped)
}

# bar plot helper function
get_top10 <- function(df){
  top10 <- df %>%
    group_by(ticker) %>%
    summarise(count = sum(n)) %>%
    arrange(desc(count)) %>%
    slice(1:10)
  return(top10)
}


ticker_to_field <- function(ticker){
  sector <- match(ticker, tickers$Symbol, nomatch = -1)
  if (sector==-1||is.na(tickers$Sector[sector]) ){
    return("Other")
  }
  return(tickers$Sector[sector])
}

pivot_sector <- function(df, input_col, output_col){
  df <- df %>%
    select(input_col, output_col) #grabs two target columns
  df <- df[!(is.na(df[[input_col]]) | df[[input_col]]==""), ] #removes empty rows
  df <- separate_rows(df, `input_col`, sep=" ") # seperates out if multiple stocks
  
  stocks <- str_c("\\b", tickers$Symbol, "\\b", collapse="|") #makes a regex of all stocks
  
  df$sector <- str_replace_all(df[[input_col]], stocks, ticker_to_field) #replaces stocks with sectors
  
  #df <- df[df$sector %in% c("Other", unique(tickers$Sector))]
  
  return(df[c("sector", output_col)]) #returns target and sector
}


#############
#Application#
#############

ui <- fluidPage(
  
  theme = bs_theme(bg = "#ffffff", #Setting theme
                   fg = "#000000", 
                   primary = "#273c75", 
                   secondary = "#f1c40f", 
                   base_font = "Arial", 
                   heading_font = "Verdana",
                   bootswatch = "flatly"), 
  
  
  # Application title
  
  navbarPage("WSB DD dashboard: STONKS ONLY GO UP",
             tabPanel("Stock Sentiment",
                      titlePanel("Stocks Sentiment Cluster"),
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput("clusterCreateRange",
                                         "Created Range:",
                                         min = "2018-01-01",
                                         max = max_date,
                                         start = "2021-04-04",
                                         end = max_date),
                          numericInput('clusters', 
                                       'Cluster count', 
                                       4, 
                                       min = 1, 
                                       max = 9),
                          submitButton("Change Output")
                        ), #end sidebar panel
                        
                        mainPanel(
                          plotOutput("cluster_graph")
                        )
                      ) # end SIdebar layout
             ), # End StockSentiment Clustering panel
             
             # bar plot page
             tabPanel("Popular Stocks",
                      titlePanel("Most Discussed Stocks"),
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput("barCreateRange",
                                         "Created Range:",
                                         min = "2018-01-01",
                                         max = max_date,
                                         start = "2021-04-29",
                                         end = max_date),
                          submitButton("Change Output")
                        ), #end sidebar panel
                        mainPanel(
                          plotOutput("bar_graph")
                        )
                      ) # end Sidebar layou
             ), #end bar plot panel
             
             # word cloud plot page
             tabPanel("Word Cloud",
                      titlePanel("Stock Key Words"),
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput("cloudCreateRange",
                                         "Created Range:",
                                         min = "2018-01-01",
                                         max = max_date,
                                         start = "2021-03-28",
                                         end = max_date),
                          submitButton("Change Range"),
                          selectizeInput("StockTicker",
                                         "Stock Ticker: ",
                                         choices = NULL,
                                         multiple = FALSE),
                          hr(),
                          sliderInput("freq",
                                      "Minimum Frequency:",
                                      min = 1,  max = 20, value = 1),
                          sliderInput("max",
                                      "Maximum Number of Words:",
                                      min = 1,  max = 50,  value = 50),
                          submitButton("Change Output")
                        ), #end sidebar panel
                        mainPanel(
                          uiOutput("plotOrPrint")
                        )
                      ) # end Sidebar layou
             ), #end word cloud plot panel
             
             tabPanel("Sector Sentiment",
                      titlePanel("Sector Sentiment"),
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput("sectorCreateRange",
                                         "Created Range:",
                                         min = "2018-01-01",
                                         max = max_date,
                                         start = "2021-04-01",
                                         end = max_date),
                          selectInput("tickersFrom",
                                      "Tickers from: ",
                                      c("Selftext" = "post_stocks",
                                        "Title" = "title_stocks"),
                                      selected = "title_stocks"),
                          selectInput("sentimentOut",
                                      "Sentiment based on: ",
                                      c("Title Sentiment" = "title_sentiment", 
                                        "Post Sentiment" = "post_sentiment", 
                                        "Score" = "score", 
                                        "Number of Comments" = "num_comments", 
                                        "Number of Awards" = "count_awards"),
                                      selected = "title_sentiment"),
                          submitButton("Change Output")
                        ), #end sidebar panel
                        mainPanel(
                          plotOutput("sector_graph")
                        )
                      ) # end Sidebar layou
             ), #end word cloud plot panel
             
             tabPanel("Table",
                      titlePanel("Table"),
                      ######################
                      #Sidebar layout for DF
                      ######################
                      sidebarLayout(
                        sidebarPanel(
                          textInput("title",
                                    "Post Title: "),
                          selectizeInput("author",
                                         "Author: ",
                                         choices = NULL,
                                         multiple = TRUE),
                          sliderInput("scoreRange", "Post Score:",
                                      min = 0, max = 5700,
                                      value = c(0, 5700),
                                      sep=""),
                          dateRangeInput("createRange",
                                         "Created Range:",
                                         min = "2018-01-01",
                                         max = max_date,
                                         start = "2018-08-02",
                                         end = max_date),
                          selectizeInput("titleStocks",
                                         "Title Stocks: ",
                                         choices = NULL,
                                         multiple = TRUE),
                          selectizeInput("postStocks",
                                         "Post Stocks: ",
                                         choices = NULL,
                                         multiple = TRUE),
                          sliderInput("titleSentiment", "Title Sentiment:",
                                      min = -20, max = 16,
                                      value = c(-20, 16),
                                      sep=""),
                          sliderInput("postSentiment", "Post Sentiment:",
                                      min = -240, max = 260,
                                      value = c(-240, 260),
                                      sep=""),
                          checkboxInput("onlySelftext",
                                        "Only posts with selftext",
                                        FALSE),
                          submitButton("Change Output")
                        ),
                        ##############
                        #Main panel DF
                        ##############
                        mainPanel(
                          tabPanel("Table", 
                                   DTOutput("table")
                          )
                        )
                        
                      ) #End sidebarLayout
             ), # End table page
             tabPanel(
               "Relevant Information",
               
               tags$p(h3("Developers")),
               tags$p(
                 "This app was created by",
                 tags$a("Taylor Blair",
                        href = "https://github.com/Goodernews",
                        taget = "_blank"),
                 ",  ",
                 tags$a("Jiarong Li",
                        href = "https://github.com/jialicatherine",
                        taget = "_blank"),
                 ", and",
                 tags$a("Sung Bum (Simon) Ahn",
                        href = "https://github.com/ahnsb5117",
                        taget = "_blank"),
                 "as a final project for Math 241: Data Science at Reed College in Spring 2021.
      The goal of this app is to allow Wall Street Bets enthusiasts to explore and visualize their stocks."
               ),
               tags$p(h3("Sources and Packages")),
               tags$p(
                 "The data used for this app was primarily pulled and scraped from Reddit subreddit",
                 tags$a("r/wallstreetbets Due Diligence(DD),",
                        href = "https://www.reddit.com/r/wallstreetbets/?f=flair_name%3A%22DD%22",
                        target = "_blank"),
                 "and new packages were used are",
                 tags$code("bslib"),
                 ", ",
                 tags$code("cluster"),
                 ", and",
                 tags$code("stats"),
                 ".", " The new components are k_mean clustering analysis using "  ,tags$code("stats"), " package, and we also used new Rshiny functions such as observeEvent and renderUI."  ),
               tags$p("The app was last updated on", max_date, ". This data will be updated monthly."
               ),
               tags$p(h3("Catalog")),
               tags$p(h5("Stock sentiment k- mean cluster"), " 
These stock clusters composed of points show how similar negative or positive sentiment they have to each other through partitioning the number of observation into k number of clusters . This is achieved through package(cluster) and using k-mean function. "
               ),
               tags$p(strong("Possible Questions: "), ("Which stocks would be in the same cluster according to the stock sentiment and the mentions in a (give a date range)?")),
               tags$p(h5("Popular Stocks"), " 
The bar graph displays most discussed top10 stocks  in r/wallstreetbets in the given time range"),
               tags$p(strong("Possible Questions: "), ("What are the top 3 most discussed stocks in March 2021?")),
               tags$p(h5("Word Cloud "), " 
For a certain stock, it allows visualization of keywords that are associated with that stock on the r/wallstreetbets DD posts."),
               tags$p(strong("Possible Questions: "), ("According to the post keywords mentioned related to your chosen stock, do you think you should buy the  chosen stock?")),
               tags$p(h5("Sector Sentiment")," 
This visualizes sentiments of thirteen sectors , which can be filtered by the body or the title of the post. "),
               tags$p(strong("Possible Questions: "), ("What sector out of the given 13 sectors has the highest (most positive) sentiment on March 2021?")),
               tags$p(h5("Table"), " 
If you want to know more details about the entire DD posts without going through the hassle of leaving the dashboard. The table tells you about the title, body, author, date of creation, upvotes, number of awards,
")
               
               
             )
             
  ) # End navbar Page
) # End fluid Page


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Table server
  updateSelectizeInput(session, 'titleStocks', 
                       choices = tickers$Symbol, 
                       server = TRUE)
  updateSelectizeInput(session, 'postStocks', 
                       choices = tickers$Symbol, 
                       server = TRUE)
  updateSelectizeInput(session, 'author', 
                       choices = unique(data$author), 
                       server = TRUE)
  
  table_clean <- reactive({
    filtered <- data %>%
      subset(select=-c(permalink, coin_awards, num_comments))
    
    if(length(input$titleStocks)>0){ #filters title stocks
      input_regex_clean <- str_c("\\b", input$titleStocks, "\\b")
      input_regex_clean <- paste(input_regex_clean, collapse = "|")
      filtered <- filtered %>%
        filter(grepl(input_regex_clean, title_stocks))
    }
    if(length(input$postStocks)>0){
      input_regex_clean <- str_c("\\b", input$postStocks, "\\b")
      input_regex_clean <- paste(input_regex_clean, collapse = "|")
      filtered <- filtered %>%
        filter(grepl(input_regex_clean, post_stocks))
    }
    if(length(input$author)>0){
      input_regex_clean <- str_c("\\b", input$author, "\\b")
      input_regex_clean <- paste(input_regex_clean, collapse = "|")
      filtered <- filtered %>%
        filter(grepl(input_regex_clean, author))
    }
    if(input$title!=""){
      filtered <- filtered %>%
        filter(grepl(tolower(input$title), tolower(title)))
    }
    
    filtered <- filtered %>%
      filter(score >= input$scoreRange[1],
             score <= input$scoreRange[2],
             post_sentiment >= input$postSentiment[1],
             post_sentiment <= input$postSentiment[2],
             title_sentiment >= input$titleSentiment[1],
             title_sentiment <= input$titleSentiment[2],
             created_utc >= as.Date(input$createRange[1]),
             created_utc <= as.Date(input$createRange[2])) %>%
      mutate(created_utc = strftime(created_utc, format="%Y-%m-%d %H:%M:%S")) %>% 
      rename(`Created UTC` = created_utc,
             `Count Awards` = count_awards,
             `Title Sentiment` = title_sentiment,
             `Post Sentiment` = post_sentiment,
             `Title Stocks` = title_stocks,
             `Post Stocks` = post_stocks)
    
    
    return(as.data.frame(filtered))
    
  })
  
  output$table <- renderDT(table_clean()) # Name of table needed, displays table
  # End Table server
  
  # Sentiment server
  cluster_clean <- reactive({
    clean_data <- data %>%
      filter(created_utc >= as.Date(input$clusterCreateRange[1]),
             created_utc <= as.Date(input$clusterCreateRange[2]),
             title_sentiment!=0) %>%
      count_and_sent() 
    clean_data_num <- clean_data %>% 
      select(sentiment, mentions)
    
    df <- clean_data_num
    df <- na.omit(df)
    df <- scale(df)
    kmeans <- kmeans(df, centers = input$clusters, nstart = 25)
    df_clustered <- clean_data %>%
      mutate(cluster = as.factor(kmeans$cluster))
    return(as.data.frame(df_clustered))
  }) # end cluster clean
  
  output$cluster_graph <- renderPlot({
    cluster_clean()  %>%
      ggplot(aes(x = mentions, y = sentiment, color = cluster)) +
      geom_point() +
      scale_y_log10()+
      scale_x_log10()+
      geom_text_repel(aes(label =stock), size = 3.5) +
      xlab("mentioned times") +
      theme_minimal() 
    
  }) # end of cluster_graph server
  # End Sentiment Server
  
  # bar Server
  bar_data <- reactive({
    clean_data <- data %>%
      filter(created_utc >= as.Date(input$barCreateRange[1]),
             created_utc <= as.Date(input$barCreateRange[2])) %>%
      mutate(ticker=strsplit(title_stocks, " ")) %>% 
      unnest(ticker) %>% 
      drop_na(ticker) %>%
      group_by(created_utc) %>%
      count(ticker) %>%
      get_top10()
    return(as.data.frame(clean_data))
  }) # end bar data
  
  output$bar_graph <- renderPlot({
    bar_data() %>%
      ggplot(aes(x = reorder(ticker, -count), y = count, fill = count)) + 
      geom_col() +
      labs(x = "Top 10 Stock Tickers") +
      theme_minimal() 
  })
  # End bar Server
  # Sector Server
  output$sector_graph <- renderPlot({
    data %>%
      filter(created_utc >= as.Date(input$sectorCreateRange[1]),
             created_utc <= as.Date(input$sectorCreateRange[2])) %>%
      pivot_sector(input$tickersFrom, input$sentimentOut) %>%
      ggplot(aes_string(x="sector", y=input$sentimentOut)) +
      coord_flip() +
      geom_boxplot() +
      geom_jitter(width = 0.25, alpha = 0.3) +
      labs(y=input$sentimentOut, 
           x="Sector")
    
    
  })
  # End of Sector Server
  cloud_data <- reactive({
    clean_data <- data %>%
      filter(created_utc >= as.Date(input$cloudCreateRange[1]),
             created_utc <= as.Date(input$cloudCreateRange[2]),
             !is.na(post_stocks)) %>%
      mutate(ticker=strsplit(title_stocks, " ")) %>% 
      unnest(ticker) %>% 
      select(ticker, selftext, created_utc) %>%
      drop_na() %>%
      unnest_tokens(word, selftext) %>%
      filter(word != "https"& word != "removed" & word != "amp" & word != "png" & word != "jpg" & word != "pjpg" & word != "preview.redd.it" & word != "webp" & word != "auto" & word != "width" & word != "format" & word != "auto") %>%
      mutate(TF = grepl("\\d",word)) %>%
      filter(TF != TRUE)
    return(clean_data)
  }) # end cloud clean
  
  observeEvent(input$cloudCreateRange, {
    updateSelectizeInput(session, 'StockTicker',
                         choices = as.character(unique(cloud_data()$ticker)),
                         server = TRUE)
  })
  
  cloud_data2 <- reactive({
    clean_data <- cloud_data() %>%
      filter(ticker == input$StockTicker) %>%
      anti_join(stop_words, by = c("word" = "word")) %>%
      count(word) %>%
      arrange(desc(n)) %>%
      slice(1:50)
    return(as.data.frame(clean_data))
  }) # end cloud clean
  
  output$IPLMatchPlot <- renderPlot({        
    wordcloud(words=cloud_data2()$word,
              freq = cloud_data2()$n,
              min.freq = input$freq, max.words=input$max, random.order=FALSE,
              rot.per=0.2, colors=brewer.pal(5, "Dark2"), scale=c(4,0.1))
  })
  
  output$IPLMatchPrint <- renderText({        
    "Please select a date range and an interested stock ticker.\nClick Change buttons to update!"
  })
  
  output$plotOrPrint <- renderUI({
    if (nrow(cloud_data2()) == 0) {
      verbatimTextOutput("IPLMatchPrint")
    } else {
      plotOutput("IPLMatchPlot")
    }
  })
  # End Word Cloud Server
}

# Run the application 
shinyApp(ui = ui, server = server)
