library(tidyverse)
library(tidytext)
library(ggplot2)

wsb <- read_csv("data/merged_submissions.csv")
merged_common <- read_csv("Data/merged_common.csv") %>%
  mutate(Symbol = tolower(Symbol))



sentiments <- get_sentiments("afinn")
calc_sentiment <- function(string){
  split <- unlist(strsplit(string, "[[:punct:] ]"))
  indexed <- match(split, sentiments$word)
  vals <- sentiments$value[indexed]
  return(sum(vals, na.rm=TRUE))
}


named_stocks <- function(string){ # Finds stocks named
  split <- unlist(strsplit(tolower(string), "[[:punct:] ]"))
  indexed <- match(split, merged_common$Symbol)
  vals <- toupper(merged_common$Symbol[indexed])
  clean <- vals[!is.na(vals)]
  return (paste(clean, collapse = ' '))
}

clean_awards <- function(string){ # Takes the all awardings column and returns three cols: count awards, coin coint, award names
  if (is.na(string) || string=="[]"){
    return(list("count_awards" = 0, "coin_awards" = 0))
  }
  split_awards <- str_extract_all(string, "(?<=\\{).+?(?=\\})")[[1]]
  only_awards <- split_awards[str_detect(split_awards, "award_sub_type")]
  
  count_awards <- str_extract_all(only_awards,"(?<=count': ).+(?=, 'days_of_drip_extension)")
  count_awards <- sum(as.numeric(unlist(count_awards)))
  
  coin_awards <- str_extract_all(only_awards,"(?<=coin_price': ).+(?=, 'coin_reward)")
  coin_awards <- sum(as.numeric(unlist(coin_awards)))
  
  return(list("count_awards" = count_awards, "coin_awards" = coin_awards))
}

awards <- map(wsb$all_awardings, clean_awards)

awards <- as.data.frame(do.call(rbind,
                      awards))

awards[] <- lapply(awards, unlist) #changes type


wsb <- cbind(wsb, awards) #joins datasets

wsb <- subset(wsb, select = -c(all_awardings)) #removes all awardings column


wsb$title_sentiment <- map(wsb$title, calc_sentiment)%>%
  unlist()

wsb$post_sentiment <- map(wsb$selftext, calc_sentiment)%>%
  unlist()

wsb$title_stocks <- map(wsb$title, named_stocks) %>%
  unlist()
wsb$post_stocks <- map(wsb$selftext, named_stocks) %>%
  unlist()


write_csv(wsb, "data/wsb_dd_submissions.csv") #writes to dev
write_csv(wsb, "WSB-viz/www/wsb_dd_submissions.csv") # Writes to shiny


#####################
# Ticker Prep
#########################

# Supposed to reduce the payload, currently not fuctioning

write_csv(merged_common, "WSB-viz/www/tickers.csv")

# titles <- wsb$title_stocks
# stocks <- c(wsb$title_stocks, wsb$post_stocks)
# 
# stocks <- stocks[!is.na(stocks)]
# 
# stocks <- paste(stocks, sep = " ")
# 
# stocks <- as.list(unlist(strsplit(stocks, '[[:space:]]')))
# 
# stocks <- unlist(unique(stocks))
# 
# merged_common <- merged_common[!is.na(match(merged_common$Symbol, stocks)), ]

#write_csv(merged_common, "WSB-viz/www/tickers.csv")

