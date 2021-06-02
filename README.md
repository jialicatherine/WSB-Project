# WSB-Project

Scrape and wrangle post data from wallstreetbets on Reddit and perform sentiment analysis to predict stock trend.

# How to prep data

To shrink the size of the repo some of the data will need to be updated or created, to do so:

1. Run `Dev/update_WSB_scrape.py`. This will update the `Data/raw-historical/2021_raw_wsb_dd_submissions.csv`
2. Run `Dev/merge_raws.r`. This will combine the historical data into a singular CSV which can be cleaned
3. Run `Dev/NLP_extract.R`. This does the feature mining (sentiment, mentioned stocks, award counts).

# Running the Shiny App

After completing the above steps, click run app in `WSB-viz/app.R`

# Notes

This was a project with [Simon Ahn](https://github.com/ahnsb5117) and [Taylor Blair](https://github.com/Goodernews) in [Kelly McConville's](https://github.com/mcconvil) Data Science 241 class at Reed College.

ShinyApp Dashboard Link：https://jialicatherine.shinyapps.io/WSB-viz/
