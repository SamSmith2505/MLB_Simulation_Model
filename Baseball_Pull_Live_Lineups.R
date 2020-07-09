#Libraries
library(httr)
library(rvest)
library(purrr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(mongolite)

#####################
# ~ Pull tables from mongodb ~ #
#####################

#Credentials
username <- 'thatsmrlongcut'
password <- 'football17'

#Index of table names in mongo to environment names in R
index <-
  data.frame(
    MONGODB_NAME = c("MLB_GAME_LIST", "MLB_BATTING_ORDER"),
    ENV_NAME = c('games', 'batting_order'),
    stringsAsFactors = FALSE
  )

#Run a loop and import/assign
for(i in 1:nrow(index)) {
  #Establish connection with mongo db
  con <-
    mongo(
      collection = "NFL",
      db = index$MONGODB_NAME[i],
      url = paste0("mongodb+srv://", username, ":", password, '@nfl-bsxce.mongodb.net/test?retryWrites=true&w=majority')
    )
  #Assign object
  assign(index$ENV_NAME[i], con$find(), envir = .GlobalEnv)
  rm(con)
}


#####################
# ~ Do what you need ~ #
#####################

#Observe what games have a batting order currently (THIS WILL UPDATE LIVE -- MY PC IS CONTINUOUSLY SCRAPING FOR IT)

batting_order = data.table(batting_order)
lineup_dt = batting_order[, .(
  game_id = gameid,
  team,
  home_away,
  player = fullName,
  lineup_spot = battingOrder,
  position = as.integer(primaryPosition_code)
)]
lineup_dt[is.na(position), position := 10]



