library(httr)
library(rvest)
library(purrr)
library(jsonlite)
library(dplyr)
library(lubridate)

#Function to transform 1:1 game info
info_structure <- function(.data) {
  idf <- 
    .data %>%
    unlist
  
  idf %>%
    paste %>%
    as.data.frame %>%
    t %>%
    as.data.frame %>%
    mutate_all(paste) %>%
    setNames(names(idf))
}

#Get today's date
today.date <- as.Date(now())

#Establish the schedule URL -- link that has all the links to the games today
base_url <- "https://statsapi.mlb.com/api"
today_sch_url <- paste0(base_url, "/v1/schedule?sportId=1&date=", today.date, "&gameTypes=E,S,R,A,F,D,L,W&hydrate=team(leaders(showOnPreview(leaderCategories=[homeRuns,runsBattedIn,battingAverage],statGroup=[pitching,hitting]))),linescore(matchup,runners),flags,liveLookin,review,broadcasts(all),decisions,person,probablePitcher,stats,homeRuns,previousPlay,game(content(media(featured,epg),summary),tickets),seriesStatus(useOverride=true)&useLatestGames=false&scheduleTypes=events,games&language=en&leagueIds=103,104")
#//TO DO -- all those additional commands in that link will eventually be problematic (so I will look to erasing them)

#Get all the links for today's games!
schedule <- 
  GET(today_sch_url) %>%
  read_html() %>%
  html_text() %>%
  fromJSON() %>%
  .$dates %>%
  .$games %>%
  map(~dplyr::select(., gamePk)) %>%
  unlist() %>%
  as.character

#Create the link to the API for that game
#// TO DO There is an easier way to go about this but it would require me digging through their API for hours
game_links <- paste0("https://statsapi.mlb.com/api/v1.1/game/", schedule, "/feed/live?language=en")


all_data <-
  #Run a loop through all of the games scheduled
  lapply(1:length(schedule), function(j) {
    print(j)
    tryCatch({
      #Establish the gameid in the loop
      game_id <- schedule[j]
      
      #Get the json dump
      df <- 
        GET(game_links[j]) %>%
        read_html() %>%
        html_text() %>%
        fromJSON()
      
      #Grab the home and away team names
      index_1 <- c("home", "away")
      index_2 <- c("H", "A")
      
      home_away_df <-
        lapply(1:length(index_1), function(i) {
          df$gameData$teams[[index_1[i]]][c("id", "name", "teamCode", "abbreviation")] %>% 
            info_structure(.) %>%
            dplyr::select(team = name) %>%
            mutate(h_a = index_2[i])
        }) %>%
        invoke(rbind, .)
      
      #Collect the player information
      plyrs <-
        lapply(1:length(df$gameData$players), function(x) {
          df$gameData$players[[x]] %>% info_structure(.)
        }) %>%
        invoke(plyr::rbind.fill, .) %>%
        #Only select what you think might be necessary
        dplyr::select(player_id = id, fullName, nameSlug, active, primaryPosition.code, primaryPosition.type, primaryPosition.abbreviation)
      
      #Combine the batting order for the home/away team
      index_1 <- c("home", "away")
      index_2 <- c("H", "A")
      
      #Check to see if there currently is a batting order!
      bat_ordr_avail <- 
        (lapply(1:length(index_1), function(i) {
          (df$liveData$boxscore$teams[[index_1[i]]]$battingOrder %>% length()) > 0
        }) %>%
          invoke(c, .) %>%
          sum) == 2
      
      #If the batting order is not available output just the home/away/team/gameid bs
      if(bat_ordr_avail == FALSE) {
        end_df <- 
          home_away_df %>%
          mutate(gameid = game_id,
                 home_away = h_a,
                 compile.timestamp = now()) %>%
          dplyr::select(compile.timestamp, gameid, team, home_away)
      } else {
        #Structure the end frame
        end_df <-
          lapply(1:length(index_1), function(i) {
            #Raw batting order
            batting_order <-
              df$liveData$boxscore$teams[[index_1[i]]]$battingOrder %>% 
              as.data.frame %>% 
              setNames('player_id') %>% 
              mutate_all(paste) %>% 
              mutate(battingOrder = row_number()) %>%
              left_join(plyrs, by = 'player_id')
            
            #Grab the probable pitchers for the game
            probable_pitcher <- 
              df$gameData$probablePitchers[[index_1[i]]] %>% 
              info_structure(.) %>% 
              as.data.frame %>% 
              dplyr::select(player_id = id) %>%
              left_join(plyrs, by = 'player_id') %>%
              mutate(battingOrder = 10)
            
            #If the pitchers ID is found in the batting order -- do nothing
            if(probable_pitcher$player_id %in% batting_order$player_id) {} else {
              #If it is not found -- add them as the 10th batter
              batting_order <- rbind(batting_order, probable_pitcher)
            }
            
            #Add the meta data and output
            batting_order %>%
              mutate(home_away = index_2[i],
                     team = home_away_df$team[match(home_away, home_away_df$h_a)],
                     gameid = game_id)
          }) %>%
          invoke(rbind, .) %>%
          #Add datetime of compile
          mutate(compile.timestamp = now()) %>%
          dplyr::select(compile.timestamp, gameid, team, home_away,
                        fullName, nameSlug, active, battingOrder, primaryPosition.code, 
                        primaryPosition.type, primaryPosition.abbreviation)
      }
      #Output
      return(end_df)
    }, error = function(e) {})
    
  }) %>%
  plyr::compact() %>% 
  invoke(plyr::rbind.fill, .)

all_data = data.table(all_data)

all_data[, game_count := .N, by = gameid]

lineup_dt = all_data[game_count > 11, .(
  game_id = gameid,
  team,
  home_away,
  player = fullName,
  lineup_spot = battingOrder,
  position = primaryPosition.code
)]

lineup_dt[player == "Jackie Bradley Jr.", player := "Jackie Bradley"]
lineup_dt[player == "Fernando Tatis Jr.", player := "Fernando Tatis"]
lineup_dt[player == "Shed Long Jr.", player := "Shed Long"]
lineup_dt[player == "Michael A. Taylor", player := "Michael Taylor"]
lineup_dt[player == "Nick Castellanos", player := "Nicholas Castellanos"]
lineup_dt[player == "Nicky Delmonico", player := "Nick Delmonico"]
lineup_dt[player == "Hyun Jin Ryu", player := "Hyun-Jin Ryu"]
lineup_dt[player == "J.T. Riddle", player := "JT Riddle"]
lineup_dt[player == "Lance McCullers Jr.", player := "Lance McCullers"]
lineup_dt[player == "AJ Pollock", player := "A.J. Pollock"]

ab_count_2019 = unique(batter_result_frequency_dt[dataset == "MLB 2019 Regular Season",.(batter, Freq_total)])
ab_count_2019[, total_eligible_ab := sum(Freq_total), by = batter]
ab_count_2019 = unique(ab_count_2019[, .(batter, total_eligible_ab)])

lineup_dt = merge(lineup_dt,
                  ab_count_2019,
                  by.x = "player",
                  by.y = "batter",
                  all.x = T)

lineup_dt = lineup_dt[order(game_id, -home_away, lineup_spot)]
#lineup_dt[total_eligible_ab < 20 & position != "1", player := "REPLACEMENT BATTER"]
lineup_dt$total_eligible_ab = NULL


print(paste0(nrow(lineup_dt) / 20, " GAMES PULLED"))
