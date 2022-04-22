library(data.table)
library(dplyr)
library(zoo)
library(lubridate)
library(caret)
library(pROC)
library(yardstick)
library(openxlsx)

setwd("~/Documents/Github/MLB_Simulation_Model")

output_filename = paste0("MLB_Simulation_Results_", today(), ".xlsx")

game_iterations = 200
batter_result_weight = .45
pitcher_result_weight = .55

dk_score_stdev_weight = 1.5
dk_salary_constraint = 50000
draftkings_lineup_count = 20

season_weight_2020 = .2
season_weight_2021 = .8

start_time = Sys.time()

source("Baseball_Data_Prep.R")

source("Baseball_Pitch_Count_Bullpen_Prep.R")

source("Baseball_Pull_Live_Lineups.R")
#lineup_dt = lineup_dt[!(game_id %in% c(662122,662570,662934)),]
lineup_dt[player == "Yoshi Tsutsugo", player := "REPLACEMENT BATTER"]
lineup_dt[player == "Mike Brosseau", player := "REPLACEMENT BATTER"]
lineup_dt[player == "Adrian Sanchez", player := "REPLACEMENT BATTER"]
lineup_dt[player == "Jose Barrero", player := "REPLACEMENT BATTER"]
lineup_dt[player == "Alfonso Rivas", player := "REPLACEMENT BATTER"]
#lineup_dt[player == "Noah Syndergaard", player := "REPLACEMENT PITCHER"]
lineup_dt[player == "Drew Ellis", player := "REPLACEMENT BATTER"]
lineup_dt[player == "Shohei Ohtani" & position == 'Y', position := 1]
#lineup_dt = fread('daily_lineup_df.csv')
#lineup_dt[player == "Jose Herrera", player := "REPLACEMENT BATTER"]
source("Baseball_Game_Simulation_Loop.R")

source("Baseball_Box_Score_Generator.R")

#source("MLB_Bet_Creation.R")

#fwrite(bet_print_dt, "today_bets.csv", row.names = F)
    
    # })
    #   if (skip) {
    #     next
    #   }
    # }

source("Baseball_Draftkings_Lineup_Optimizer.R")

source("Baseball_Random_Functions.R")

output_list = list(
  "Game Results" = box_daily_win_dt,
  "Hitter Box Score" = box_score,
  "Pitcher Box Score" = pitcher_box_score,
  #"Draftkings Lineup" = optimal_lineup,
  "Full DK Hitter Projections" = dk_score_table_hitter,
  "Full DK Pitcher Projections" = dk_score_table_pitcher
)
write.xlsx(output_list, file = output_filename)

fwrite(box_daily_win_dt, "win_dt_today.csv", row.names = F)
# })
#   if (skip) {
#     next
#   }
# }
end_time = Sys.time()

runtime = end_time - start_time

graph_results("Cleveland Indians")

remove_game(removal_game_id = 6)

