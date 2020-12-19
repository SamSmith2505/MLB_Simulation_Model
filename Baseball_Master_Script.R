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

game_iterations = 300
batter_result_weight = .4
pitcher_result_weight = .6

dk_score_stdev_weight = 1.5
dk_salary_constraint = 50000
draftkings_lineup_count = 20

start_time = Sys.time()

source("Baseball_Data_Prep.R")

source("Baseball_Pitch_Count_Bullpen_Prep.R")

# while(1==1){
#   skip = F
#   tryCatch({
#source("Baseball_Pull_Live_Lineups.R")

#fwrite(lineup_dt, "today.csv", row.names = F)
# lineup_dt = lineup_dt[!(game_id %in% c("630894", "630895")),]
lineup_dt = fread("daily_lineup_df.csv")
# #lineup_dt[player == "Hyun-Jin Ryu", player := "Hyun Jin Ryu"]
# #lineup_dt[player == "Yoshi Tsutsugo", player := "REPLACEMENT BATTER"]
# # # lineup_dt[player == "Joseph Odom", player := "REPLACEMENT BATTER"]
# # # lineup_dt[player == "Ryan Cordell", player := "REPLACEMENT BATTER"]
# lineup_dt[player == "_Ronald Acuna Jr.", player := "Ronald Acuna Jr."]
#lineup_dt[player == "Mike Brosseau", player := "Lourdes Gurriel"]
# # # # lineup_dt[player == "Trevor Rogers", player := "REPLACEMENT PITCHER"]
# lineup_dt[player == "Jose Urquidy", player := "REPLACEMENT PITCHER"]
#lineup_dt[player == "Lance McCullers", player := "Lance McCullers Jr."]

source("Baseball_Game_Simulation_Loop.R")

source("Baseball_Box_Score_Generator.R")
    
    # })
    #   if (skip) {
    #     next
    #   }
    # }

source("Baseball_Draftkings_Lineup_Optimizer.R")
# 
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

