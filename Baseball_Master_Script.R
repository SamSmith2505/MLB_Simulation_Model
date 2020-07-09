library(data.table)
library(dplyr)
library(zoo)
library(lubridate)
library(caret)
library(pROC)
library(yardstick)
library(openxlsx)

setwd("~/Desktop/MLB Model")

output_filename = paste0("MLB_Simulation_Results_", today(), ".xlsx")

game_iterations = 500
batter_result_weight = .3
pitcher_result_weight = .6
park_result_weight = .1

dk_score_stdev_weight = 1.6
dk_salary_constraint = 50000

start_time = Sys.time()

source("Baseball_Data_Prep.R")

source("Baseball_Pitch_Count_Bullpen_Prep.R")

source("Baseball_Pull_Live_Lineups.R")

source("Baseball_Game_Simulation_Loop.R")

source("Baseball_Box_Score_Generator.R")

source("Baseball_Draftkings_Lineup_Optimizer.R")

source("Baseball_Graph_Functions.R")

output_list = list(
  "Game Results" = daily_win_dt,
  "Hitter Box Score" = box_score,
  "Pitcher Box Score" = pitcher_box_score,
  "Draftkings Lineup" = optimal_lineup,
  "Game Margin Distribution" = win_margin_distribution,
  "Game Total Runs Scored" = total_run_distribution
)
write.xlsx(output_list, file = output_filename)

end_time = Sys.time()

runtime = end_time - start_time

graph_results("Los Angeles Angels")
