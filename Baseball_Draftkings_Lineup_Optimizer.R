library(dummies)
library(lpSolve)

dk_players = rbind(dk_score_table_hitter[, .(player = batter,
                                             mean_points,
                                             sd_points,
                                             upside_expected_score)], dk_score_table_pitcher[!(pitcher %in% c("BULLPEN   H", "BULLPEN   A")), .(player = pitcher,
                                                                                                                                                mean_points,
                                                                                                                                                sd_points,
                                                                                                                                                upside_expected_score)])

dk_players = merge(dk_players,
                   full_lineup_dt[, .(player, position)],
                   by = "player",
                   all.x = T)

dk_players[is.na(position), position := 1]

dk_salaries = fread("DKSalaries.csv")

dk_salaries = dk_salaries[!(Name == "Luis Garcia"),]

dk_players[position == 1, player := substr(player, 0, nchar(player) - 4)]

dk_players = merge(dk_players,
                   dk_salaries[, .(player = Name,
                                   position = `Roster Position`,
                                   dk_salary = Salary)], by = "player")

dk_players = dk_players[, .(player,
                            mean_points,
                            sd_points,
                            upside_expected_score,
                            position = position.y,
                            dk_salary)]

dk_players[position == "P", position_mat := "P"]
dk_players[position %in% c("C", "C/1B", "C/OF"), position_mat := "C"]
dk_players[position %in% c("1B", "1B/OF"), position_mat := "1B"]
dk_players[position %in% c("2B", "2B/OF", "2B/3B", "1B/2B"), position_mat := "2B"]
dk_players[position %in% c("3B", "1B/3B", "3B/OF"), position_mat := "3B"]
dk_players[position %in% c("SS", "2B/SS", "SS/OF", "3B/SS"), position_mat := "SS"]
dk_players[position == "OF", position_mat := "OF"]
dk_players[position == "1B/SS", position_mat := "SS"]

print(paste0(nrow(dk_players[is.na(position_mat)]), " Players Excluded Due To Position Error"))

position_matrix = dummy(dk_players[, position_mat]) %>% data.table()
colnames(position_matrix) = c("1b",
                              "2b",
                              "3b",
                              "c",
                              "of",
                              "p",
                              "ss")
optimal_lineups = data.table()
for (i in 1:draftkings_lineup_count) {
  position_matrix$salary = dk_players$dk_salary
  
  position_matrix$upside_projection = dk_players$upside_expected_score
  
  position_matrix$player = dk_players$player
  
  position_matrix = position_matrix[, 1:8]
  
  lp_obj = dk_players$upside_expected_score
  
  lp_dir = base::rep(0, ncol(position_matrix))
  
  lp_con = t(position_matrix)
  
  lp_rhs = rep(0, ncol(position_matrix))
  
  lp_rhs[1:4] = 1
  
  lp_rhs[5] = 3
  
  lp_rhs[6] = 2
  
  lp_rhs[7] = 1
  
  lp_rhs[8] = dk_salary_constraint
  
  lp_dir[1:7] = "="
  
  lp_dir[8] = "<="
  
  lp_model = lp("max", lp_obj, lp_con, lp_dir, lp_rhs, all.bin = T)
  
  dk_players$is_player_in_lineup = lp_model$solution
  
  optimal_lineup = dk_players[is_player_in_lineup == 1,]
  
  dk_players[is_player_in_lineup == 1, mean_points := mean_points * .9]
  
  dk_players[, upside_expected_score := mean_points + (dk_score_stdev_weight * sd_points)]
  
  optimal_lineup$lineup_id = i
  
  optimal_lineups = rbind(optimal_lineups, optimal_lineup)
  
  dk_players[, is_player_in_lineup := 0]
}

View(optimal_lineups)

submission_sheet = optimal_lineups[, .(player,
                                       position_mat,
                                       lineup_id)]

submission_sheet = merge(submission_sheet,
                         dk_salaries[, .(player = Name,
                                         id = `Name + ID`)],
                         by = "player")

submission_sheet = submission_sheet[order(lineup_id)]

final_submission_sheet = data.table(
  P = character(draftkings_lineup_count),
  P = character(draftkings_lineup_count),
  C = character(draftkings_lineup_count),
  `1B` = character(draftkings_lineup_count),
  `2B` = character(draftkings_lineup_count),
  `3B` = character(draftkings_lineup_count),
  SS = character(draftkings_lineup_count),
  OF = character(draftkings_lineup_count),
  OF = character(draftkings_lineup_count),
  OF = character(draftkings_lineup_count)
)

for (i in unique(submission_sheet$lineup_id)) {
  temp_submission = submission_sheet[lineup_id == i,]
  temp_submission = temp_submission[order(position_mat, player)]
  
  final_submission_sheet[i, 1] = temp_submission[8,id]
  final_submission_sheet[i, 2] = temp_submission[9,id]
  final_submission_sheet[i, 3] = temp_submission[4,id]
  final_submission_sheet[i, 4] = temp_submission[1,id]
  final_submission_sheet[i, 5] = temp_submission[2,id]
  final_submission_sheet[i, 6] = temp_submission[3,id]
  final_submission_sheet[i, 7] = temp_submission[10,id]
  final_submission_sheet[i, 8] = temp_submission[5,id]
  final_submission_sheet[i, 9] = temp_submission[6,id]
  final_submission_sheet[i, 10] = temp_submission[7,id]
}

player_exposure = table(submission_sheet$player) %>% data.table()

colnames(player_exposure) = c("player",
                              "lineup_count")

player_exposure = player_exposure[order(-lineup_count)]

View(player_exposure)

final_submission_sheet = unique(final_submission_sheet)

fwrite(final_submission_sheet, paste0("dk_lineups_", today(), ".csv"), row.names = F)

best_lineup = optimal_lineups[lineup_id == 1, .(
  player,
  projected_points = mean_points,
  position,
  draftkings_salary = dk_salary
)]

View(best_lineup)
