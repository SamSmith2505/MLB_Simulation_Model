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
                   lineup_dt[, .(player, position)],
                   by = "player",
                   all.x = T)

dk_players[is.na(position), position := 1]

##TO BE CHANGED ONCE WE HAVE DK SALARIES
dk_players[, dk_salary := runif(nrow(dk_players), 0, 1) * 10000]

dk_players[position == 1, position_mat := "P"]
dk_players[position == 2, position_mat := "C"]
dk_players[position == 3, position_mat := "1B"]
dk_players[position == 4, position_mat := "2B"]
dk_players[position == 5, position_mat := "3B"]
dk_players[position == 6, position_mat := "SS"]
dk_players[position == 10, position_mat := "DH"]
dk_players[is.na(position_mat), position_mat := "OF"]

position_matrix = dummy(dk_players[, position_mat]) %>% data.table()
colnames(position_matrix) = c(
  "1b",
  "2b",
  "3b",
  "c",
  "dh",
  "of",
  "p",
  "ss"
)

position_matrix$salary = dk_players$dk_salary

position_matrix$upside_projection = dk_players$upside_expected_score

position_matrix$player = dk_players$player

position_matrix = position_matrix[,1:9]

lp_obj = dk_players$upside_expected_score

lp_dir = base::rep(0, ncol(position_matrix))

lp_con = t(position_matrix)

lp_rhs = rep(0, ncol(position_matrix))

lp_rhs[1:4] = 1

lp_rhs[5] = 0

lp_rhs[6] = 3

lp_rhs[7] = 2

lp_rhs[8] = 1

lp_rhs[9] = dk_salary_constraint

lp_dir[1:8] = "="

lp_dir[9] = "<="

lp_model = lp("max", lp_obj, lp_con, lp_dir, lp_rhs, all.bin = T)

dk_players$is_player_in_lineup = lp_model$solution

optimal_lineup = dk_players[is_player_in_lineup == 1,]

View(optimal_lineup)
