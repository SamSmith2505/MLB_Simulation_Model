team_assignment = merge(full_day_dt,
                        lineup_dt[, .(player, team, game_id)],
                        by.x = "batter",
                        by.y = "player") %>% unique()

team_scores = aggregate(runs_generated ~ team + game_number + game_id + home_away,
                        team_assignment,
                        sum) %>% data.table()
team_mean_scores = aggregate(runs_generated ~ team, team_scores, mean) %>% data.table()

compiled_game_table = data.table()
for (id in unique(team_scores$game_id)) {
  print(paste0("COMPILING GAME ", id))
  game_calc_dt = team_scores[game_id == id,]
  home_team = unique(game_calc_dt[home_away == "H", team])
  away_team = unique(game_calc_dt[home_away == "A", team])
  for (iter in 1:game_iterations) {
    relevant_dt = game_calc_dt[game_number == iter,]
    home_team_dt = relevant_dt[team == home_team, ]
    away_team_dt = relevant_dt[team == away_team, ]
    temp_dt = data.table(
      home_team = home_team,
      away_team = away_team,
      home_runs_generated = home_team_dt[, runs_generated],
      away_runs_generated = away_team_dt[, runs_generated]
    )
    compiled_game_table = rbind(compiled_game_table, temp_dt)
  }
}

compiled_game_table = data.table(compiled_game_table)

compiled_game_table[, `:=`(
  did_home_team_cover_plus = ifelse(home_runs_generated - away_runs_generated >= -1, 1, 0),
  did_home_team_cover_minus = ifelse(home_runs_generated - away_runs_generated >= 2, 1, 0),
  did_away_team_cover_plus = ifelse(home_runs_generated - away_runs_generated <= 1, 1, 0),
  did_away_team_cover_minus = ifelse(home_runs_generated - away_runs_generated <= -2, 1, 0)
)]

compiled_game_table = aggregate(. ~ home_team + away_team, compiled_game_table, mean)

daily_win_dt = merge(daily_win_dt,
                     team_mean_scores,
                     by.x = "home_team",
                     by.y = "team")
daily_win_dt = merge(daily_win_dt,
                     team_mean_scores,
                     by.x = "away_team",
                     by.y = "team")

daily_win_dt = daily_win_dt[, .(
  home_team,
  away_team,
  home_team_win_pct = home_wins / game_iterations,
  away_team_win_pct = away_wins / game_iterations,
  home_team_runs = runs_generated.x,
  away_team_runs = runs_generated.y
)]

compiled_game_table = data.table(compiled_game_table)
daily_win_dt = merge(daily_win_dt,
                     compiled_game_table[, .(
                       home_team,
                       away_team,
                       did_home_team_cover_plus,
                       did_home_team_cover_minus,
                       did_away_team_cover_plus,
                       did_away_team_cover_minus
                     )],
                     by = c("home_team", "away_team"))

daily_win_dt[,`:=`(
  home_team_win_odds = round(ifelse(home_team_win_pct / (1 - home_team_win_pct) > 1, 
                              - (home_team_win_pct / (1 - home_team_win_pct)) * 100,
                              ((1 - home_team_win_pct) / home_team_win_pct) * 100)),
  away_team_win_odds = round(ifelse(away_team_win_pct / (1 - away_team_win_pct) > 1, 
                              - (away_team_win_pct / (1 - away_team_win_pct)) * 100,
                              ((1 - away_team_win_pct) / away_team_win_pct) * 100)),
  home_team_cover_plus_odds = round(ifelse(did_home_team_cover_plus / (1 - did_home_team_cover_plus) > 1, 
                              - (did_home_team_cover_plus / (1 - did_home_team_cover_plus)) * 100,
                              ((1 - did_home_team_cover_plus) / did_home_team_cover_plus) * 100)),
  home_team_cover_minus_odds = round(ifelse(did_home_team_cover_minus / (1 - did_home_team_cover_minus) > 1, 
                                     - (did_home_team_cover_minus / (1 - did_home_team_cover_minus)) * 100,
                                     ((1 - did_home_team_cover_minus) / did_home_team_cover_minus) * 100)),
  away_team_cover_plus_odds = round(ifelse(did_away_team_cover_plus / (1 - did_away_team_cover_plus) > 1, 
                                     - (did_away_team_cover_plus / (1 - did_away_team_cover_plus)) * 100,
                                     ((1 - did_away_team_cover_plus) / did_away_team_cover_plus) * 100)),
  away_team_cover_minus_odds = round(ifelse(did_away_team_cover_minus / (1 - did_away_team_cover_minus) > 1, 
                                      - (did_away_team_cover_minus / (1 - did_away_team_cover_minus)) * 100,
                                      ((1 - did_away_team_cover_minus) / did_away_team_cover_minus) * 100))
)]

daily_win_dt = daily_win_dt[, .(
  home_team,
  away_team,
  home_team_runs,
  away_team_runs,
  home_team_win_pct,
  away_team_win_pct,
  home_team_win_odds,
  away_team_win_odds,
  home_team_cover_plus_odds,
  home_team_cover_minus_odds,
  away_team_cover_plus_odds,
  away_team_cover_minus_odds
)]

full_day_dt = unique(full_day_dt)

batter_result_dt = table(full_day_dt$batter, full_day_dt$play_type) %>% data.table()
colnames(batter_result_dt) = c("batter",
                               "result",
                               "frequency")

at_bat_counter = aggregate(frequency ~ batter, batter_result_dt, sum) %>% data.table()

batter_result_dt[, is_result_hit := ifelse(result %in% c("SINGLE", "DOUBLE", "TRIPLE", "HOME RUN"),
                                           1,
                                           0)]

batter_result_dt[, is_result_on_base := ifelse(result %in% c("SINGLE", "DOUBLE", "TRIPLE", "HOME RUN", "WALK", "INTENT WALK"),
                                               1,
                                               0)]

batter_result_dt[, result_slugging_contribution := ifelse(result == "SINGLE",
                                                          1,
                                                          ifelse(result == "DOUBLE",
                                                                 2,
                                                                 ifelse(
                                                                   result == "TRIPLE",
                                                                   3,
                                                                   ifelse(result == "HOME RUN",
                                                                          4,
                                                                          0)
                                                                 )))]

batter_result_dt[, was_home_run := ifelse(result == "HOME RUN",
                                          1,
                                          0)]

batter_result_dt[, ab := sum(frequency), by = batter]

batter_result_dt[, relative_result_frequency := frequency / ab]

batter_result_dt[, `:=`(
  hit_contribution = is_result_hit * relative_result_frequency,
  obp_contribution = is_result_on_base * relative_result_frequency,
  slg_contribution = result_slugging_contribution * relative_result_frequency
)]


box_score <- batter_result_dt[, list(
  avg = sum(hit_contribution),
  obp = sum(obp_contribution),
  slg = sum(slg_contribution),
  hr = sum(was_home_run * frequency) / game_iterations
),
by = batter]

pitching_box_score = full_day_dt[, .(
  pitcher = paste(pitcher, " ", home_away),
  play_type,
  outs_created,
  runs_generated,
  is_hit = ifelse(play_type %in% c("SINGLE", "DOUBLE", "TRIPLE", "HOME RUN"), 1, 0),
  is_whip_contribution = ifelse(
    play_type %in% c("SINGLE", "DOUBLE", "TRIPLE", "HOME RUN", "WALK", "HIT BY PITCH"),
    1,
    0
  ),
  is_strikeout = ifelse(play_type == "STRIKEOUT", 1, 0),
  game_number
)]

k_counter = pitching_box_score[play_type == "STRIKEOUT", pitcher]
k_counter = table(k_counter) %>% data.table()
colnames(k_counter) = c("pitcher", "k")

innings_pitched_counter = pitching_box_score[, .(pitcher, outs_created)]
innings_pitched_counter = aggregate(outs_created ~ pitcher, innings_pitched_counter, sum) %>% data.table()
innings_pitched_counter[, innings_pitched := outs_created / 3]

runs_allowed_counter = aggregate(runs_generated ~ pitcher, pitching_box_score, sum)
hits_allowed_counter = aggregate(is_hit ~ pitcher, pitching_box_score, sum)
whip_contribution_counter = aggregate(is_whip_contribution ~ pitcher, pitching_box_score, sum)

pitcher_box_score = merge(innings_pitched_counter, k_counter, by = "pitcher")
pitcher_box_score = merge(pitcher_box_score, runs_allowed_counter, by = "pitcher")
pitcher_box_score = merge(pitcher_box_score, hits_allowed_counter, by = "pitcher")
pitcher_box_score = merge(pitcher_box_score, whip_contribution_counter, by = "pitcher")

pitcher_box_score[, `:=`(
  whip = is_whip_contribution / innings_pitched,
  era = (runs_generated / innings_pitched) * 9,
  innings_pitched = innings_pitched / game_iterations,
  k = k / game_iterations,
  outs_created = NULL,
  runs_generated = NULL,
  is_hit = NULL,
  is_whip_contribution = NULL
)]

singles_dt = full_day_dt[play_type == "SINGLE", ]
doubles_dt = full_day_dt[play_type == "DOUBLE", ]
triples_dt = full_day_dt[play_type == "TRIPLE", ]
hr_dt = full_day_dt[play_type == "HOME RUN", ]
walks_dt = full_day_dt[play_type %in% c("WALK", "INTENT WALK", "HIT BY PITCH"), ]

singles_dt[, iteration_singles := .N, by = .(game_number, batter)]
doubles_dt[, iteration_doubles := .N, by = .(game_number, batter)]
triples_dt[, iteration_triples := .N, by = .(game_number, batter)]
walks_dt[, iteration_walks := .N, by = .(game_number, batter)]
hr_dt[, iteration_hrs := .N, by = .(game_number, batter)]

singles_dt = singles_dt[order(batter, game_number), ]
doubles_dt = doubles_dt[order(batter, game_number), ]
triples_dt = triples_dt[order(batter, game_number), ]
walks_dt = walks_dt[order(batter, game_number), ]
hr_dt = hr_dt[order(batter, game_number), ]

singles_dt = unique(singles_dt[, .(batter, game_number, iteration_singles)])
doubles_dt = unique(doubles_dt[, .(batter, game_number, iteration_doubles)])
triples_dt = unique(triples_dt[, .(batter, game_number, iteration_triples)])
walks_dt = unique(walks_dt[, .(batter, game_number, iteration_walks)])
hr_dt = unique(hr_dt[, .(batter, game_number, iteration_hrs)])

day_rbi_dt = aggregate(rbi ~ ., day_rbi_dt, sum) %>% data.table()

day_run_dt[!is.na(scorer), runs_scored := 1]
day_run_dt[is.na(runs_scored), runs_scored := 0]
day_run_dt = aggregate(runs_scored ~ ., day_run_dt, sum) %>% data.table()

day_stolen_base_dt[, stolen_bases := 1]
day_stolen_base_dt = aggregate(stolen_bases ~ ., day_stolen_base_dt, sum) %>% data.table()

day_rbi_dt = day_rbi_dt[order(batter, game_number), ]
day_run_dt = day_run_dt[order(batter, game_number), ]
day_stolen_base_dt = day_stolen_base_dt[order(batter, game_number), ]

fantasy_score_base = unique(full_day_dt[, .(batter, game_number)])
fantasy_score_base = merge(
  fantasy_score_base,
  singles_dt,
  by = c("batter", "game_number"),
  all.x = T
)
fantasy_score_base = merge(
  fantasy_score_base,
  doubles_dt,
  by = c("batter", "game_number"),
  all.x = T
)
fantasy_score_base = merge(
  fantasy_score_base,
  triples_dt,
  by = c("batter", "game_number"),
  all.x = T
)
fantasy_score_base = merge(
  fantasy_score_base,
  walks_dt,
  by = c("batter", "game_number"),
  all.x = T
)
fantasy_score_base = merge(
  fantasy_score_base,
  hr_dt,
  by = c("batter", "game_number"),
  all.x = T
)
fantasy_score_base = merge(
  fantasy_score_base,
  day_rbi_dt,
  by = c("batter", "game_number"),
  all.x = T
)
fantasy_score_base = merge(
  fantasy_score_base,
  day_run_dt,
  by.x = c("batter", "game_number"),
  by.y = c("scorer", "game_number"),
  all.x = T
)
fantasy_score_base = merge(
  fantasy_score_base,
  day_stolen_base_dt,
  by.x = c("batter", "game_number"),
  by.y = c("runner_on_first", "game_number"),
  all.x = T
)

fantasy_score_base[is.na(fantasy_score_base)] = 0

fantasy_score_base[, points := iteration_singles * 3 + iteration_doubles * 5 + iteration_triples * 8 + iteration_walks * 2 + iteration_hrs * 10 + rbi * 2 + stolen_bases * 5 + runs_scored * 2]

dk_score_table_hitter = fantasy_score_base[, .(mean_points = mean(points),
                                               sd_points = sd(points)), by = batter]

dk_score_table_hitter[, upside_expected_score := mean_points + dk_score_stdev_weight * sd_points]

pitching_box_score[, points_generated := (outs_created * .75) + (runs_generated * -2) + (is_whip_contribution * -.6) + (is_strikeout * 2)]

pitcher_results = aggregate(points_generated ~ pitcher + game_number,
                            pitching_box_score,
                            sum) %>% data.table()

dk_score_table_pitcher = pitcher_results[, .(
  mean_points = mean(points_generated),
  sd_points = sd(points_generated)
), by = pitcher]

dk_score_table_pitcher[, upside_expected_score := mean_points + dk_score_stdev_weight * sd_points]

rbi_dt = aggregate(rbi ~ batter, fantasy_score_base, sum)
run_dt = aggregate(runs_scored ~ batter, fantasy_score_base, sum)
steal_dt = aggregate(stolen_bases ~ batter, fantasy_score_base, sum)

box_score = merge(box_score, rbi_dt, by = "batter")
box_score = merge(box_score, run_dt, by = "batter")
box_score = merge(box_score, steal_dt, by = "batter")

box_score[, `:=`(
  rbi = rbi / game_iterations,
  runs_scored = runs_scored / game_iterations,
  stolen_bases = stolen_bases / game_iterations
)]

View(dk_score_table_hitter)
View(dk_score_table_pitcher)
View(daily_win_dt)
