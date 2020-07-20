season_2017 = fread("2017_MLB_PbP_Logs.csv")
colnames(season_2017) = c(
  "dataset",
  "game_id",
  "date",
  "inning",
  "road_score",
  "home_score",
  "batting_team",
  "batter",
  "batter_id",
  "batter_hand",
  "runner_on_first",
  "runner_on_second",
  "runner_on_third",
  "pitching_team",
  "pitcher",
  "pitcher_id",
  "pitcher_hand",
  "catcher",
  "first_base",
  "second_base",
  "third_base",
  "shortstop",
  "left_field",
  "center_field",
  "right_field",
  "number_of_pitches",
  "pitch_sequence",
  "ball_1",
  "ball_2",
  "ball_3",
  "ball_4",
  "strike_1",
  "strike_2",
  "strike_3",
  "hit_type",
  "play_type",
  "runs",
  "outs",
  "stolen_bases",
  "caught_stealing",
  "defensive_interference",
  "passed_ball",
  "wild_pitch",
  "description"
)

season_2018 = fread("2018_MLB_PbP_Logs.csv")

colnames(season_2018) = c(
  "dataset",
  "game_id",
  "date",
  "inning",
  "road_score",
  "home_score",
  "batting_team",
  "batter",
  "batter_id",
  "batter_hand",
  "runner_on_first",
  "runner_on_second",
  "runner_on_third",
  "pitching_team",
  "pitcher",
  "pitcher_id",
  "pitcher_hand",
  "catcher",
  "first_base",
  "second_base",
  "third_base",
  "shortstop",
  "left_field",
  "center_field",
  "right_field",
  "number_of_pitches",
  "pitch_sequence",
  "ball_1",
  "ball_2",
  "ball_3",
  "ball_4",
  "strike_1",
  "strike_2",
  "strike_3",
  "hit_type",
  "play_type",
  "runs",
  "outs",
  "stolen_bases",
  "caught_stealing",
  "defensive_interference",
  "passed_ball",
  "wild_pitch",
  "description"
)
season_2019 = fread("2019_MLB_PbP_Logs.csv")
season_2019$V45 = NULL
colnames(season_2019) = c(
  "dataset",
  "game_id",
  "date",
  "inning",
  "road_score",
  "home_score",
  "batting_team",
  "batter",
  "batter_id",
  "batter_hand",
  "runner_on_first",
  "runner_on_second",
  "runner_on_third",
  "pitching_team",
  "pitcher",
  "pitcher_id",
  "pitcher_hand",
  "catcher",
  "first_base",
  "second_base",
  "third_base",
  "shortstop",
  "left_field",
  "center_field",
  "right_field",
  "number_of_pitches",
  "pitch_sequence",
  "ball_1",
  "ball_2",
  "ball_3",
  "ball_4",
  "strike_1",
  "strike_2",
  "strike_3",
  "hit_type",
  "play_type",
  "runs",
  "outs",
  "stolen_bases",
  "caught_stealing",
  "defensive_interference",
  "passed_ball",
  "wild_pitch",
  "description"
)

mlb_dt = rbind(season_2017, season_2018, season_2019) %>% data.table()

base_stealing_dt = mlb_dt[, .(runner_on_first, stolen_bases, caught_stealing)]

mlb_dt = mlb_dt[play_type != "", ]
mlb_dt[play_type %in% c("SAC FLY"), play_type := "FLYOUT"]
mlb_dt[play_type %in% c("GROUNDED INTO DP", "SAC BUNT", "FIELDERS CHOICE", "DOUBLE PLAY"), play_type := "GROUNDOUT"]

rm(season_2017, season_2018, season_2019)

batter_result_frequency = mlb_dt[, .(dataset,
                                     batter,
                                     batter_hand,
                                     batter_id,
                                     pitcher_hand,
                                     play_type)] %>% group_by(dataset,
                                                                             batter,
                                                                             batter_hand,
                                                                             batter_id,
                                                                             pitcher_hand,
                                                                             play_type) %>%
  summarise(Freq = n())

pitcher_result_frequency = mlb_dt[, .(dataset,
                                      pitcher,
                                      pitcher_id,
                                      pitcher_hand,
                                      batter_hand,
                                      play_type)] %>% group_by(dataset,
                                                                              pitcher,
                                                                              pitcher_id,
                                                                              pitcher_hand,
                                                                              batter_hand,
                                                                              play_type) %>%
  summarise(Freq = n())

total_batter_frequency = mlb_dt[, .(dataset,
                                    batter,
                                    batter_hand,
                                    batter_id,
                                    pitcher_hand)] %>% group_by(dataset, batter, batter_hand, batter_id, pitcher_hand) %>%
  summarise(Freq_total = n())

total_pitcher_frequency = mlb_dt[, .(dataset,
                                     pitcher,
                                     pitcher_hand,
                                     pitcher_id,
                                     batter_hand)] %>% group_by(dataset, pitcher, pitcher_hand, pitcher_id, batter_hand) %>%
  summarise(Freq_total = n())

batter_result_frequency = merge(
  batter_result_frequency,
  total_batter_frequency,
  by = c("dataset", "batter", "batter_id", "pitcher_hand"),
  all.x = T
) %>% data.table()

pitcher_result_frequency = merge(
  pitcher_result_frequency,
  total_pitcher_frequency,
  by = c("dataset", "pitcher", "pitcher_id", "batter_hand"),
  all.x = T
) %>% data.table()

batter_result_frequency[, result_frequency := Freq / Freq_total]

pitcher_result_frequency[, result_frequency := Freq / Freq_total]

###NEW STUFF
# batter_result_frequency[, `:=`(Freq = as.numeric(Freq),
#                                Freq_total = as.numeric(Freq_total))]
# 
# pitcher_result_frequency[, `:=`(Freq = as.numeric(Freq),
#                                 Freq_total = as.numeric(Freq_total))]
# 
# batter_result_frequency = batter_result_frequency[Freq_total > 30, ]
# pitcher_result_frequency = pitcher_result_frequency[Freq_total > 30, ]
# 
# batter_result_frequency[season == 2017, `:=`(Freq = Freq * season_weight_2017,
#                                              Freq_total = Freq_total * season_weight_2017)]
# 
# batter_result_frequency[season == 2018, `:=`(Freq = Freq * season_weight_2018,
#                                              Freq_total = Freq_total * season_weight_2018)]
# 
# batter_result_frequency[season == 2019, `:=`(Freq = Freq * season_weight_2019,
#                                              Freq_total = Freq_total * season_weight_2019)]
# 
# batter_result_frequency = batter_result_frequency[, .(Freq = sum(Freq),
#                                                       Freq_total = sum(Freq_total)), by = .(batter,
#                                                                                             batter_id,
#                                                                                             pitcher_hand,
#                                                                                             season,
#                                                                                             batter_hand.x,
#                                                                                             play_type,
#                                                                                             batter_hand.y)]
# 
# batter_result_frequency[, result_frequency := Freq / Freq_total]
# 
# total_frequency = unique(batter_result_frequency[, .(batter, pitcher_hand, batter_hand.x, Freq_total, season)])
# total_frequency$season = NULL
# total_frequency = aggregate(. ~ batter + pitcher_hand + batter_hand.x, total_frequency, sum) %>% data.table()
# 
# batter_result_frequency = batter_result_frequency[, .(Freq = sum(Freq)), by = .(batter,
#                                                                                 batter_id,
#                                                                                 pitcher_hand,
#                                                                                 batter_hand.x,
#                                                                                 play_type,
#                                                                                 batter_hand.y)]
# 
# batter_result_frequency = merge(
#   batter_result_frequency,
#   total_frequency,
#   by = c("batter", "pitcher_hand", "batter_hand.x")
# )
# 
# batter_result_frequency[, result_frequency := Freq / Freq_total]
# 
# pitcher_result_frequency[season == 2017, `:=`(Freq = Freq * season_weight_2017,
#                                               Freq_total = Freq_total * season_weight_2017)]
# 
# pitcher_result_frequency[season == 2018, `:=`(Freq = Freq * season_weight_2018,
#                                               Freq_total = Freq_total * season_weight_2018)]
# 
# pitcher_result_frequency[season == 2019, `:=`(Freq = Freq * season_weight_2019,
#                                               Freq_total = Freq_total * season_weight_2019)]
# 
# pitcher_result_frequency = pitcher_result_frequency[, .(Freq = sum(Freq),
#                                                         Freq_total = sum(Freq_total)), by = .(pitcher,
#                                                                                               pitcher_id,
#                                                                                               batter_hand,
#                                                                                               season,
#                                                                                               pitcher_hand.x,
#                                                                                               play_type,
#                                                                                               pitcher_hand.y)]
# 
# pitcher_result_frequency[, result_frequency := Freq / Freq_total]
# 
# total_frequency = unique(pitcher_result_frequency[, .(pitcher, batter_hand, pitcher_hand.x, Freq_total, season)])
# total_frequency$season = NULL
# total_frequency = aggregate(. ~ pitcher + batter_hand + pitcher_hand.x, total_frequency, sum) %>% data.table()
# 
# pitcher_result_frequency = pitcher_result_frequency[, .(Freq = sum(Freq)), by = .(pitcher,
#                                                                                   pitcher_id,
#                                                                                   batter_hand,
#                                                                                   pitcher_hand.x,
#                                                                                   play_type,
#                                                                                   pitcher_hand.y)]
# 
# pitcher_result_frequency = merge(
#   pitcher_result_frequency,
#   total_frequency,
#   by = c("pitcher", "batter_hand", "pitcher_hand.x")
# )
# 
# pitcher_result_frequency[, result_frequency := Freq / Freq_total]

####
replacement_batter = aggregate(
  result_frequency ~ pitcher_hand + batter_hand.x + batter_hand.y + play_type,
  batter_result_frequency[batter_hand.x == "R",],
  mean
) %>% data.table()

replacement_pitcher = aggregate(
  result_frequency ~ pitcher_hand.x + batter_hand + pitcher_hand.y + play_type,
  pitcher_result_frequency[pitcher_hand.x == "R",],
  mean
) %>% data.table()

replacement_batter[, batter := "REPLACEMENT BATTER"]
replacement_pitcher[, pitcher := "REPLACEMENT PITCHER"]

replacement_batter = replacement_batter[, .(
  dataset = "MLB 2019 Regular Season",
  batter,
  batter_id = 4206969,
  pitcher_hand,
  batter_hand.x,
  play_type,
  Freq = 1000,
  batter_hand.y,
  Freq_total = 1000,
  result_frequency
)]

replacement_pitcher = replacement_pitcher[, .(
  dataset = "MLB 2019 Regular Season",
  pitcher,
  pitcher_id = 42069420,
  batter_hand,
  pitcher_hand.x,
  play_type,
  Freq = 1000,
  pitcher_hand.y,
  Freq_total = 1000,
  result_frequency
)]

batter_result_frequency = rbind(batter_result_frequency, replacement_batter)

pitcher_result_frequency = rbind(pitcher_result_frequency, replacement_pitcher)

home_team_dt = unique(mlb_dt[inning == "1T", .(game_id, pitching_team)])

colnames(home_team_dt) = c("game_id", "home_team")

mlb_dt = merge(mlb_dt, home_team_dt, by = "game_id")

home_field_results = mlb_dt[, .(home_team, pitcher_hand, batter_hand, play_type)] %>%
  group_by(home_team, pitcher_hand, batter_hand, play_type) %>% summarise(Freq_total = n())

total_home_field_frequency = mlb_dt[, .(home_team, pitcher_hand, batter_hand)] %>% group_by(home_team,
                                                                                            pitcher_hand,
                                                                                            batter_hand) %>% summarise(Freq_total = n())
colnames(total_home_field_frequency) = c("home_team", "pitcher_hand", "batter_hand", "total_frequency")

home_field_result_frequency = merge(
  home_field_results,
  total_home_field_frequency,
  by = c("home_team", "pitcher_hand", "batter_hand")
) %>% setDT()

home_field_result_frequency[, `:=`(
  park_result_frequency = Freq_total / total_frequency,
  Freq_total = NULL,
  total_frequency = NULL
)]

colnames(home_field_result_frequency) = c(
  "game_home_team",
  "pitcher_hand",
  "batter_hand",
  "play_type",
  "park_result_frequency"
)

full_freq = home_field_result_frequency[, .(
  total_result_frequency = mean(park_result_frequency)
), by = .(pitcher_hand, batter_hand, play_type)]

home_field_result_frequency = merge(home_field_result_frequency,
                                    full_freq,
                                    by = c("pitcher_hand",
                                           "batter_hand",
                                           "play_type"))

home_field_result_frequency[, park_adjustment := (park_result_frequency - total_result_frequency) + 1]

home_field_result_frequency = home_field_result_frequency[, .(
  pitcher_hand,
  batter_hand,
  play_type,
  game_home_team,
  park_adjustment
)]

base_stealing_dt[is.na(stolen_bases), stolen_bases := 0]
base_stealing_dt[is.na(caught_stealing), caught_stealing := 0]

base_stealing_dt = base_stealing_dt[, .(
  runner_on_first,
  stolen_bases_attempt = mean(stolen_bases) + mean(caught_stealing),
  caught_stealing = (sum(caught_stealing) / (sum(caught_stealing) + sum(stolen_bases)))
), by = runner_on_first]
base_stealing_dt = base_stealing_dt[, 2:4]
colnames(base_stealing_dt) = c('runner_on_first',
                               'stolen_base_attempt_odds',
                               'stolen_base_success_odds')
base_stealing_dt[is.na(stolen_base_success_odds), stolen_base_success_odds := 0]

base_stealing_dt = rbind(base_stealing_dt, list("REPLACEMENT BATTER", 0, 0))
base_stealing_dt = rbind(base_stealing_dt, list("REPLACEMENT PITCHER", 0, 0))

full_day_dt = data.table()

daily_win_dt = data.table(
  game_id = integer(),
  home_team = character(),
  away_team = character(),
  home_wins = integer(),
  away_wins = integer()
)

daily_matchup_dt = data.table()
day_stolen_base_dt = data.table(runner_on_first = character(),
                                game_number = integer())
day_rbi_dt = data.table(batter = character(),
                        rbi = integer(),
                        game_number = integer())
day_run_dt = data.table(scorer = character(),
                        game_number = integer())


