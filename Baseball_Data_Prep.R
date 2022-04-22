library(readxl)

season_2020 = read_excel("2020_MLB_PbP_Logs.xlsx") %>% setDT()

colnames(season_2020) = c(
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

season_2020[, `:=`(
  date = as.character(date),
  road_score = as.integer(road_score),
  home_score = as.integer(home_score),
  batter_id = as.numeric(batter_id),
  pitcher_id = as.integer(pitcher_id),
  number_of_pitches = as.integer(number_of_pitches),
  runs = as.integer(runs),
  outs = as.integer(outs),
  stolen_bases = as.integer(stolen_bases),
  caught_stealing = as.integer(caught_stealing),
  defensive_interference = as.integer(defensive_interference),
  passed_ball = as.integer(passed_ball),
  wild_pitch = as.integer(wild_pitch)
)]

season_2021 = read_excel("2021_MLB_PbP_Logs.xlsx") %>% setDT()

colnames(season_2021) = c(
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

season_2021[, `:=`(
  date = as.character(date),
  road_score = as.integer(road_score),
  home_score = as.integer(home_score),
  batter_id = as.numeric(batter_id),
  pitcher_id = as.integer(pitcher_id),
  number_of_pitches = as.integer(number_of_pitches),
  runs = as.integer(runs),
  outs = as.integer(outs),
  stolen_bases = as.integer(stolen_bases),
  caught_stealing = as.integer(caught_stealing),
  defensive_interference = as.integer(defensive_interference),
  passed_ball = as.integer(passed_ball),
  wild_pitch = as.integer(wild_pitch)
)]

newest_file = paste0("~/Dropbox/22-mlb-pbp/",format.Date(today() - days(2), "%m"), "-", format.Date(today() - days(2), "%d"), "-", year(today()), "-mlb-season-pbp-feed.xlsx")

season_2022 = read_excel(newest_file) %>% setDT()

season_2022 = season_2022[2:nrow(season_2022),1:44]

colnames(season_2022) = c(
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

season_2022[, `:=`(
  date = as.character(date),
  road_score = as.integer(road_score),
  home_score = as.integer(home_score),
  batter_id = as.numeric(batter_id),
  pitcher_id = as.integer(pitcher_id),
  number_of_pitches = as.integer(number_of_pitches),
  runs = as.integer(runs),
  outs = as.integer(outs),
  stolen_bases = as.integer(stolen_bases),
  caught_stealing = as.integer(caught_stealing),
  defensive_interference = as.integer(defensive_interference),
  passed_ball = as.integer(passed_ball),
  wild_pitch = as.integer(wild_pitch)
)]
season_2022[, dataset := 'MLB 2021 Regular Season']

mlb_dt = rbind(season_2020, season_2021, season_2022) %>% data.table()

mlb_dt = mlb_dt[dataset %in% c("MLB 2020 Regular Season", "MLB 2021 Regular Season"),]
mlb_dt[batter == 'Jackie Bradley', batter := 'Jackie Bradley Jr.']
mlb_dt[batter == "Hyun-Jin Ryu", batter := "Hyun Jin Ryu"]
mlb_dt[batter == "AJ Pollock", batter := "A.J. Pollock"]
mlb_dt[pitcher == "Lance McCullers", pitcher := "Lance McCullers Jr."]
mlb_dt[batting_team == 'Cleveland Indians', batting_team := 'Cleveland Guardians']
mlb_dt[pitching_team == 'Cleveland Indians', pitching_team := 'Cleveland Guardians']
base_stealing_dt = mlb_dt[, .(runner_on_first, stolen_bases, caught_stealing)]

mlb_dt = mlb_dt[play_type != "", ]
mlb_dt[play_type %in% c("SAC FLY"), play_type := "FLYOUT"]
mlb_dt[play_type %in% c("GROUNDED INTO DP", "SAC BUNT", "FIELDERS CHOICE", "DOUBLE PLAY"), play_type := "GROUNDOUT"]

rm(season_2020, season_2021, season_2022)

mlb_dt[batter_hand %in% c('B', 'S') &
         pitcher_hand == 'R', batter_hand := 'L']
mlb_dt[batter_hand %in% c('B', 'S') &
         pitcher_hand == 'L', batter_hand := 'R']

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

batter_result_frequency[, `:=`(Freq = as.numeric(Freq),
                               Freq_total = as.numeric(Freq_total))]

pitcher_result_frequency[, `:=`(Freq = as.numeric(Freq),
                                Freq_total = as.numeric(Freq_total))]

batters_2020 = unique(batter_result_frequency[dataset == 'MLB 2020 Regular Season',batter])
batters_2021 = unique(batter_result_frequency[dataset == 'MLB 2021 Regular Season',batter])

batter_result_frequency[dataset == 'MLB 2020 Regular Season' & batter %in% batters_2021, `:=`(Freq = Freq * season_weight_2020,
                                                                   Freq_total = Freq_total * season_weight_2020)]

batter_result_frequency[dataset == 'MLB 2021 Regular Season' & batter %in% batters_2020, `:=`(Freq = Freq * season_weight_2021,
                                                                   Freq_total = Freq_total * season_weight_2021)]

frequency_calc = unique(batter_result_frequency[, .(dataset,
                                                    batter,
                                                    pitcher_hand,
                                                    batter_hand.x,
                                                    batter_hand.y,
                                                    Freq_total)])

frequency_calc[, Freq_total := sum(Freq_total), by = .(batter, pitcher_hand, batter_hand.x, batter_hand.y)]

frequency_calc$dataset = NULL

frequency_calc = unique(frequency_calc)

batter_result_frequency$Freq_total = NULL

batter_result_frequency = merge(
  batter_result_frequency,
  frequency_calc,
  by = c("batter", "pitcher_hand", "batter_hand.x", "batter_hand.y")
)

batter_result_frequency = batter_result_frequency[, .(Freq = sum(Freq)), by = .(batter,
                                                                                batter_id,
                                                                                pitcher_hand,
                                                                                batter_hand.x,
                                                                                play_type,
                                                                                batter_hand.y,
                                                                                Freq_total)]

batter_result_frequency[, result_frequency := Freq / Freq_total]

pitchers_2020 = unique(pitcher_result_frequency[dataset == 'MLB 2020 Regular Season',pitcher])
pitchers_2021 = unique(pitcher_result_frequency[dataset == 'MLB 2021 Regular Season',pitcher])

pitcher_result_frequency[dataset == 'MLB 2020 Regular Season', `:=`(Freq = Freq * season_weight_2020,
                                                                   Freq_total = Freq_total * season_weight_2020)]
pitcher_result_frequency[dataset == 'MLB 2021 Regular Season', `:=`(Freq = Freq * season_weight_2021,
                                                                   Freq_total = Freq_total * season_weight_2021)]

frequency_calc = unique(pitcher_result_frequency[, .(dataset,
                                                    pitcher,
                                                    pitcher_hand.x,
                                                    batter_hand,
                                                    pitcher_hand.y,
                                                    Freq_total)])

frequency_calc[, Freq_total := sum(Freq_total), by = .(pitcher, batter_hand, pitcher_hand.x, pitcher_hand.y)]

frequency_calc$dataset = NULL

frequency_calc = unique(frequency_calc)

pitcher_result_frequency$Freq_total = NULL

pitcher_result_frequency = merge(
  pitcher_result_frequency,
  frequency_calc,
  by = c("pitcher", "batter_hand", "pitcher_hand.x", "pitcher_hand.y")
)

pitcher_result_frequency = pitcher_result_frequency[, .(Freq = sum(Freq)), by = .(pitcher,
                                                                                pitcher_id,
                                                                                batter_hand,
                                                                                pitcher_hand.x,
                                                                                play_type,
                                                                                pitcher_hand.y,
                                                                                Freq_total)]

pitcher_result_frequency[, result_frequency := Freq / Freq_total]

batter_result_frequency = batter_result_frequency[, .(
  batter,
  batter_id,
  pitcher_hand,
  batter_hand.x,
  play_type,
  Freq,
  batter_hand.y,
  Freq_total,
  result_frequency
)]

pitcher_result_frequency = pitcher_result_frequency[, .(
  pitcher,
  pitcher_id,
  batter_hand,
  pitcher_hand.x,
  play_type,
  Freq,
  pitcher_hand.y,
  Freq_total,
  result_frequency
)]

mlb_dt[, batter_at_bat_count := seq_len(.N), by = batter]
mlb_dt[, pitcher_at_bat_count := seq_len(.N), by = pitcher]

replacement_batter_freq_calc = mlb_dt[dataset == 'MLB 2021 Regular Season' & batter_at_bat_count <= 30,]
replacement_batter_freq_calc = replacement_batter_freq_calc[!(batter %in% mlb_dt$pitcher),]
replacement_pitcher_freq_calc = mlb_dt[dataset == 'MLB 2021 Regular Season' & pitcher_at_bat_count <= 30,]

replacement_batter_result_frequency = replacement_batter_freq_calc[, .(dataset,
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

replacement_pitcher_result_frequency = replacement_pitcher_freq_calc[, .(dataset,
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

replacement_batter = aggregate(
  result_frequency ~ pitcher_hand + batter_hand.x + play_type,
  batter_result_frequency,
  mean
) %>% data.table()

replacement_pitcher = aggregate(
  result_frequency ~ pitcher_hand.x + batter_hand + play_type,
  pitcher_result_frequency,
  mean
) %>% data.table()

replacement_batter = replacement_batter[batter_hand.x == 'R' & pitcher_hand != 'S',]
replacement_pitcher = replacement_pitcher[pitcher_hand.x == 'R' & batter_hand != 'S',]

replacement_batter[, batter := "REPLACEMENT BATTER"]
replacement_pitcher[, pitcher := "REPLACEMENT PITCHER"]

replacement_batter = replacement_batter[, .(
  batter,
  batter_id = 99999,
  pitcher_hand,
  batter_hand.x,
  play_type,
  Freq = 1000,
  batter_hand.y = batter_hand.x,
  Freq_total = 1000,
  result_frequency
)]

replacement_pitcher = replacement_pitcher[, .(
  pitcher,
  pitcher_id = 99999,
  batter_hand,
  pitcher_hand.x,
  play_type,
  Freq = 1000,
  pitcher_hand.y = pitcher_hand.x,
  Freq_total = 1000,
  result_frequency
)]

batter_result_frequency = rbind(batter_result_frequency, replacement_batter)

pitcher_result_frequency = rbind(pitcher_result_frequency, replacement_pitcher)

replacement_batting_pitcher = replacement_batter[play_type %in% c('SINGLE',
                                                                  'DOUBLE',
                                                                  'GROUNDOUT',
                                                                  'FLYOUT',
                                                                  'STRIKEOUT',
                                                                  'WALK')]

replacement_batting_pitcher[play_type == "SINGLE", result_frequency := .08]
replacement_batting_pitcher[play_type == "DOUBLE", result_frequency := .02]
replacement_batting_pitcher[play_type == "GROUNDOUT", result_frequency := .25]
replacement_batting_pitcher[play_type == "FLYOUT", result_frequency := .11]
replacement_batting_pitcher[play_type == "STRIKEOUT", result_frequency := .5]
replacement_batting_pitcher[play_type == "WALK", result_frequency := .04]

replacement_batting_pitcher$batter = 'REPLACEMENT PITCHER'

batter_result_frequency = rbind(batter_result_frequency, replacement_batting_pitcher)

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

full_freq = home_field_result_frequency[, .(total_result_frequency = mean(park_result_frequency)), by = .(pitcher_hand, batter_hand, play_type)]

home_field_result_frequency = merge(
  home_field_result_frequency,
  full_freq,
  by = c("pitcher_hand",
         "batter_hand",
         "play_type")
)

home_field_result_frequency[, park_adjustment := (park_result_frequency - total_result_frequency) + 1]

home_field_result_frequency = home_field_result_frequency[, .(pitcher_hand,
                                                              batter_hand,
                                                              play_type,
                                                              game_home_team,
                                                              park_adjustment)]

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

league_result_frequency = pitcher_result_frequency[, .(freq = sum(Freq)), by = .(play_type, batter_hand, pitcher_hand.x)]

league_result_frequency[, league_freq_total := freq / sum(freq), by = .(batter_hand, pitcher_hand.x)]

pitcher_result_frequency = merge(
  pitcher_result_frequency,
  league_result_frequency[, .(play_type, batter_hand, pitcher_hand.x, league_freq_total)],
  by = c("play_type", "batter_hand", "pitcher_hand.x")
)

pitcher_result_frequency[, pitcher_probability_multiplier := result_frequency / league_freq_total]

batter_result_frequency_dt = batter_result_frequency
pitcher_result_frequency_dt = pitcher_result_frequency

rm(batter_result_frequency)
rm(pitcher_result_frequency)




