batter_pitches_seen = mlb_dt[, .(dataset,
                                 batter,
                                 number_of_pitches)]

pitcher_pitches_thrown = mlb_dt[, .(dataset,
                                    pitcher,
                                    number_of_pitches)]

batter_pitches_seen_mean = aggregate(number_of_pitches ~ dataset + batter, batter_pitches_seen, mean)
pitcher_pitches_thrown_mean = aggregate(number_of_pitches ~ dataset + pitcher,
                                        pitcher_pitches_thrown,
                                        mean)
batter_pitches_seen_sd = aggregate(number_of_pitches ~ dataset + batter, batter_pitches_seen, sd)
pitcher_pitches_thrown_sd = aggregate(number_of_pitches ~ dataset + pitcher,
                                      pitcher_pitches_thrown,
                                      sd)

batter_pitches_seen = merge(batter_pitches_seen_mean,
                            batter_pitches_seen_sd,
                            by = c("dataset", "batter"))
pitcher_pitches_thrown = merge(pitcher_pitches_thrown_mean,
                               pitcher_pitches_thrown_sd,
                               by = c("dataset", "pitcher"))

colnames(batter_pitches_seen) = c("dataset", "batter", "pitches_mean", "pitches_sd")
colnames(pitcher_pitches_thrown) = c("dataset", "pitcher", "pitches_mean", "pitches_sd")
batter_pitches_seen = subset(batter_pitches_seen, dataset == "MLB 2019 Regular Season") %>% data.table()
pitcher_pitches_thrown = subset(pitcher_pitches_thrown, dataset == "MLB 2019 Regular Season") %>% data.table()

pitcher_pitch_counts = aggregate(number_of_pitches ~ game_id + pitcher, mlb_dt, sum)
pitcher_pitch_means = aggregate(number_of_pitches ~ pitcher, pitcher_pitch_counts, mean)
pitcher_pitch_sd = aggregate(number_of_pitches ~ pitcher, pitcher_pitch_counts, sd)
pitch_count_historical = merge(pitcher_pitch_means, pitcher_pitch_sd, by = "pitcher") %>% data.table()

colnames(pitch_count_historical) = c("pitcher", "pitch_count_mean", "pitch_count_sd")

first_at_bat_dt = subset(
  mlb_dt,
  (
    inning == "1T" &
      road_score == 0 &
      runner_on_first == "" &
      runner_on_second == "" &
      runner_on_third == ""
  ) |
    (
      inning == "1B" &
        home_score == 0 &
        runner_on_first == 0 &
        runner_on_second == 0 & runner_on_third == 0
    )
)

starting_pitcher_dt = first_at_bat_dt[, .(game_id, pitcher, pitching_team)] %>% unique()
colnames(starting_pitcher_dt) = c("game_id", "starting_pitcher", "pitching_team")

bullpen_abs = merge(mlb_dt, starting_pitcher_dt, by = c("game_id", "pitching_team"))
bullpen_abs = subset(bullpen_abs, pitcher != starting_pitcher & inning %in% c("7T", "7B", "8T", "8B", "9T", "9B"))

bullpen_result_frequency = bullpen_abs[, .(dataset,
                                           pitching_team,
                                           play_type,
                                           pitcher_hand,
                                           batter_hand)] %>% group_by(dataset,
                                                                      pitching_team,
                                                                      play_type,
                                                                      pitcher_hand,
                                                                      batter_hand) %>%
  summarise(Freq = n())

total_bullpen_frequency = bullpen_abs[, .(dataset,
                                     pitching_team,
                                     pitcher_hand,
                                     batter_hand)] %>% group_by(dataset, pitching_team, pitcher_hand, batter_hand) %>%
  summarise(Freq_total = n())


bullpen_result_frequency = merge(
  bullpen_result_frequency,
  total_bullpen_frequency,
  by = c("dataset", "pitching_team", "pitcher_hand", "batter_hand"),
  all.x = T
) %>% data.table()

bullpen_result_frequency[, result_frequency := Freq / Freq_total]

bullpen_result_frequency = subset(bullpen_result_frequency, dataset == "MLB 2019 Regular Season")
bullpen_result_frequency = bullpen_result_frequency[pitcher_hand == "R", .(player = "BULLPEN",
                                                                           game_id = NA_integer_,
                                                                           team = pitching_team,
                                                                           home_away = NA_character_,
                                                                           lineup_spot = 11,
                                                                           position = 1,
                                                                           dataset, 
                                                                           pitcher_id = NA_integer_,
                                                                           batter_hand,
                                                                           pitcher_hand,
                                                                           play_type,
                                                                           result_frequency,
                                                                           is_starting_pitcher = 0
)]

replacement_batter = batter_pitches_seen[, .(
  dataset = "MLB 2019 Regular Season",
  batter = "REPLACEMENT BATTER",
  pitches_mean = mean(pitches_mean),
  pitches_sd = mean(pitches_sd, na.rm = T)
)]

batter_pitches_seen = rbind(batter_pitches_seen, replacement_batter)

replacement_pitcher = pitcher_pitches_thrown[, .(
  dataset = "2019 MLB Regular Season",
  pitcher = "REPLACEMENT PITCHER",
  pitches_mean = mean(pitches_mean),
  pitches_sd = mean(pitches_sd, na.rm = T)
)]

pitcher_pitches_thrown = rbind(pitcher_pitches_thrown, replacement_pitcher)

replacement_pitcher = pitch_count_historical[, .(
  pitcher = "REPLACEMENT PITCHER",
  pitch_count_mean = mean(pitch_count_mean),
  pitch_count_sd = mean(pitch_count_sd, na.rm = T)
)]

pitch_count_historical = rbind(pitch_count_historical, replacement_pitcher)
