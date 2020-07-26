#lineup_dt = fread("daily_lineup_df.csv")
full_lineup_dt = lineup_dt
lineup_dt = lineup_dt[!(team %in% daily_win_dt$home_team) &
                        !(team %in% daily_win_dt$away_team), ]

lineup_dt = lineup_dt[!(game_id %in% daily_win_dt$game_id)]

for (game in unique(lineup_dt$game_id)) {
  skip_to_next = F
  tryCatch({
    current_game = subset(lineup_dt, game_id == game)
    home_team = unique(current_game[home_away == "H", team])
    away_team = unique(current_game[home_away == "A", team])
    home_wins = 0
    away_wins = 0
    
    batter_result_frequency_dt = subset(batter_result_frequency,
                                        dataset %in% c("MLB 2019 Regular Season"))
    pitcher_result_frequency_dt = subset(pitcher_result_frequency,
                                         dataset %in% c("MLB 2019 Regular Season"))
    
    current_game[!(player %in% batter_result_frequency_dt$batter) &
                   position != 1, player := "REPLACEMENT BATTER"]
    current_game[!(player %in% pitcher_result_frequency_dt$pitcher) &
                   position == 1, player := "REPLACEMENT PITCHER"]
    
    game_batter_result_frequency_dt = merge(
      current_game,
      batter_result_frequency_dt,
      by.x = "player",
      by.y = "batter",
      all.x = T
    )
    
    game_batter_result_frequency_dt = game_batter_result_frequency_dt[, .(
      player,
      game_id,
      team,
      home_away,
      lineup_spot,
      position,
      dataset,
      batter_id,
      pitcher_hand,
      batter_hand = batter_hand.x,
      play_type,
      result_frequency
    )]
    
    game_pitcher_dt = subset(current_game, position == 1)
    game_pitcher_result_frequency_dt = merge(
      game_pitcher_dt,
      pitcher_result_frequency_dt,
      by.x = "player",
      by.y = "pitcher",
      all.x = T
    )
    
    game_pitcher_result_frequency_dt = game_pitcher_result_frequency_dt[, .(
      player,
      game_id,
      team,
      home_away,
      lineup_spot,
      position,
      dataset,
      pitcher_id,
      batter_hand,
      pitcher_hand = pitcher_hand.x,
      play_type,
      result_frequency,
      is_starting_pitcher = 1
    )]
    
    game_bullpen_frequency_dt = subset(bullpen_result_frequency,
                                       team %in% c(home_team, away_team))
    game_bullpen_frequency_dt[, `:=`(game_id = game,
                                     home_away = ifelse(team == home_team, 'H', "A"))]
    game_pitcher_result_frequency_dt = rbindlist(list(
      game_pitcher_result_frequency_dt,
      game_bullpen_frequency_dt
    )) %>% setDT()
    
    game_historical_pitch_count = subset(pitch_count_historical,
                                         pitcher %in% game_pitcher_dt$player)
    game_historical_pitch_count = merge(
      game_historical_pitch_count,
      game_pitcher_dt[, .(player, home_away)],
      by.x = "pitcher",
      by.y = "player"
    ) %>% data.table()
    
    for (iteration in 1:game_iterations) {
      game_dt = data.table()
      game_stolen_base_dt = data.table(runner_on_first = character(50))
      game_rbi_dt = data.table(batter = character(50),
                               rbi = integer(50))
      game_run_dt = data.table(scorer = integer(50))
      home_lineup_counter = 1
      away_lineup_counter = 1
      home_pitch_count = 0
      away_pitch_count = 0
      home_score = 0
      away_score = 0
      out_count = 0
      
      home_pitch_count_max = rnorm(1, mean = game_historical_pitch_count[home_away == "H", pitch_count_mean], sd = game_historical_pitch_count[home_away == "H", pitch_count_sd])
      away_pitch_count_max = rnorm(1, mean = game_historical_pitch_count[home_away == "A", pitch_count_mean], sd = game_historical_pitch_count[home_away == "A", pitch_count_sd])
      
      for (inning_half in (1:50)) {
        out_count = 0
        if (inning_half > 2) {
          away_score = sum(game_dt[h_a_identifier == "A", runs_generated])
          home_score = sum(game_dt[h_a_identifier == "H", runs_generated])
        }
        if ((inning_half >= 19 &
             away_score - home_score != 0) ||
            (inning_half > 17 &
             away_score < home_score)) {
          if (away_score > home_score) {
            away_wins = away_wins + 1
          } else {
            home_wins = home_wins + 1
          }
          break
        }
        inning_half_dt = data.table()
        basepaths_dt = data.table()
        basepaths_dt$runner_on_first = NA
        basepaths_dt$runner_on_second = NA
        basepaths_dt$runner_on_third = NA
        if (inning_half %% 2 == 1) {
          inning_half_lineup = subset(current_game, home_away == "A")
          inning_pitcher_freq_dt = subset(game_pitcher_result_frequency_dt, home_away == "H")
        } else {
          inning_half_lineup = subset(current_game, home_away == "H")
          inning_pitcher_freq_dt = subset(game_pitcher_result_frequency_dt, home_away == "A")
        }
        for (inning_batter_count in 1:30) {
          if (inning_half %% 2 == 1) {
            if (home_pitch_count < home_pitch_count_max) {
              is_home_starter_pitching = 1
            } else {
              is_home_starter_pitching = 0
            }
          } else {
            if (away_pitch_count < away_pitch_count_max) {
              is_away_starter_pitching = 1
            } else {
              is_away_starter_pitching = 0
            }
          }
          runs_scored_on_at_bat = 0
          if (!is.na(basepaths_dt$runner_on_first) &
              is.na(basepaths_dt$runner_on_second)) {
            at_bat_base_stealing_prob = base_stealing_dt[runner_on_first == basepaths_dt$runner_on_first, .(stolen_base_attempt_odds,
                                                                                                            stolen_base_success_odds)]
            base_stealing_attempt_randomizer = runif(1, 0, 1)
            if (base_stealing_attempt_randomizer < at_bat_base_stealing_prob$stolen_base_attempt_odds) {
              base_stealing_success_randomizer = runif(1, 0, 1)
              if (base_stealing_success_randomizer >  at_bat_base_stealing_prob$stolen_base_success_odds) {
                basepaths_dt$runner_on_second = basepaths_dt$runner_on_first
                basepaths_dt$runner_on_first = NA
                base_stealer_dt = basepaths_dt$runner_on_second %>% data.table()
                colnames(base_stealer_dt) = "runner_on_first"
                game_stolen_base_dt = rbindlist(list(game_stolen_base_dt, base_stealer_dt))
              } else {
                basepaths_dt$runner_on_first = NA
                out_count = out_count + 1
              }
            }
          }
          if (inning_half %% 2 == 1) {
            batter = subset(inning_half_lineup,
                            lineup_spot == away_lineup_counter)
            is_starter_pitching = is_home_starter_pitching
          } else {
            batter = subset(inning_half_lineup,
                            lineup_spot == home_lineup_counter)
            is_starter_pitching = is_away_starter_pitching
          }
          
          if(is_starter_pitching == 0){
            bullpen_hand_randomizer = runif(1,0,1)
            bullpen_ab_hand = ifelse(bullpen_hand_randomizer > .5, "R", "L")
          }
          
          at_bat_frequency_dt = game_batter_result_frequency_dt[player == batter$player, .(player,
                                                                                           pitcher_hand,
                                                                                           batter_hand,
                                                                                           play_type,
                                                                                           result_frequency)]
          at_bat_pitching_frequency_dt = subset(
            game_pitcher_result_frequency_dt,
            home_away != batter$home_away &
              is_starting_pitcher == is_starter_pitching 
          )
          
          if(is_starter_pitching == 0) {
            at_bat_pitching_frequency_dt = at_bat_pitching_frequency_dt[pitcher_hand == bullpen_ab_hand,]
          }
            
          full_at_bat_probability_dt = merge(
            at_bat_frequency_dt[, .(player,
                                    pitcher_hand,
                                    batter_hand,
                                    play_type,
                                    batter_result_frequency = result_frequency)],
            at_bat_pitching_frequency_dt[, .(player,
                                             pitcher_hand,
                                             batter_hand,
                                             play_type,
                                             pitcher_result_frequency = result_frequency)],
            by = c("pitcher_hand", "batter_hand", "play_type")
          )
          
          full_at_bat_probability_dt = merge(
            full_at_bat_probability_dt,
            home_field_result_frequency[game_home_team == home_team],
            by = c("pitcher_hand", "batter_hand", "play_type")
          )
          
          full_at_bat_probability_dt[, outcome_probability := (batter_result_frequency * batter_result_weight) + (pitcher_result_frequency * pitcher_result_weight) * park_adjustment]
          full_at_bat_probability_dt[, outcome_probability := outcome_probability + ((1 - sum(outcome_probability)) / nrow(full_at_bat_probability_dt)), by = player.y]
          
          at_bat_outcome_dt = full_at_bat_probability_dt[, .(batter = player.x,
                                                             pitcher = player.y,
                                                             play_type,
                                                             outcome_probability)]
          
          at_bat_outcome_dt[, lower_bound_outcome_prob := cumsum(outcome_probability) - outcome_probability]
          at_bat_outcome_dt[, upper_bound_outcome_prob := cumsum(outcome_probability)]
          
          outcome = runif(1, 0, 1)
          
          at_bat_final_outcome = subset(
            at_bat_outcome_dt,
            lower_bound_outcome_prob < outcome &
              upper_bound_outcome_prob > outcome
          )
          
          at_bat_final_outcome = at_bat_final_outcome[1, ]
          at_bat_final_outcome[, outs_created := ifelse(
            play_type %in% c(
              "POP OUT",
              "STRIKEOUT",
              "GROUNDOUT",
              "FLYOUT",
              "LINEOUT",
              "FORCEOUT"
            ),
            1,
            0
          )]
          
          batter_bases_advanced = ifelse(
            at_bat_final_outcome$play_type %in% c(
              "POP OUT",
              "STRIKEOUT",
              "GROUNDOUT",
              "FLYOUT",
              "LINEOUT",
              "FORCEOUT"
            ),
            0,
            ifelse(
              at_bat_final_outcome$play_type %in% c(
                "WALK",
                "SINGLE",
                "HIT BY PITCH",
                "INTENT WALK",
                "FIELD ERROR",
                "CATCHER INTERFERENCE",
                "FAN INTERFERENCE"
              ),
              1,
              ifelse(
                at_bat_final_outcome$play_type == "DOUBLE",
                2,
                ifelse(at_bat_final_outcome$play_type == "TRIPLE", 3,
                       4)
              )
            )
          )
          
          at_bat_final_outcome$batter_bases_advanced = batter_bases_advanced
          
          pitch_count_dt = at_bat_final_outcome[, .(batter, pitcher)]
          pitch_count_dt = merge(pitch_count_dt, batter_pitches_seen[, .(batter, pitches_mean, pitches_sd)], by = "batter") %>% unique()
          if (pitch_count_dt$pitcher != "BULLPEN") {
            pitch_count_dt = merge(pitch_count_dt, pitcher_pitches_thrown[, .(pitcher, pitches_mean, pitches_sd)], by = "pitcher")
          } else {
            pitch_count_dt[, `:=`(
              pitches_mean.x = pitches_mean,
              pitches_sd.x = pitches_sd,
              pitches_mean.y = 4,
              pitches_sd.y = 1,
              pitches_mean = NULL,
              pitches_sd = NULL
            )]
          }
          ab_pitch_count_mean = mean(pitch_count_dt[, pitches_mean.x], pitch_count_dt[, pitches_mean.y])
          ab_pitch_count_sd = mean(pitch_count_dt[, pitches_sd.x], pitch_count_dt[, pitches_sd.y])
          pitches_in_at_bat = rnorm(n = 1,
                                    mean = ab_pitch_count_mean,
                                    sd = ab_pitch_count_sd)
          
          if (inning_half %% 2 == 1) {
            away_pitch_count = away_pitch_count + pitches_in_at_bat
          } else {
            home_pitch_count = home_pitch_count + pitches_in_at_bat
          }
          
          out_count = out_count + at_bat_final_outcome$outs_created
          if (out_count < 3) {
            if (batter_bases_advanced == 4) {
              runs_scored_on_at_bat = runs_scored_on_at_bat + sum(!is.na(basepaths_dt$runner_on_first)) + sum(!is.na(basepaths_dt$runner_on_second)) + sum(!is.na(basepaths_dt$runner_on_third)) + 1
              game_run_dt = rbind(
                game_run_dt,
                basepaths_dt$runner_on_first,
                basepaths_dt$runner_on_second,
                basepaths_dt$runner_on_third,
                at_bat_final_outcome$batter,
                use.names = F
              )
              basepaths_dt$runner_on_first = NA
              basepaths_dt$runner_on_second = NA
              basepaths_dt$runner_on_third = NA
              
            }
            if (batter_bases_advanced == 3) {
              runs_scored_on_at_bat = runs_scored_on_at_bat + sum(!is.na(basepaths_dt$runner_on_first)) + sum(!is.na(basepaths_dt$runner_on_second)) + sum(!is.na(basepaths_dt$runner_on_third))
              game_run_dt = rbind(
                game_run_dt,
                basepaths_dt$runner_on_first,
                basepaths_dt$runner_on_second,
                basepaths_dt$runner_on_third,
                use.names = F
              )
              basepaths_dt$runner_on_first = NA
              basepaths_dt$runner_on_second = NA
              basepaths_dt$runner_on_third = at_bat_final_outcome$batter
            }
            if (batter_bases_advanced == 2) {
              runs_scored_on_at_bat = runs_scored_on_at_bat + sum(!is.na(basepaths_dt$runner_on_first)) + sum(!is.na(basepaths_dt$runner_on_second)) + sum(!is.na(basepaths_dt$runner_on_third))
              game_run_dt = rbind(
                game_run_dt,
                basepaths_dt$runner_on_first,
                basepaths_dt$runner_on_second,
                basepaths_dt$runner_on_third,
                use.names = F
              )
              basepaths_dt$runner_on_first = NA
              basepaths_dt$runner_on_second = at_bat_final_outcome$batter
              basepaths_dt$runner_on_third = NA
            }
            if (at_bat_final_outcome$play_type == "SINGLE") {
              runs_scored_on_at_bat = runs_scored_on_at_bat + sum(!is.na(basepaths_dt$runner_on_third)) + sum(!is.na(basepaths_dt$runner_on_second))
              game_run_dt = rbind(
                game_run_dt,
                basepaths_dt$runner_on_second,
                basepaths_dt$runner_on_third,
                use.names = F
              )
              basepaths_dt$runner_on_third = NA
              basepaths_dt$runner_on_second = basepaths_dt$runner_on_first
              basepaths_dt$runner_on_first = at_bat_final_outcome$batter
            }
            if (at_bat_final_outcome$play_type %in% c(
              "WALK",
              "HIT BY PITCH",
              "INTENT WALK",
              "FIELD ERROR",
              "CATCHER INTERFERENCE",
              "FAN INTERFERENCE"
            )) {
              runs_scored_on_at_bat = runs_scored_on_at_bat + sum(!is.na(basepaths_dt$runner_on_third))
              game_run_dt = rbind(game_run_dt,
                                  basepaths_dt$runner_on_third,
                                  use.names = F)
              basepaths_dt$runner_on_third = basepaths_dt$runner_on_second
              basepaths_dt$runner_on_second = basepaths_dt$runner_on_first
              basepaths_dt$runner_on_first = at_bat_final_outcome$batter
            }
            if (at_bat_final_outcome$play_type == "FLYOUT" &
                out_count < 2) {
              runs_scored_on_at_bat = runs_scored_on_at_bat + sum(!is.na(basepaths_dt$runner_on_third))
              game_run_dt = rbind(game_run_dt,
                                  basepaths_dt$runner_on_third,
                                  use.names = F)
              basepaths_dt$runner_on_third = NA
            }
            if (at_bat_final_outcome$play_type == "GROUNDOUT" &
                out_count < 2) {
              runs_scored_on_at_bat = runs_scored_on_at_bat + sum(!is.na(basepaths_dt$runner_on_third))
              game_run_dt = rbind(game_run_dt,
                                  basepaths_dt$runner_on_third,
                                  use.names = F)
              basepaths_dt$runner_on_third = NA
            }
          }
          at_bat_final_outcome$runs_generated = runs_scored_on_at_bat
          if (runs_scored_on_at_bat > 0) {
            batter_rbi_dt = data.table()
            batter_rbi_dt[, `:=`(batter = at_bat_final_outcome$batter,
                                 rbi = runs_scored_on_at_bat)]
            game_rbi_dt = rbindlist(list(game_rbi_dt, batter_rbi_dt))
          }
          
          if (inning_half %% 2 == 1) {
            away_lineup_counter = away_lineup_counter + 1
            if (away_lineup_counter > 9) {
              away_lineup_counter = 1
            }
          } else {
            home_lineup_counter = home_lineup_counter + 1
            if (home_lineup_counter > 9) {
              home_lineup_counter = 1
            }
          }
          inning_half_dt = rbindlist(list(inning_half_dt, at_bat_final_outcome))
          out_count = sum(inning_half_dt$outs_created)
          if (out_count >= 3) {
            inning_half_dt$inning = ifelse(inning_half %% 2 == 0,
                                           paste("B", inning_half / 2),
                                           paste("T", ceiling(inning_half / 2)))
            inning_half_dt$h_a_identifier = ifelse(inning_half %% 2 == 0,
                                                   "H",
                                                   "A")
            basepaths_dt$runner_on_first = NA
            basepaths_dt$runner_on_second = NA
            basepaths_dt$runner_on_third = NA
            game_dt = rbindlist(list(game_dt, inning_half_dt))
            out_count = 0
            break
          }
        }
      }
      home_away_assignment = lineup_dt[, .(player,
                                           home_away)]
      
      game_dt[, h_a_identifier := NULL]
      
      game_dt = merge(game_dt,
                      home_away_assignment,
                      by.x = "batter",
                      by.y = "player")
      
      home_at_bats = subset(game_dt, home_away == "H")
      away_at_bats = subset(game_dt, home_away == "A")
      
      
      print(
        paste0(
          "FINAL SCORE ",
          iteration,
          ": ",
          away_team,
          " ",
          sum(away_at_bats$runs_generated),
          " ",
          home_team,
          " ",
          sum(home_at_bats$runs_generated),
          " ",
          ifelse(inning_half > 19, ceiling(inning_half / 2), "")
        )
      )
      game_dt$game_number = iteration
      game_stolen_base_dt$game_number = iteration
      game_run_dt$game_number = iteration
      game_rbi_dt$game_number = iteration
      game_run_dt = game_run_dt[scorer != "0" & !is.na(scorer), ]
      game_rbi_dt = game_rbi_dt[batter != "" & !is.na(batter), ]
      game_stolen_base_dt = game_stolen_base_dt[!is.na(runner_on_first), ]
      daily_matchup_dt = rbindlist(list(daily_matchup_dt, game_dt))
      day_stolen_base_dt = rbindlist(list(day_stolen_base_dt, game_stolen_base_dt))
      day_rbi_dt = rbindlist(list(day_rbi_dt, game_rbi_dt))
      day_run_dt = rbindlist(list(day_run_dt, game_run_dt))
    }
    full_day_dt = rbindlist(list(daily_matchup_dt, full_day_dt))
    
    
    game_win_totals = data.table(
      game_id = game,
      home_team = home_team,
      away_team = away_team,
      home_wins = home_wins,
      away_wins = away_wins
    )
    
    daily_win_dt = rbindlist(list(daily_win_dt, game_win_totals))
  },
  error = function(e) {
    skip_to_next <<- T
  })
  if (skip_to_next) {
    next
  }
}
