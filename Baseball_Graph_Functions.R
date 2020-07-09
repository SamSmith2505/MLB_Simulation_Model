library(ggplot2)
library(teamcolors)

##RGB Pat Purple - r99, g60, b151
graph_results = function(id) {
  
  if(is.numeric(id) == T){
  team_1 = unique(lineup_dt[game_id == id & home_away == "A", team])
  team_2 = unique(lineup_dt[game_id == id & home_away == "H", team])
  } else {
    graph_game_id = unique(lineup_dt[team == id,game_id])
    id = graph_game_id
    
    team_1 = unique(lineup_dt[game_id == id & home_away == "A", team])
    team_2 = unique(lineup_dt[game_id == id & home_away == "H", team])
  }
  
  color_1 = team_filter(team_1)$primary
  color_2 = team_filter(team_2)$primary
  graph_dt = team_scores[team %in% c(team_1, team_2), .(team, runs_generated)]
  
  ggplot(graph_dt, aes(x = runs_generated, fill = team)) + geom_density(alpha = .5) +
    scale_fill_manual(values = c(
      color_1,
      color_2
    )) + xlab("Runs Scored") + ylab ("Result Density") + ggtitle(paste0(team_1, " vs. ", team_2, " Score Distributions"),
                                                                 subtitle = "Simulated Results") + theme_bw()
}

