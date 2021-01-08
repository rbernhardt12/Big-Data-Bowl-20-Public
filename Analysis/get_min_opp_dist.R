get_min_opp_dist <- function(player_position_table) {
    
    min_dist <- vector(mode = "double", length = nrow(player_position_table))
    
    for (i in 1:nrow(player_position_table)) {
        x <- player_position_table$x[i]
        y <- player_position_table$y[i]
        team <- player_position_table$team[i]
        
        if (team == "football") {
            next()
        }
        
        idx <- player_position_table$team != team & player_position_table$team != "football"
        min_dist[i] = min(sqrt((x - player_position_table$x[idx])^2 + (y - player_position_table$y[idx])^2))
    }
    
    player_position_table$min_dist <- min_dist

    return(player_position_table)
    
}