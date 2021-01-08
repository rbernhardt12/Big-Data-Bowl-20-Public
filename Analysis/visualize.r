### NFL Big Data Bowl 2020-21 
# Robert Bernhardt, Andrew Rogan, Daniel Weiss 
# October 2020 

## Preliminary Data Visualization (Visualizing All Plays in a Week)

# Source functions 
source("Analysis/Functions/visualization_functions.r")

WeekCounter = 0 
for (WeekData in BaseData) {

WeekCounter = WeekCounter + 1 
  
# List of games for each week
GameList <- unique(WeekData$gameId) 

# Loop through each game of that week
for (idx_game in 1:length(GameList)) { 
   
game <- GameList[idx_game]
   
   GameData <- WeekData %>% # Data for the specified game
     filter(gameId == game, event =="ball_snap")  
   
   PlayList <- unique(GameData$playId) # Relevant play ID values for this game 
   
   # Loop through each play of the current game
   for (i in 1:length(PlayList)) { 
     
     play <- PlayList[i]
     
     GeneralPlayData <- plays %>% # Obtain general info about play
       filter(gameId == game) %>% 
       filter(playId == play)
     
     TrackingPlayData <- GameData %>% # Obtain specific tracking data 
       filter(playId == play)
     
     vis_data <- prep_vis_data(GeneralPlayData, TrackingPlayData) # Data used pulled from the general and tracking info
                                                                  # to use in our visualizations
     
     Offense <- vis_data$Offense
     Defense <- vis_data$Defense
     Ball <- vis_data$Ball
     BallX <- vis_data$BallX
     FirstDownLine <- vis_data$FirstDownLine
     
     title <- paste("Week:" , WeekCounter , " Game:" , GeneralPlayData$gameId , " Play:" , GeneralPlayData$playId)
     subtitle <- paste("Quarter:" , GeneralPlayData$quarter , "  Down:" , GeneralPlayData$down , "  To Go:" , GeneralPlayData$yardsToGo ,   
                       "  Time:" , GeneralPlayData$gameClock)
     caption <- paste(GeneralPlayData$playDescription)
     
     shell_features <- prep_shell_vis(TrackingPlayData) 
     
     ggplot() + 
       AddFieldColor() + AddFieldLines() + AddYardNumbers() + AddUpsideDownYardNumbers() + ClearFigBackground() + 
       AddsPlayData(Offense, Defense, Ball, BallX, FirstDownLine) + AddShellMarkers(shell_features) + 
       labs(title = title , subtitle = subtitle , caption = caption ) + theme_void()
     
     FileName <- paste("Figures/w", WeekCounter , "g", game, "p", play, ".png", sep = "")
     ggsave(FileName, width=14*2, height=7.33*2, unit="cm")
     
     clear()
     print("Whole Play") 
     print(paste( "Play" , i , "of" , length(PlayList)))
     print(paste( "Game" , idx_game , "of" , length(GameList)))
     print(paste( "Week" , WeekCounter , "of" , "17" ))
     } 
  } 
}

#

# Visualizing Play at Time of Throw 

WeekCounter = 0 
for (WeekData in BaseData) {
  
  WeekCounter = WeekCounter + 1 

# List of games for a particular week
GameList <- unique(WeekData$gameId) 

# Loop through each game of that week
for (idx_game in 1:length(GameList)) { 
  
  game <- GameList[idx_game]
  
  GameData <- WeekData %>% # Data for specified game
    filter(gameId == game)  
  
  PlayList <- unique(GameData$playId) # Relevant play ID values for this game 
  
  # Loop through each play of the current game
  for (i in 1:length(PlayList)) { 
    
    play <- PlayList[i]
    
    GeneralPlayData <- plays %>% # Obtain general information about play
      filter(gameId == game) %>% 
      filter(playId == play)
    
    TrackingPlayData <- GameData %>% # Obtain specific tracking data 
      filter(gameId == game) %>% 
      filter(playId == play)
    
    PlayerTargeted <- TargetedReceiver %>% 
      filter(gameId == game) %>% 
      filter(playId == play) 
    
    throw_vis_data <- prep_throw_vis_data(GeneralPlayData , TrackingPlayData , PlayerTargeted) 
      # Data used pulled from the target, general, and tracking info
    
    Offense_throw <- throw_vis_data$Offense_throw
    Defense_throw <- throw_vis_data$Defense_throw
    Ball_throw <- throw_vis_data$Ball_throw
    BallX <- throw_vis_data$BallX
    FirstDownLine <- throw_vis_data$FirstDownLine
    target_player_location <- throw_vis_data$target_player_location
    
    title <- paste("Week:" , WeekCounter , " Game:" , GeneralPlayData$gameId , " Play:" , GeneralPlayData$playId)
    subtitle <- paste("Quarter:" , GeneralPlayData$quarter , "  Down:" , GeneralPlayData$down , "  To Go:" , GeneralPlayData$yardsToGo ,   
                      "  Time:" , GeneralPlayData$gameClock)
    caption <- paste(GeneralPlayData$playDescription)

    ggplot() + 
      AddFieldColor() + AddFieldLines() + AddYardNumbers() + AddUpsideDownYardNumbers() + ClearFigBackground() + 
      AddsThrowData(Offense_throw, Defense_throw, Ball_throw, BallX, FirstDownLine, target_player_location) + 
      labs(title = title , subtitle = subtitle , caption = caption ) + theme_void()

    FileName <- paste("Figures/w" , WeekCounter , "g" , game , "p" , play , "throw.png" , sep = "")
    ggsave(FileName , width=14*2, height=7.33*2, unit="cm")

    clear()
    print("Throw-Specific") 
    print(paste( "Play" , i , "of" , length(PlayList)))
    print(paste( "Game" , idx_game , "of" , length(GameList)))
    print(paste( "Week" , WeekCounter , "of" , "17" ))
    }
  }
}

print("Visualizations Completed")
