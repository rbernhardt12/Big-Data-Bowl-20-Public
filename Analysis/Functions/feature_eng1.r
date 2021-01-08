### NFL Big Data Bowl 2020-21 
# Robert Bernhardt, Andrew Rogan, Daniel Weiss 
# December 2020 

## Feature Engineering 
# Objective: Further clean data & create features which can be used for analysis 

# Task 
# Make all plays go left to right (from Offense POV) 

AnalysisData <- AnalysisData %>% 
  mutate( x =      case_when(  playDirection == "right" ~ x ,
                               playDirection == "left" ~ 120 - x)) %>% 
  mutate( y =      case_when(  playDirection == "right" ~ y ,
                               playDirection == "left" ~ 160/3 - y)) %>% 
  mutate( dir =    case_when(  playDirection == "right" ~ dir ,
                               playDirection == "left" ~ mod(dir+180,360))) %>% 
  mutate( o =      case_when(  playDirection == "right" ~ o ,
                               playDirection == "left" ~ mod(o+180,360)))


# Task
# Create Event 2 (event2) featuring greater standardization 

AnalysisData <- AnalysisData %>% 
  group_by(gameId , playId) %>%
  mutate(event2 = case_when(  
            event == "ball_snap" ~ "ball_snap" ,  
            event == "pass_forward" | event == "pass_shovel" ~ "pass_thrown" , 
            event == "pass_arrived" ~ "pass_arrived" , 
            event == "pass_outcome_caught" | event == "pass_outcome_interception" |
                event == "pass_outcome_incomplete" | event == "pass_outcome_touchdown" ~ "pass_outcome" , 
            event == "tackle" | event == "out_of_bounds" | event == "touchdown" | event == "touchback" 
                ~ "later_final_outcome" , 
            TRUE ~ "None")) 

   
# Task 
# Create Event 3 (event3) featuring maximal standardization: ball_snap, pass_thrown, pass_arrived
# If no "pass_arrived" event, then "pass_outcome" indicates the arrival for event 3 

AnalysisData <- AnalysisData %>% 
  group_by(gameId , playId) %>% 
  mutate(no_arrived = sum(event2 == "pass_arrived") == 0) %>% 
  mutate( event3 = case_when(event2 == "ball_snap" ~ "ball_snap" , 
                             event2 == "pass_thrown" ~ "pass_thrown" , 
                             event2 == "pass_arrived" ~ "pass_arrived" , 
                             event2 == "pass_outcome" & no_arrived == TRUE ~ "pass_arrived" , 
                             TRUE ~ "None")) %>% 
  mutate(no_arrived = NULL) # %>% 


# Task
# Removes Data without any pass or any arrival in the events 

AnalysisData <- AnalysisData %>% 
  filter(sum(event3 == "pass_thrown") > 0 ) %>% 
  filter(sum(event3 == "pass_arrived") > 0)

  
# Task
# Identify relevant teams 

AnalysisData <- AnalysisData %>%
  group_by(gameId, playId, frameId) %>% 
  mutate(QBTeam = team[match('QB', position)]) %>%
  ungroup
  
AnalysisData <- AnalysisData %>%
  group_by(playId , gameId) %>% 
  mutate(OffDef = case_when(  team == QBTeam ~ "Offense",
                              team != QBTeam ~ "Defense")) %>%
  mutate(OffDef = case_when(  displayName != "Football" ~ OffDef,
                              displayName == "Football" ~ "Football")) %>% 
  mutate(Offense = case_when( OffDef == "Offense" ~ TRUE , 
                              OffDef == "Defense" ~ FALSE , 
                              OffDef == "Football" ~ NA ))
  

# Task 
# Adjust and Create Time Variables 

# Creates Date variable R understands
AnalysisData$Date <- as.Date(substr(AnalysisData$time,1,10))

options("digits.secs"=6) # Allows for fractions of seconds. 
AnalysisData <- AnalysisData %>% # Creates date-time variable R understands 
  mutate(TimeClean = paste(substr(time,1,10) , substr(time,12,23))) %>% 
  mutate(TimeClean = as.POSIXct(TimeClean)) 

AnalysisData <- AnalysisData %>% # Identifies Critical Frames
  group_by(gameId, playId) %>% 
  mutate(SnapFrame = frameId[match('ball_snap', event3)]) %>%
  mutate(PassFrame = frameId[match('pass_thrown',event3)]) %>% 
  mutate(ArrivedFrame = frameId[match('pass_arrived',event3)])

AnalysisData <- AnalysisData %>% 
  mutate(FramesSinceSnap = frameId - SnapFrame) %>% 
  mutate(FramesSincePass = frameId - PassFrame) %>% 
  mutate(FramesSinceArrive = frameId - ArrivedFrame)


# Task 
# Identify Primary Quarterback (player who receives snap)

AnalysisData <- AnalysisData %>% 
  group_by(gameId , playId) %>% 
  mutate(ball_snap_y = y[match('Football ball_snap' , paste(displayName, event3))]) %>%
  mutate(dist2ball_snap = case_when( event == "ball_snap" ~ abs(y - ball_snap_y) , 
                                     TRUE ~ 999)) %>% 
  mutate( BackfieldQB = case_when( event == "ball_snap" & position == "QB" & dist2ball_snap < 3 ~ 'yes' , 
                                     TRUE ~ 'no')) %>% 
  mutate( BackfieldQB = displayName[match('yes',BackfieldQB)]) %>% 
  mutate(ball_snap_y = NULL) %>% mutate(dist2ball_snap = NULL) 
  

# Task 
# Identify Targeted Player 

AnalysisData <- left_join(AnalysisData , TargetedReceiver) 
AnalysisData <- AnalysisData %>% 
  mutate(targetName = case_when( nflId == targetNflId ~ "yes" , 
                                 TRUE ~ "no")) %>% 
  mutate(targetName = displayName[match('yes' , targetName)]) %>%
  filter(is.na(targetNflId) == 0) %>% # Removes plays with no targeted player
  mutate(targetPosition = case_when( nflId == targetNflId ~ "yes" , 
                                     TRUE ~ "no")) %>%
  mutate(targetPosition = position[match('yes',targetPosition)]) 


# Task 
# Identify Primary/Secondary Defenders & Their Distance to Target at Time of Throw 

AnalysisData <- AnalysisData %>% 
  group_by(playId , gameId) %>% 
  mutate(dist2target_x = case_when( displayName == targetName & event3 == "pass_thrown" ~ 'yes' , 
                                    TRUE ~ 'no')) %>% 
  mutate(dist2target_x = x[match('yes' , dist2target_x)]) %>% 
  mutate(dist2target_x = case_when( event3 != "pass_thrown" ~ 999 , 
                                    TRUE ~ abs(x - dist2target_x))) %>% # x-distance found 
  mutate(dist2target_y = case_when( displayName == targetName & event3 == "pass_thrown" ~ 'yes' , 
                                    TRUE ~ 'no')) %>% 
  mutate(dist2target_y = y[match('yes' , dist2target_y)]) %>% 
  mutate(dist2target_y = case_when( event3 != "pass_thrown" ~ 999 , 
                                    TRUE ~ abs(y - dist2target_y))) %>% # y-distance found 
  mutate(dist2target = case_when( OffDef == "Defense" & event3 == "pass_thrown" ~ 
                                      round(sqrt( dist2target_x^2 + dist2target_y^2 ),3) , 
                                  TRUE ~ 999)) %>%  
  # ^ Identify x, y, and Euclidean distance to targeted players for all defenders 
  
  mutate(PrimDefDistThrow = min(dist2target)) %>% 
  mutate(PrimaryDefName = case_when( PrimDefDistThrow == dist2target ~ 'yes' , 
                                     TRUE ~ 'no' )) %>% 
  mutate(PrimaryDefName = displayName[match('yes' , PrimaryDefName)]) %>% 
  mutate(PrimaryDefPosition = case_when( PrimDefDistThrow == dist2target ~ 'yes' , 
                                         TRUE ~ 'no' )) %>% 
  mutate(PrimaryDefPosition = position[match('yes',PrimaryDefPosition)]) %>% 
  mutate(PrimDefDistThrow = case_when( PrimDefDistThrow < 12 ~ PrimDefDistThrow , # Caps primary def distance at 12 yards
                                       TRUE ~ 12 )) %>% 
  # ^ Identify Primary defender (min dist to target), his roster position, and his Euclidean distance 

  mutate(dist2target = case_when( PrimDefDistThrow == dist2target ~ dist2target + 1000 , # Allows ID of 2nd closest defender 
                                  TRUE ~ dist2target )) %>% 
  mutate(SecDefDistThrow = min(dist2target)) %>% 
  mutate(SecondaryDefName = case_when( SecDefDistThrow == dist2target ~ 'yes' , 
                                       TRUE ~ 'no' )) %>% 
  mutate(SecondaryDefName = displayName[match('yes' , SecondaryDefName)]) %>% 
  mutate(SecondaryDefPosition = case_when( SecDefDistThrow == dist2target ~ 'yes' , 
                                           TRUE ~ 'no' )) %>% 
  mutate(SecondaryDefPosition = position[match('yes',SecondaryDefPosition)]) %>% 
  mutate(SecDefDistThrow = case_when( SecDefDistThrow < 12 ~ SecDefDistThrow , 
                                      TRUE ~ 12)) %>% # Caps secondary def dist at 12 yards 
  # ^ Identify Secondary defender, his roster position, & his Euclidean distance 
    
  mutate(dist2target = NULL) %>% mutate(dist2target_y = NULL) %>% mutate(dist2target_x = NULL) 
  

# Task 
# Extract Trajectory Attributes of Targeted Receiver, Primary/Secondary Defender (speed, acceleration, direction, orientation) 

# At Time of throw 
AnalysisData <- AnalysisData %>% 
  mutate(TargetSpeedThrow = case_when( displayName == targetName & event3 == "pass_thrown" ~ 'yes' , 
                                       TRUE ~ 'no' )) %>% 
  mutate(TargetSpeedThrow = s[match('yes',TargetSpeedThrow)]) %>% 
  mutate(PrimDefSpeedThrow = case_when( displayName == PrimaryDefName & event3 == "pass_thrown" ~ 'yes' , 
                                        TRUE ~ 'no' )) %>% 
  mutate(PrimDefSpeedThrow = s[match('yes',PrimDefSpeedThrow)]) %>% 
  mutate(SecDefSpeedThrow = case_when( displayName == SecondaryDefName & event3 == "pass_thrown" ~ 'yes' , 
                                        TRUE ~ 'no' )) %>% 
  mutate(SecDefSpeedThrow = s[match('yes',SecDefSpeedThrow)]) %>% 
  # ^ Indicates critical player speed 
  mutate(TargetAccThrow = case_when( displayName == targetName & event3 == "pass_thrown" ~ 'yes' , 
                                       TRUE ~ 'no' )) %>% 
  mutate(TargetAccThrow = a[match('yes',TargetAccThrow)]) %>% 
  mutate(PrimDefAccThrow = case_when( displayName == PrimaryDefName & event3 == "pass_thrown" ~ 'yes' , 
                                        TRUE ~ 'no' )) %>% 
  mutate(PrimDefAccThrow = a[match('yes',PrimDefAccThrow)]) %>% 
  mutate(SecDefAccThrow = case_when( displayName == SecondaryDefName & event3 == "pass_thrown" ~ 'yes' , 
                                       TRUE ~ 'no' )) %>% 
  mutate(SecDefAccThrow = a[match('yes',SecDefAccThrow)]) %>% 
  # ^ Indicates critical player acceleration  
  mutate(TargetDirThrow = case_when( displayName == targetName & event3 == "pass_thrown" ~ 'yes' , 
                                       TRUE ~ 'no' )) %>% 
  mutate(TargetDirThrow = dir[match('yes',TargetDirThrow)]) %>% 
  mutate(PrimDefDirThrow = case_when( displayName == PrimaryDefName & event3 == "pass_thrown" ~ 'yes' , 
                                        TRUE ~ 'no' )) %>% 
  mutate(PrimDefDirThrow = dir[match('yes',PrimDefDirThrow)]) %>% 
  mutate(SecDefDirThrow = case_when( displayName == SecondaryDefName & event3 == "pass_thrown" ~ 'yes' , 
                                       TRUE ~ 'no' )) %>% 
  mutate(SecDefDirThrow = dir[match('yes',SecDefDirThrow)]) %>% 
  # ^ Indicates critical player direction 
mutate(TargetOThrow = case_when( displayName == targetName & event3 == "pass_thrown" ~ 'yes' , 
                                   TRUE ~ 'no' )) %>% 
  mutate(TargetOThrow = o[match('yes',TargetOThrow)]) %>% 
  mutate(PrimDefOThrow = case_when( displayName == PrimaryDefName & event3 == "pass_thrown" ~ 'yes' , 
                                      TRUE ~ 'no' )) %>% 
  mutate(PrimDefOThrow = o[match('yes',PrimDefOThrow)]) %>% 
  mutate(SecDefOThrow = case_when( displayName == SecondaryDefName & event3 == "pass_thrown" ~ 'yes' , 
                                     TRUE ~ 'no' )) %>% 
  mutate(SecDefOThrow = o[match('yes',SecDefOThrow)]) 
# ^ Indicates critical player orientation 
  
  
# Task 
# Identify More Advanced Features relating Targeted Player, Primary/Secondary Defender 

# Generating variables indicating positions of ball, target, defenders at time of throw 
AnalysisData <- AnalysisData %>% # Generating variables indicating positions of ball, target, defenders at pass arrival 
  ungroup %>% group_by(gameId , playId) %>% 
  mutate(ball_throw_x = x[match('Football pass_thrown', paste(displayName, event3))]) %>% 
  mutate(ball_throw_y = y[match('Football pass_thrown', paste(displayName, event3))]) %>% 
  # ^ Identify Ball Position 
  mutate(targ_throw_x = case_when( displayName == targetName & event3 == "pass_thrown" ~ 'yes' , 
                                    TRUE ~ 'no')) %>% 
  mutate(targ_throw_x = x[match('yes' , targ_throw_x)]) %>% 
  mutate(targ_throw_y = case_when( displayName == targetName & event3 == "pass_thrown" ~ 'yes' , 
                                    TRUE ~ 'no')) %>% 
  mutate(targ_throw_y = y[match('yes' , targ_throw_y)]) %>% 
  # ^ Identify targeted player position 
  mutate(prim_throw_x = case_when( displayName == PrimaryDefName & event3 == "pass_thrown" ~ 'yes' , 
                                    TRUE ~ 'no' )) %>% 
  mutate(prim_throw_x = x[match('yes' , prim_throw_x)]) %>% 
  mutate(prim_throw_y = case_when( displayName == PrimaryDefName & event3 == "pass_thrown" ~ 'yes' , 
                                    TRUE ~ 'no' )) %>% 
  mutate(prim_throw_y = y[match('yes' , prim_throw_y)]) %>% 
  # ^ Identify Primary Defender position 
  mutate(sec_throw_x = case_when( displayName == SecondaryDefName & event3 == "pass_thrown" ~ 'yes' , 
                                   TRUE ~ 'no' )) %>% 
  mutate(sec_throw_x = x[match('yes' , sec_throw_x)]) %>% 
  mutate(sec_throw_y = case_when( displayName == SecondaryDefName & event3 == "pass_thrown" ~ 'yes' , 
                                   TRUE ~ 'no' )) %>% 
  mutate(sec_throw_y = y[match('yes' , sec_throw_y)])  
# ^ Identify Secondary Defender Position 

# Generating variables indicating positions of ball, target, defenders at pass arrival
AnalysisData <- AnalysisData %>% # Generating variables indicating positions of ball, target, defenders at pass arrival 
  ungroup %>% group_by(gameId , playId) %>% 
  mutate(ball_arrive_x = x[match('Football pass_arrived', paste(displayName, event3))]) %>% 
  mutate(ball_arrive_y = y[match('Football pass_arrived', paste(displayName, event3))]) %>% 
  # ^ Identify Ball Position 
  mutate(targ_arrive_x = case_when( displayName == targetName & event3 == "pass_arrived" ~ 'yes' , 
                                    TRUE ~ 'no')) %>% 
  mutate(targ_arrive_x = x[match('yes' , targ_arrive_x)]) %>% 
  mutate(targ_arrive_y = case_when( displayName == targetName & event3 == "pass_arrived" ~ 'yes' , 
                                    TRUE ~ 'no')) %>% 
  mutate(targ_arrive_y = y[match('yes' , targ_arrive_y)]) %>% 
  # ^ Identify targeted player position 
  mutate(prim_arrive_x = case_when( displayName == PrimaryDefName & event3 == "pass_arrived" ~ 'yes' , 
                                    TRUE ~ 'no' )) %>% 
  mutate(prim_arrive_x = x[match('yes' , prim_arrive_x)]) %>% 
  mutate(prim_arrive_y = case_when( displayName == PrimaryDefName & event3 == "pass_arrived" ~ 'yes' , 
                                    TRUE ~ 'no' )) %>% 
  mutate(prim_arrive_y = y[match('yes' , prim_arrive_y)]) %>% 
  # ^ Identify Primary Defender position 
  mutate(sec_arrive_x = case_when( displayName == SecondaryDefName & event3 == "pass_arrived" ~ 'yes' , 
                                    TRUE ~ 'no' )) %>% 
  mutate(sec_arrive_x = x[match('yes' , sec_arrive_x)]) %>% 
  mutate(sec_arrive_y = case_when( displayName == SecondaryDefName & event3 == "pass_arrived" ~ 'yes' , 
                                    TRUE ~ 'no' )) %>% 
  mutate(sec_arrive_y = y[match('yes' , sec_arrive_y)])  
  # ^ Identify Secondary Defender Position 

# Distance to eventual Ball Landing/Arrival Point at Time of Throw 
AnalysisData <- AnalysisData %>% 
  mutate( TargDist2BallLandThrow = sqrt( (targ_throw_x - ball_arrive_x)^2 + (targ_throw_y - ball_arrive_y)^2 )) %>% 
  mutate( PrimDefDist2BallLandThrow = sqrt( (prim_throw_x - ball_arrive_x)^2 + (prim_throw_y - ball_arrive_y)^2 )) %>% 
  mutate( SecDefDist2BallLandThrow = sqrt( (sec_throw_x - ball_arrive_x)^2 + (sec_throw_y - ball_arrive_y)^2 ))

# Angular Separation between Target & Primary/Secondary Defenders at Throw 
# At time of throw, consider the triangle defined by the QB, WR, and each defender

AnalysisData <- AnalysisData %>% 
  mutate(a1 = sqrt( (ball_throw_x - targ_throw_x)^2 + (ball_throw_y - targ_throw_y)^2) ) %>% # Target Player to Ball/QB 
  mutate(b1 = sqrt( (ball_throw_x - prim_throw_x)^2 + (ball_throw_y - prim_throw_y)^2) ) %>% # Primary Def. to Ball/QB 
  mutate(c1 = PrimDefDistThrow) %>% # Target Player to Primary Def. 
  mutate(PrimDefAngThrow = LawOfCosines(a1,b1,c1)) %>% # Angle between Primary Defender & Target Player 
  # a1 does not change 
  mutate(b1 = sqrt( (ball_throw_x - sec_throw_x)^2 + (ball_throw_y - sec_throw_y)^2) ) %>% # Secondary def. to Ball/QB 
  mutate(c1 = SecDefDistThrow) %>% # Target Played to Secondary Def. 
  mutate(SecDefAngThrow = LawOfCosines(a1,b1,c1)) # Angle between Secondary Defender & Target Player 
  
# Distance between Target & Primary/Secondary Defenders at Pass Arrival 
AnalysisData <- AnalysisData %>% # Obtaining variables for relevant distances 
  mutate(PrimDefDistArrive = 
           sqrt( (targ_arrive_x - prim_arrive_x)^2 + (targ_arrive_y - prim_arrive_y)^2 )) %>% 
  mutate(SecDefDistArrive = 
           sqrt( (targ_arrive_x - sec_arrive_x)^2 + (targ_arrive_y - sec_arrive_y)^2 ))

# Distance between Target & Primary/Secondary Defenders & Ball at Pass Arrival 
AnalysisData <- AnalysisData %>% 
  mutate( TargDist2BallLandArrive = sqrt( (targ_arrive_x - ball_arrive_x)^2 + (targ_arrive_y - ball_arrive_y)^2 )) %>% 
  mutate( PrimDefDist2BallLandArrive = sqrt( (prim_arrive_x - ball_arrive_x)^2 + (prim_arrive_y - ball_arrive_y)^2 )) %>% 
  mutate( SecDefDist2BallLandArrive = sqrt( (sec_arrive_x - ball_arrive_x)^2 + (sec_arrive_y - ball_arrive_y)^2 ))

# Angle between receiver at throw, defenders at throw, and ball landing spot 
AnalysisData <- AnalysisData %>% 
  mutate(a1 = TargDist2BallLandThrow) %>% # Ball (arrive) to Target Receiver (throw)
  mutate(b1 = PrimDefDist2BallLandThrow) %>% # Ball (arrive) to Primary Def. (throw)
  mutate(c1 = PrimDefDistThrow) %>% # Target Player to Primary Def. (throw)
  mutate(PrimBallRecAng = LawOfCosines(a1,b1,c1)) %>% # Primary Def.-Ball-Target Angle 
  # a1 stays the same 
  mutate(b1 = SecDefDist2BallLandThrow) %>% # Ball (arrive) to Secondary Def. (throw)
  mutate(c1 = SecDefDistThrow) %>% # Target Player to Secondary Def. (throw)
  mutate(SecBallRecAng = LawOfCosines(a1,b1,c1)) %>% # Secondary Def.-Ball-Target Angle 
  mutate(a1 = NULL) %>% mutate(b1 = NULL) %>% mutate(c1 = NULL) # Removing unnecessary variables 
  

# Task
# Target Depth/Throw Distance Variables 

AnalysisData <- AnalysisData %>% # Identifies location of ball at various points based on event3 
  group_by(gameId, playId) %>% 
  mutate(ball_snap_x = x[match('Football ball_snap' , paste(displayName, event3))]) %>%
  mutate(ball_snap_y = y[match('Football ball_snap' , paste(displayName, event3))]) 

AnalysisData <- AnalysisData %>% 
  mutate( ThrowDist = # Distance from throw to where ball lands or is contacted by receiver/defender
            sqrt( (ball_throw_x-ball_arrive_x)^2 + (ball_throw_y-ball_arrive_y)^2 )) %>% 
  mutate( Passer2RecDistThrow = # Determines QB-Rec dist when throw is made, assumes QB is where the ball is at throw 
            sqrt( (ball_throw_x - targ_throw_x)^2 + (ball_throw_y - targ_throw_y)^2 ) ) %>% 
  mutate( Passer2RecDistArrive =  # Determines QB-Rec dist when ball arrives, assumes QB is where the ball is at throw 
            sqrt( (ball_throw_x - targ_arrive_x)^2 + (ball_throw_y - targ_arrive_y)^2 ) ) %>% 
  mutate( TargDownfieldDistThrow = targ_throw_x - ball_snap_x ) %>% # Yards Downfield Receiver is at throw 
  mutate( TargDownfieldDistRec = targ_arrive_x - ball_snap_x ) # Yards Downfield Receiver is at arrival/outcome 


# Task 
# Pursuit Angle Feature 

AnalysisData <- AnalysisData %>% 
  mutate(est_flight_time = 0.4 + 0.001 * Passer2RecDistThrow^2 + 0.012 * Passer2RecDistThrow) %>% 
    # Approximates flight time based on various regression results 
  mutate(targ_future_x = targ_throw_x + est_flight_time * TargetSpeedThrow * cos(TargetDirThrow * pi/180)) %>% 
  mutate(targ_future_y = targ_throw_y + est_flight_time * TargetSpeedThrow * cos(TargetDirThrow * pi/180)) %>% 
  mutate(avgDev = sqrt( (targ_throw_x-targ_future_x)^2 + (targ_throw_y-targ_future_y)^2 ))


# Task 
# Removes Unnecessary Intermediate Variables 

AnalysisData <- AnalysisData %>% 
  mutate(ball_snap_x = NULL) %>% mutate(ball_snap_y = NULL) %>%  
  mutate(ball_throw_x = NULL) %>% mutate(ball_throw_y = NULL) %>% 
  mutate(ball_arrive_x = NULL) %>% mutate(ball_arrive_y = NULL) %>% 
  mutate(targ_arrive_x = NULL) %>% mutate(targ_arrive_y = NULL) %>% 
  mutate(targ_throw_x = NULL) %>% mutate(targ_throw_y = NULL) %>% 
  mutate(prim_throw_x = NULL) %>% mutate(prim_throw_y = NULL) %>% 
  mutate(prim_arrive_x = NULL) %>% mutate(prim_arrive_y = NULL) %>% 
  mutate(sec_arrive_x = NULL) %>% mutate(sec_arrive_y = NULL) %>% 
  mutate(targ_future_x = NULL) %>% mutate(targ_future_y = NULL) 


# Task
# Identify Play-Specific Factors of Interest 

AnalysisData <- AnalysisData %>% 
  group_by(gameId , playId) %>% 
  mutate(AnyShift = (sum(event == "shift") > 1)) %>%
  mutate(AnyMotion = (sum(event == "man_in_motion") > 1)) %>%
  mutate(AnyPlayAction = (sum(event == "play_action") > 1)) %>%
  mutate(AnyTipped = (sum(event == "pass_tipped") > 1)) %>%
  mutate(AnyFlag = (sum(event == "penalty_flag") > 1))


# Task 
# Identify Number of Times a Player is on the Field 

AnalysisData <- AnalysisData %>% 
  mutate(playerPlayCount = case_when(event3 == "ball_snap" ~ 1 , 
                                     TRUE ~ 0)) %>% 
  group_by(displayName) %>% 
  mutate(playerPlayCount = sum(playerPlayCount)) %>% 
  ungroup %>% group_by(playId , gameId) 


# Task 
# Import & Merge Weather Data 
weatherData <- read.csv("Data/weather_for_plays.csv")
weatherData <- weatherData %>% 
  select(gameId , playId , Temperature , Conditions) 

AnalysisData <- left_join(AnalysisData , weatherData) 


# Task 
# Incorporate Coverage ID 

coverages <- readRDS('Data/allWeeksCoverageID') 
coverages <- coverages %>% 
  select(gameId , playId , nflId , displayName , zone, Coverage , CoverageFamily)

AnalysisData <- left_join(AnalysisData , coverages)


# Task 
# Determine Primary/Secondary Defender Coverage Assignment 

AnalysisData <- AnalysisData %>% 
  mutate(PrimDefZone = case_when(displayName == PrimaryDefName & event3 == "ball_snap" ~ 'yes' , 
                                 TRUE ~ 'no')) %>% 
  mutate(PrimDefZone = zone[match('yes',PrimDefZone)]) %>% 
  mutate(SecDefZone = case_when(displayName == SecondaryDefName & event3 == "ball_snap" ~ 'yes' , 
                                 TRUE ~ 'no')) %>% 
  mutate(SecDefZone = zone[match('yes',SecDefZone)])

# Task
# Merge features with General Play data (plays.csv) & Create Extracted Features 

length(unique(AnalysisData$playId)) # Indicates number of different play IDs 
ExtractedFeatures <- AnalysisData %>% # Extracts 1 observation per play 
  group_by(gameId , playId) %>% 
  filter(displayName == BackfieldQB & event3 == "ball_snap") %>% 
  filter(is.na(targetName)==0) %>% # Removes players missing key variables 
  filter(is.na(PrimaryDefName)==0) 
length(unique(ExtractedFeatures$playId)) # Indicates number of different play IDs. Should be the same as previously. 

ExtractedFeatures <- left_join(ExtractedFeatures , plays) 

ExtractedFeatures <- ExtractedFeatures %>% 
  mutate(Complete = (passResult == "C")) %>% 
  mutate(Incomplete = (passResult == "I")) %>% 
  mutate(Interception = (passResult == "IN")) 


# Task 
# Simplifying Key Player Positions 

ExtractedFeatures <- ExtractedFeatures %>% 
  mutate(targetPosBasic = case_when( targetPosition == "RB" | targetPosition == "HB" | targetPosition == "FB" ~ "RB" , 
                                     TRUE ~ targetPosition )) %>% 
  mutate(PrimaryDefPosBasic = case_when(PrimaryDefPosition == "SS" | PrimaryDefPosition == "S" | PrimaryDefPosition == "FS" | 
                                          PrimaryDefPosition == "CB" | PrimaryDefPosition == "DB" ~ "DB" , 
                                        PrimaryDefPosition == "LB" | PrimaryDefPosition == "MLB" | 
                                            PrimaryDefPosition == "OLB" | PrimaryDefPosition == "ILB" ~ "LB" ,
                                        PrimaryDefPosition == "DL" ~ "DL" )) %>% 
  mutate(SecondaryDefPosBasic = case_when(SecondaryDefPosition == "SS" | SecondaryDefPosition == "S" | SecondaryDefPosition == "FS" | 
                                              SecondaryDefPosition == "CB" | SecondaryDefPosition == "DB" ~ "DB" , 
                                          SecondaryDefPosition == "LB" | SecondaryDefPosition == "MLB" | 
                                              SecondaryDefPosition == "OLB" | SecondaryDefPosition == "ILB" ~ "LB" , 
                                          SecondaryDefPosition == "DL" ~ "DL" ))


# Task 
# Mutate Final Extracted Features 

# Creating Delta Features 
ExtractedFeatures <- ExtractedFeatures %>% 
  filter(is.na(TargetDirThrow) == 0 & is.na(PrimDefDirThrow) == 0 & is.na(SecDefDirThrow) == 0) %>% 
  # Accounts for rare scenario where a player's tracker partly malfunctions 
  filter(is.na(PrimDefDistArrive)==0) %>% # Removes plays missing key variables (few & far in between) 
  mutate(PrimDeltaSpeedThrow = TargetSpeedThrow - PrimDefSpeedThrow) %>% 
  mutate(PrimDeltaAccThrow = TargetAccThrow - PrimDefAccThrow) %>% 
  mutate(PrimDeltaDirThrow = OrientationDiff(TargetDirThrow,PrimDefDirThrow)) %>% 
  mutate(SecDeltaSpeedThrow = TargetSpeedThrow - SecDefSpeedThrow) %>% 
  mutate(SecDeltaAccThrow = TargetAccThrow - SecDefAccThrow) %>% 
  mutate(SecDeltaDirThrow = OrientationDiff(TargetDirThrow,SecDefDirThrow)) 

# Time Pass in Air (& Square) 
ExtractedFeatures <- ExtractedFeatures %>% 
  mutate(TimePassInAir = (ArrivedFrame - PassFrame)/10) %>% 
  mutate(P2RDistThrowSq = Passer2RecDistThrow^2 )

# Player Controls -- Minimum Number of Targets 
ExtractedFeatures <- ExtractedFeatures %>% 
  ungroup %>% group_by(targetName) %>% 
  mutate(targetNameQ = 1) %>% mutate(targetNameQ = sum(targetNameQ)) %>% 
  mutate(targetNameQ = case_when(targetNameQ < 15 ~ "AAA" , 
                                 targetNameQ >= 15 ~ targetName)) %>% 
  ungroup %>% group_by(PrimaryDefName) %>% 
  mutate(PrimaryDefNameQ = 1) %>% mutate(PrimaryDefNameQ = sum(PrimaryDefNameQ)) %>% 
  mutate(PrimaryDefNameQ = case_when(PrimaryDefNameQ < 15 ~ "AAA" , 
                                     PrimaryDefNameQ >= 15 ~ PrimaryDefName)) %>% 
  ungroup %>% group_by(playId , gameId) 

# Fixing Primary Defender Man/Zone 
ExtractedFeatures <- ExtractedFeatures %>% 
  mutate(PrimDefZone = case_when(PrimDefZone == 1 ~ 'Zone' , 
                                 PrimDefZone == 0 ~ 'Man' , 
                                 TRUE ~ 'AAA-Other')) 

print("Feature Engineering 1 Completed") 
