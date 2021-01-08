### NFL Big Data Bowl 2020-21 
# Robert Bernhardt, Andrew Rogan, Daniel Weiss 
# November 2020 

## User-Defined Functions for Data Visualization 

# Plot Play-Specific Data 


## Data Visualization Preparation Source Function 
# Required Inputs: TrackingPlayData , input_game , input_play 
# Outputs: Offense , Defense , Ball , BallX , FirstDownLine 

prep_vis_data <- function(GeneralPlayData, TrackingPlayData) {
  
  TrackingPlayData <- TrackingPlayData %>% 
    mutate( x =      case_when(  playDirection == "right" ~ x ,
                                 playDirection == "left" ~ 120 - x)) %>% 
    mutate( y =      case_when(  playDirection == "right" ~ y ,
                                 playDirection == "left" ~ 160/3 - y)) %>% 
    mutate( dir =    case_when(  playDirection == "right" ~ dir ,
                                 playDirection == "left" ~ mod(dir+180,360))) %>% 
    mutate( o =      case_when(  playDirection == "right" ~ o ,
                                 playDirection == "left" ~ mod(o+180,360)))
  
  qb_team <- TrackingPlayData %>% 
    filter(position == "QB") %>% 
    filter(frameId == 1) %>% 
    select(team)
  
  Offense <- TrackingPlayData %>% 
    filter(team == as.character(qb_team))
  
  Defense <- TrackingPlayData %>% 
    filter(team != as.character(qb_team)) %>% 
    filter(team != "football")
  
  Ball <- TrackingPlayData %>% 
    filter(team == "football") 
  
  BallX <- Ball %>% 
    filter(frameId == 1) %>%
    select(x)  
  
  YardsToGo <- GeneralPlayData %>% 
    select(yardsToGo)
  FirstDownLine <- BallX + YardsToGo
  
  vis_data <- list("Offense" = Offense, "Defense" = Defense, "Ball" = Ball, "BallX" = BallX, "FirstDownLine" = FirstDownLine)
  return(vis_data)
  
}

# 

## Throw-Based Preparation Source Function 
# Required Inputs: input_data , input_game , input_play 
# Outputs: Offense_throw , Defense_throw , Ball_throw , BallX , FirstDownLine 

# Identifying Players , Play Information

prep_throw_vis_data <- function(GeneralPlayData , TrackingPlayData , PlayerTargeted)  {
  
  TrackingPlayData <- TrackingPlayData %>% 
    mutate( x =      case_when(  playDirection == "right" ~ x ,
                                 playDirection == "left" ~ 120 - x)) %>% 
    mutate( y =      case_when(  playDirection == "right" ~ y ,
                                 playDirection == "left" ~ 160/3 - y)) %>% 
    mutate( dir =    case_when(  playDirection == "right" ~ dir ,
                                 playDirection == "left" ~ mod(dir+180,360))) %>% 
    mutate( o =      case_when(  playDirection == "right" ~ o ,
                                 playDirection == "left" ~ mod(o+180,360)))
  
  qb_team <- TrackingPlayData %>% 
    filter(position == "QB") %>% 
    filter(frameId == 1) %>% 
    select(team)
  
  # Identifying Critical Moment  
  
  if (sum(TrackingPlayData$event == "pass_forward") > 0) { 
    critical_moment <- "pass_forward" 
  }   else if (sum(TrackingPlayData$event == "pass_shovel") > 0) { 
    critical_moment <- "pass_shovel" 
  }   else if (sum(TrackingPlayData$event == "qb_spike") > 0) { 
    critical_moment <- "qb_spike" 
  }   else if (sum(TrackingPlayData$event == "qb_sack") > 0) { 
    critical_moment <- "qb_sack" 
  }   else if (sum(TrackingPlayData$event == "qb_strip_sack") > 0) { 
    critical_moment <- "qb_strip_sack" 
  }   else if (sum(TrackingPlayData$event == "tackle") > 0) { 
    critical_moment <- "tackle" 
  } 
  
  critical_frame <- TrackingPlayData %>% 
    filter(event == critical_moment )
  
  # Identifying Targeted Player 
  
  target_player_location <- critical_frame %>% 
    filter(nflId == PlayerTargeted$targetNflId) %>% 
    select(x , y , s , a , o , dis , nflId , displayName)
  
  if (is.na(PlayerTargeted$targetNflId) | nrow(target_player_location) == 0) { 
    ball_arrive <- critical_frame %>% 
      filter(displayName == "Football") 
    
    target_player <- critical_frame %>% 
      mutate(ballDistance = sqrt( (as.numeric(critical_frame$x) - as.numeric(ball_arrive$x))^2 + (as.numeric(critical_frame$y) - as.numeric(ball_arrive$y))^2 )) %>%        
      filter(displayName != "Football") %>% 
      filter(team == qb_team$team) %>% 
      filter(ballDistance == min(ballDistance)) %>% 
      select(nflId , displayName)
    
    target_player_location <- critical_frame %>% 
      filter(nflId == target_player$nflId) %>% 
      select(x , y , s , a , o , dis , nflId , displayName)
    }
  
  # Preparing Offense/Defense/Ball Data at Critical Moment  
  
  Offense_throw <- TrackingPlayData %>% 
    filter(team == as.character(qb_team)) %>% 
    filter(event == critical_moment) 
  
  Defense_throw <- TrackingPlayData %>% 
    filter(team != as.character(qb_team)) %>% 
    filter(team != "football") %>% 
    filter(event == critical_moment) 
  
  Ball_throw <- TrackingPlayData %>% 
    filter(team == "football") %>% 
    filter(event == critical_moment) 
  
  BallX <- TrackingPlayData %>% 
    filter(team == "football") %>%
    filter(frameId == 1) %>% 
    select(x)  
  
  YardsToGo <- GeneralPlayData %>% 
    select(yardsToGo)
  FirstDownLine <- BallX + YardsToGo
  
  # Generating Function Output 
  
  throw_vis_data <- list("Offense_throw" = Offense_throw, "Defense_throw" = Defense_throw, 
                         "Ball_throw" = Ball_throw, "BallX" = BallX, "FirstDownLine" = FirstDownLine, 
                         "target_player_location" = target_player_location)
  
  return(throw_vis_data)
}

# 

## Adds Shell ID Visualization 

prep_shell_vis <- function(TrackingPlayData){ 
  
  dat1 <- TrackingPlayData
  
  #flip x,y,direction, orientation
  dat1 <- dat1 %>% 
    mutate(x = case_when(playDirection == "right" ~ x , 
                         playDirection == "left" ~ 120 - x)) %>%
    mutate(y = case_when(playDirection == "right" ~ y , 
                         playDirection == "left" ~ 53.33 - y)) %>%
    mutate(o = case_when(playDirection == "right" ~ o,
                         playDirection == "left" ~ mod(o+180, 360))) %>%
    mutate(dir = case_when(playDirection == "right" ~ dir,
                           playDirection == "left" ~ mod(dir+180, 360)))
  
  ##ball location
  #y
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(BallSnapY = y[match('Football ball_snap', paste(displayName, event))]) %>% 
    ungroup
  
  #x
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(BallSnapX = x[match('Football ball_snap', paste(displayName, event))]) %>% 
    ungroup
  
  ###Deleting problematic plays/frames
  
  #week 2
  dat1 <- dat1 %>% 
    filter(!(gameId == 2018091605 & playId == 2715))
  
  #week 3
  dat1 <- dat1 %>% 
    filter(!(gameId == 2018092301))
  
  #week 4
  dat1 <- dat1 %>% 
    filter(!(gameId == 2018093011))
  
  ##week 14
  #fix week 14 issue where there are two different frames with ball_snap
  dat1$event[dat1$gameId == 2018120905 & dat1$playId == 1426 & dat1$event == 'ball_snap' & dat1$frameId == 12] = 'None'
  #two frame 92s in week 14
  dat1 <- dat1 %>% 
    filter(!(gameId == 2018120905 & playId == 1426))
  
  #week 15
  dat1 <- dat1 %>% 
    filter(!(gameId == 2018121605))
  
  #week 16
  dat1 <- dat1 %>% 
    filter(!(gameId == 2018123001 & playId == 435))
  dat1 <- dat1 %>% 
    filter(!(gameId == 2018123006))
  dat1 <- dat1 %>% 
    filter(!(gameId == 2018123000 & playId == 131))
  
  #snap frame
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%  
    mutate(SnapFrame = frameId[match('ball_snap', event)])
  
  #is it the snap frame?
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%  
    mutate(isSnapFrame = case_when(frameId==SnapFrame ~ 1,
                                   TRUE ~0))
  #frames since the snap
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%  
    mutate(FramesSinceSnap = frameId - SnapFrame)
  
  ##side of player relative to spot where ball is snapped from (changes continuously)
  dat1 <- dat1 %>% 
    mutate(SideDuring = case_when(BallSnapY < y ~ "Left", 
                                  BallSnapY > y ~ "Right",
                                  BallSnapY == y ~ "OTB"))
  
  #side at the snap (should be SideDuring for that player when the ball was snapped)
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>%
    mutate(SideSnap = case_when(event == 'ball_snap' & y >= BallSnapY ~ 'Left', 
                                event == 'ball_snap' & y < BallSnapY ~ 'Right', 
                                event == 'ball_snap' & y == BallSnapY ~ 'Center')) %>%
    mutate(SideSnap = replace(SideSnap, event != 'ball_snap', SideSnap[event == 'ball_snap'])) %>%
    ungroup
  
  ##eliminate plays in opposing redzone
  dat1 <- filter(dat1, BallSnapX < 90)
  #needs to be 90 to account for left endzone
  
  ##location of each player at snap
  #Y
  dat1 <- dat1 %>% 
    group_by(displayName, gameId, playId) %>%
    mutate(SnapY = y[event == "ball_snap"]) %>%
    ungroup
  
  #X
  dat1 <- dat1 %>% 
    group_by(displayName, gameId, playId) %>%
    mutate(SnapX = x[event == "ball_snap"]) %>%
    ungroup
  
  #ID correct QB
  dat1 <- dat1 %>% 
    group_by(gameId , playId) %>% 
    mutate(ball_snap_y = y[match('Football ball_snap' , paste(displayName, event))]) %>%
    mutate(ydist2ball_snap = abs(SnapY - BallSnapY)) %>%
    mutate( isBackfieldQB = case_when(position == "QB" & ydist2ball_snap < 2 ~ 1, 
                                      TRUE ~ 0)) %>% 
    mutate(ball_snap_y = NULL) %>% mutate(dist2ball_snap = NULL) 
  
  #backfield count
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(BackfieldQBCount = sum(isBackfieldQB)) 
  
  dattest <- filter(dat1, BackfieldQBCount == 0 & FramesSinceSnap==0)
  
  #eliminate no QB under center 
  dat1 <- filter(dat1, BackfieldQBCount != 0)
  
  #eliminate 2 QBs under center
  dat1 <- filter(dat1, BackfieldQBCount != 2)
  
  ##is player on offense or defense
  #create qb team                        
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>% 
    mutate(qb_team = team[match(1, isBackfieldQB)]) %>%
    ungroup
  
  #is player on QB's team?
  dat1 <- dat1 %>%
    mutate(OffDef = case_when(team == qb_team ~ "Offense",
                              team != qb_team ~ "Defense")) %>%
    mutate(OffDef = case_when(displayName != "Football" ~ OffDef,
                              displayName == "Football" ~ "Football"))
  ##delete plays where  QB isn't directly behind center
  
  #create QB team
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>%
    mutate(QBTeam = case_when(isBackfieldQB == 1 ~ "QB",
                              TRUE ~ OffDef))
  
  ##number receivers on each side
  dat1 <- dat1 %>% 
    group_by(gameId, playId, QBTeam) %>% 
    mutate(Num = local({
      fsnap <- isSnapFrame == 1
      y <- y[fsnap]
      left <- SideSnap[fsnap] == "Left"
      right <- !left
      x <- integer(length(y))
      names(x) <- displayName[fsnap]
      x[left] <- rank(-y[left], ties.method = "min")
      x[right] <- rank(y[right], ties.method = "min")
      unname(x[displayName])
    }))
  
  ###location of each player at various frames
  # delete plays less than 1.5 seconds
  #dat1 <- dat1 %>%
  #group_by(gameId, playId) %>%  mutate(MaxFrame = max(frameId)) %>%
  #filter(MaxFrame >= 26)
  
  # get rid of plays where pass happens before frame 26
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%  
    mutate(ThrowFrame = frameId[match('pass_forward', event)]) %>%
    mutate(ShovelThrowFrame = frameId[match('pass_shovel', event)]) %>%
    filter(ThrowFrame >= 26 | is.na(ThrowFrame) | ShovelThrowFrame >= 26)
  
  # get rid of plays where theres not 15 frames after snap
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%  
    mutate(MaxFramesSinceSnap = max(FramesSinceSnap)) %>%
    filter(MaxFramesSinceSnap >= 15)
  
  ##half-second into the play
  #Y
  dat1 <- dat1 %>% 
    group_by(displayName, gameId, playId) %>%
    mutate(HalfSecondY = y[FramesSinceSnap == 5]) %>%
    ungroup
  
  #X
  dat1 <- dat1 %>% 
    group_by(displayName, gameId, playId) %>%
    mutate(HalfSecond = x[FramesSinceSnap == 5]) %>%
    ungroup
  
  ##8/10s of second into the play
  #Y
  dat1 <- dat1 %>% 
    group_by(displayName, gameId, playId) %>%
    mutate(EightTenthsY = y[FramesSinceSnap == 8]) %>%
    ungroup
  #X
  dat1 <- dat1 %>% 
    group_by(displayName, gameId, playId) %>%
    mutate(EightTenthsX = x[FramesSinceSnap == 8]) %>%
    ungroup
  
  ##1.5 seconds into the play
  #Y
  dat1 <- dat1 %>% 
    group_by(displayName, gameId, playId) %>%
    mutate(SecondAndHalfY = y[FramesSinceSnap == 15]) %>%
    ungroup
  #X
  dat1 <- dat1 %>% 
    group_by(displayName, gameId, playId) %>%
    mutate(SecondAndHalfX = x[FramesSinceSnap == 15]) %>%
    ungroup
  
  #new db position name
  dat1 <- dat1 %>% 
    mutate(DBPos = case_when(position == "DB" |position == "CB"| position == "FS" | position == "SS" |position == "S" ~ "DB",
                             position == "MLB" |position == "LB" | position == "OLB" |position == "ILB"~"LB",
                             TRUE ~ position))
  
  ##L and R
  dat1 <- dat1%>%
    mutate(LR = case_when(SideSnap == "Left" ~ "L",
                          SideSnap == "Right" ~ "R",
                          SideSnap == "OTB" ~ "C"))
  ##paste
  dat1$ReceiverNumber <- paste(dat1$LR, dat1$Num, sep="")
  ##paste again
  dat1$ReceiverNumber <- paste(dat1$ReceiverNumber, dat1$QBTeam, sep="")
  
  
  #unique playid
  dat1 <- dat1 %>%
    mutate(uniqueplay = (gameId*1000)+ playId)
  
  ###location of each player by number
  ##Left
  #L1 
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(L1OffY = y[match('L1Offense ball_snap', paste(ReceiverNumber, event))]) %>%
    ungroup
  
  #L2 
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(L2OffY = y[match('L2Offense ball_snap', paste(ReceiverNumber, event))]) %>%
    ungroup
  
  #L3 
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(L3OffY = y[match('L3Offense ball_snap', paste(ReceiverNumber, event))]) %>%
    ungroup
  
  #L4 
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(L4OffY = y[match('L4Offense ball_snap', paste(ReceiverNumber, event))]) %>%
    ungroup
  
  ##Right  
  #R1
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(R1OffY = y[match('R1Offense ball_snap', paste(ReceiverNumber, event))]) %>%
    ungroup
  
  #R2
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(R2OffY = y[match('R2Offense ball_snap', paste(ReceiverNumber, event))]) %>%
    ungroup
  
  #R3
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(R3OffY = y[match('R3Offense ball_snap', paste(ReceiverNumber, event))]) %>%
    ungroup
  
  #R4
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(R4OffY = y[match('R4Offense ball_snap', paste(ReceiverNumber, event))]) %>%
    ungroup
  
  ## creating column
  #L1 - L2
  dat1 <- dat1 %>%
    mutate(L1L2Diff = L1OffY - L2OffY)
  
  #R1 - R2 (opposite--has to be R2-R1)
  dat1 <- dat1 %>%
    mutate(R1R2Diff = R2OffY - R1OffY)
  
  ##create bounds of field
  #left side
  dat1 <- dat1 %>% 
    mutate(LeftSideline = 53.33)
  
  #right side
  dat1 <- dat1 %>% 
    mutate(RightSideline = 0)
  
  ##drop obs where there's no L1 or R1
  dat1 <- dat1[!is.na(dat1$L1OffY), ]
  dat1 <- dat1[!is.na(dat1$R1OffY), ]
  
  
  ##Create column lines
  #left
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(LeftColumnLine = case_when(is.na(L1L2Diff) ~ (L1OffY + BallSnapY + 4) * .5,
                                      L1L2Diff > 3 ~ L1OffY - 1.5,
                                      L1L2Diff <=3 ~ (L1OffY-L2OffY)*.75 + L2OffY))
  
  #right
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(RightColumnLine = case_when(is.na(R1R2Diff) ~ (R1OffY + BallSnapY - 4) * .5,
                                       R1R2Diff > 3 ~ R1OffY + 1.5,
                                       R1R2Diff <=3 ~ (R2OffY-R1OffY)*.25 + R1OffY))
  
  ##account for tight splits 
  #create indicator 
  #left
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(TightSplitLeft = case_when(L1OffY <= BallSnapY + 6 ~ 1,
                                      TRUE ~ 0))
  #right
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(TightSplitRight = case_when(R1OffY >= BallSnapY - 6 ~ 1,
                                       TRUE ~ 0))
  ##change column line accordingly                                   
  #left
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(LeftColumnLine = case_when(TightSplitLeft==1 & L1L2Diff >=1 ~ L1OffY - .5,
                                      TRUE ~ LeftColumnLine))
  
  #right
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(RightColumnLine = case_when(TightSplitRight==1 & R1R2Diff >=1 ~ R1OffY + .5,
                                       TRUE ~ RightColumnLine))
  
  ##create column indicator
  #left
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(LeftColumn = case_when(SnapY >= LeftColumnLine & SnapY < LeftSideline & OffDef=="Defense" ~ 1,
                                  TRUE ~ 0))
  #right
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(RightColumn = case_when(SnapY <= RightColumnLine & SnapY > RightSideline & OffDef=="Defense" ~ 1,
                                   TRUE ~ 0))
  
  
  ##column count(all positions)  
  #left
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(LeftColumnCount = sum(LeftColumn)) 
  
  #right
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(RightColumnCount = sum(RightColumn)) 
  
  
  ##account for when column player is a little more than a yard inside number 1 
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(LeftColumnLine = case_when(LeftColumnCount==0 & TightSplitLeft == 0 & L1L2Diff > 4 ~ L1OffY - 2.5,
                                      TRUE ~ LeftColumnLine))
  
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(RightColumnLine = case_when(RightColumnCount==0 & TightSplitRight == 0 & R1R2Diff > 4 ~ R1OffY + 2.5,
                                       TRUE ~ RightColumnLine))
  
  ##redo column indicator
  #left
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(LeftColumn = case_when(SnapY >= LeftColumnLine & SnapY < LeftSideline & OffDef=="Defense" ~ 1,
                                  TRUE ~ LeftColumn))
  #right
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(RightColumn = case_when(SnapY <= RightColumnLine & SnapY > RightSideline & OffDef=="Defense" ~ 1,
                                   TRUE ~ RightColumn))
  
  ###update column
  ##eliminate blitzers
  #left
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(LeftColumn = case_when(SecondAndHalfX < BallSnapX ~ 0,
                                  TRUE ~ LeftColumn))
  #right
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(RightColumn = case_when(SecondAndHalfX < BallSnapX ~ 0,
                                   TRUE ~RightColumn))
  ##Eliminate DL in column
  #left
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(LeftColumn = case_when(LeftColumn == 1 & position == 'DL' ~ 0,
                                  TRUE ~ LeftColumn))
  #right
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(RightColumn = case_when(RightColumn == 1 & position == 'DL' ~ 0,
                                   TRUE ~ RightColumn))
  
  ##if there is a tight split, eliminate LBs 
  #left
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(LeftColumn = case_when(L1OffY <= BallSnapY + 6 & DBPos =="LB" & LeftColumnCount > 1~ 0,
                                  TRUE ~ LeftColumn))
  #right
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(RightColumn = case_when(R1OffY >= BallSnapY - 6 & DBPos =="LB" & RightColumnCount > 1 ~ 0,
                                   TRUE ~ RightColumn))
  
  #redo column count
  #left
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(LeftColumnCount = sum(LeftColumn))
  #right
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(RightColumnCount = sum(RightColumn))
  
  ##count number of DBs in column (just DBs)
  #left
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(LeftColumnDBCount = sum(DBPos=="DB" & LeftColumn==1))
  
  #right
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(RightColumnDBCount = sum(DBPos=="DB" & RightColumn==1))
  

  ## alley 
  #L2 - L3
  dat1 <- dat1 %>%
    mutate(L2L3Diff = L2OffY - L3OffY)
  
  #R2 - R3 (opposite--has to be R3-R2)
  dat1 <- dat1 %>%
    mutate(R2R3Diff = R3OffY - R2OffY)
  
  ##is #2 inside tackle?
  #left
  dat1 <- dat1 %>%
    mutate(L2OffInsideOT = case_when(is.na(L2OffY) ~ 0,
                                     L2OffY < (BallSnapY + 4) ~ 1,
                                     L2OffY >= (BallSnapY + 4) ~ 0))
  #right
  dat1 <- dat1 %>%
    mutate(R2OffInsideOT = case_when(is.na(R2OffY) ~ 0,
                                     R2OffY < (BallSnapY - 4) ~ 1,
                                     R2OffY >= (BallSnapY - 4) ~ 0))
  
  #what number is the QB
  dat1 <- dat1 %>%
    mutate(ReceiverNumber  = case_when(ReceiverNumber == "L2Offense" & isBackfieldQB == 1 ~ "QB",
                                       ReceiverNumber == "L3Offense" & isBackfieldQB == 1 ~ "QB",
                                       ReceiverNumber == "L4Offense" & isBackfieldQB == 1 ~ "QB",
                                       ReceiverNumber == "L5Offense" & isBackfieldQB == 1 ~ "QB",
                                       ReceiverNumber == "L6Offense" & isBackfieldQB == 1 ~ "QB",
                                       ReceiverNumber == "R2Offense" & isBackfieldQB == 1 ~ "QB",
                                       ReceiverNumber == "R3Offense" & isBackfieldQB == 1 ~ "QB",
                                       ReceiverNumber == "R4Offense" & isBackfieldQB == 1 ~ "QB",
                                       ReceiverNumber == "R5Offense" & isBackfieldQB == 1 ~ "QB",
                                       ReceiverNumber == "R6Offense" & isBackfieldQB == 1 ~ "QB",
                                       TRUE ~ ReceiverNumber))
  
  
  #Receiver count by side 
  dat1$LeftReceiverCount <- rowSums(!is.na(dat1[c('L1OffY', 'L2OffY', 'L3OffY', "L4OffY")]))
  dat1$RightReceiverCount <- rowSums(!is.na(dat1[c('R1OffY', 'R2OffY', 'R3OffY', "R4OffY")]))
  
  ##ID players inside tight end
  #Left
  #L1 inside tight?
  dat1 <- dat1 %>%
    mutate(L1InsideTE = case_when(L1OffY <= (BallSnapY + 4) ~ 1,
                                  L1OffY > (BallSnapY + 4) ~ 0))
  #L2 inside tight?
  dat1 <- dat1 %>%
    mutate(L2InsideTE = case_when(is.na(L2OffY) ~ 0,
                                  L2OffY <= (BallSnapY + 4) ~ 1,
                                  L2OffY > (BallSnapY + 4) ~ 0))
  #L3 inside tight?
  dat1 <- dat1 %>%
    mutate(L3InsideTE = case_when(is.na(L2OffY) ~ 0,
                                  is.na(L3OffY) ~ 0,
                                  L3OffY <= (BallSnapY + 4) ~ 1,
                                  L3OffY > (BallSnapY + 4) ~ 0))
  #L4 inside tight?
  dat1 <- dat1 %>%
    mutate(L4InsideTE = case_when(is.na(L2OffY) ~ 0,
                                  is.na(L3OffY) ~ 0,
                                  is.na(L4OffY) ~ 0,
                                  L4OffY <= (BallSnapY + 4) ~ 1,
                                  L4OffY > (BallSnapY + 4) ~ 0))
  #Right
  #R1 inside tight?
  dat1 <- dat1 %>%
    mutate(R1InsideTE = case_when(R1OffY >= (BallSnapY - 4) ~ 1,
                                  R1OffY < (BallSnapY - 4) ~ 0))
  
  #R2 inside tight?
  dat1 <- dat1 %>%
    mutate(R2InsideTE = case_when(is.na(R2OffY) ~ 0,
                                  R2OffY >= (BallSnapY - 4) ~ 1,
                                  R2OffY < (BallSnapY - 4) ~ 0))
  
  #R3 inside tight?
  dat1 <- dat1 %>%
    mutate(R3InsideTE = case_when(is.na(R2OffY) ~ 0,
                                  is.na(R3OffY) ~ 0,
                                  R3OffY >= (BallSnapY - 4) ~ 1,
                                  R3OffY < (BallSnapY - 4) ~ 0))
  
  #R4 inside tight?
  dat1 <- dat1 %>%
    mutate(R4InsideTE = case_when(is.na(R2OffY) ~ 0,
                                  is.na(R3OffY) ~ 0,
                                  is.na(R4OffY) ~ 0,
                                  R4OffY >= (BallSnapY - 4) ~ 1,
                                  R4OffY < (BallSnapY - 4) ~ 0))
  
  #drop plays where number 1 is inside tackle
  dat1 <- filter(dat1, L1InsideTE != 1)
  dat1 <- filter(dat1, R1InsideTE != 1)
  
  
  
  ###subtract count if inside tackle
  #Left
  #L4
  dat1 <- dat1 %>%
    mutate(LeftReceiverCountOutsideTackle = LeftReceiverCount - L4InsideTE)
  #L3
  dat1$LeftReceiverCountOutsideTackle <- dat1$LeftReceiverCountOutsideTackle - dat1$L3InsideTE
  #L2
  dat1$LeftReceiverCountOutsideTackle <- dat1$LeftReceiverCountOutsideTackle - dat1$L2InsideTE
  #L1
  dat1$LeftReceiverCountOutsideTackle <- dat1$LeftReceiverCountOutsideTackle - dat1$L1InsideTE
  #Right
  #R4
  dat1 <- dat1 %>%
    mutate(RightReceiverCountOutsideTackle = RightReceiverCount - R4InsideTE)
  #R3
  dat1$RightReceiverCountOutsideTackle <- dat1$RightReceiverCountOutsideTackle - dat1$R3InsideTE
  #R2
  dat1$RightReceiverCountOutsideTackle <- dat1$RightReceiverCountOutsideTackle - dat1$R2InsideTE
  #R1
  dat1$RightReceiverCountOutsideTackle <- dat1$RightReceiverCountOutsideTackle - dat1$R1InsideTE
  
  #Create indicator if first player outside is outside 5 yards from ball laterally
  #left
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(LeftAlleyID = case_when(LeftReceiverCountOutsideTackle == 1 & L1OffY > (BallSnapY + 5) ~ 'L1Plus5',
                                   LeftReceiverCountOutsideTackle == 1 & L1OffY <= (BallSnapY + 5) ~ 'L1Minus5',
                                   LeftReceiverCountOutsideTackle == 2 & L2OffY > (BallSnapY + 5) ~ 'L2Plus5',
                                   LeftReceiverCountOutsideTackle == 2 & L2OffY <= (BallSnapY + 5) ~ 'L2Minus5',
                                   LeftReceiverCountOutsideTackle == 3 & L3OffY > (BallSnapY + 5) ~ 'L3Plus5',
                                   LeftReceiverCountOutsideTackle == 3 & L3OffY <= (BallSnapY + 5) ~ 'L3Minus5',
                                   LeftReceiverCountOutsideTackle == 4 & L3OffY > (BallSnapY + 5) ~ 'L4Plus5',
                                   LeftReceiverCountOutsideTackle == 4 & L3OffY <= (BallSnapY + 5) ~ 'L4Minus5'))
  
  #right #NOTE--"plus 5" means receiver is more than five yards from the ball 
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(RightAlleyID = case_when(RightReceiverCountOutsideTackle == 1 & R1OffY < (BallSnapY - 5) ~ 'R1Plus5',
                                    RightReceiverCountOutsideTackle == 1 & R1OffY >= (BallSnapY - 5) ~ 'R1Minus5',
                                    RightReceiverCountOutsideTackle == 2 & R2OffY < (BallSnapY - 5) ~ 'R2Plus5',
                                    RightReceiverCountOutsideTackle == 2 & R2OffY >= (BallSnapY - 5) ~ 'R2Minus5',
                                    RightReceiverCountOutsideTackle == 3 & R3OffY < (BallSnapY - 5) ~ 'R3Plus5',
                                    RightReceiverCountOutsideTackle == 3 & R3OffY >= (BallSnapY - 5) ~ 'R3Minus5',
                                    RightReceiverCountOutsideTackle == 4 & R3OffY < (BallSnapY - 5) ~ 'R4Plus5',
                                    RightReceiverCountOutsideTackle == 4 & R3OffY >= (BallSnapY - 5) ~ 'R4Minus5'))
  
  ##Create alley lines
  #left
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(LeftAlleyLine = case_when(LeftAlleyID == 'L1Plus5' ~ BallSnapY + 5,
                                     LeftAlleyID == 'L1Minus5' ~ (L1OffY + BallSnapY + 4)* .5,
                                     LeftAlleyID == 'L2Plus5' ~ BallSnapY + 5,
                                     LeftAlleyID == 'L2Minus5' ~ (L2OffY + (BallSnapY + 4))*.5,
                                     LeftAlleyID == 'L3Plus5' ~ BallSnapY + 5,
                                     LeftAlleyID == 'L3Minus5' ~ (L3OffY + (BallSnapY + 4))*.5,
                                     LeftAlleyID == 'L4Plus5' ~ BallSnapY + 5,
                                     LeftAlleyID == 'L4Minus5' ~ (L4OffY + (BallSnapY + 4))*.5))
  
  #right
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(RightAlleyLine = case_when(RightAlleyID == 'R1Plus5' ~ BallSnapY - 5,
                                      RightAlleyID == 'R1Minus5' ~ (R1OffY + (BallSnapY - 4))* .5,
                                      RightAlleyID == 'R2Plus5' ~ BallSnapY - 5,
                                      RightAlleyID == 'R2Minus5' ~ (R2OffY + (BallSnapY - 4))*.5,
                                      RightAlleyID == 'R3Plus5' ~ BallSnapY - 5,
                                      RightAlleyID == 'R3Minus5' ~ (R3OffY + (BallSnapY - 4))*.5,
                                      RightAlleyID == 'R4Plus5' ~ BallSnapY - 5,
                                      RightAlleyID == 'R4Minus5' ~ (R4OffY + (BallSnapY - 4))*.5))
  
  
  #replace obs where alley line is wider than L1 or L2  
  #left
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(LeftAlleyLine = case_when(LeftAlleyLine > LeftColumnLine ~ LeftColumnLine,
                                     TRUE ~ LeftAlleyLine))
  
  #right
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(RightAlleyLine = case_when(RightAlleyLine < RightColumnLine ~ RightColumnLine,
                                      TRUE ~ RightAlleyLine))                                    
  
  #create alley indicator
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(LeftAlley = case_when(SnapY < LeftColumnLine & SnapY >= LeftAlleyLine & OffDef=="Defense" ~ 1,
                                 TRUE ~ 0))
  #right
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(RightAlley = case_when(SnapY > RightColumnLine & SnapY <= RightAlleyLine & OffDef == "Defense" ~ 1,
                                  TRUE ~ 0))
  
  ###identify chute
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(Chute = case_when(SnapY < LeftColumnLine & SnapY > RightColumnLine & OffDef == "Defense" ~ 1,
                             TRUE ~ 0))
  
  #indicate highest DB in column
  #left
  dat1 <- dat1 %>%
    arrange(gameId, playId, frameId, LeftColumn != 1, DBPos != 'DB', desc(SnapX)) %>%
    group_by(gameId, playId, frameId) %>%
    mutate(LeftColumnHighest = c(1, rep(0, n() - 1)))
  
  #right
  dat1 <- dat1 %>%
    arrange(gameId, playId, frameId, RightColumn != 1, DBPos != 'DB', desc(SnapX)) %>%
    group_by(gameId, playId, frameId) %>%
    mutate(RightColumnHighest = c(1, rep(0, n() - 1)))
  
  
  ##include safeties in the column
  #left
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(Chute = case_when(LeftColumnDBCount==2 & LeftColumnHighest ==1 & SnapX > BallSnapX + 7 ~ 1,
                             TRUE ~ Chute))
  
  #right
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(Chute = case_when(RightColumnDBCount==2 & RightColumnHighest ==1 & SnapX > BallSnapX + 7 ~ 1,
                             TRUE ~ Chute))
  
  #name these players "columnchutes"
  #left
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(LeftColumnChute = case_when(LeftColumn == 1 & Chute == 1 ~ 1,
                                       TRUE ~ 0))
  #right
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(RightColumnChute = case_when(RightColumn==1 & Chute ==1 ~ 1,
                                        TRUE ~ 0))
  
  #####defining the window
  ## define two highest DBs 
  dat1 <- dat1 %>%
    arrange(gameId, playId, frameId, Chute != 1,DBPos != 'DB', desc(SnapX)) %>%
    group_by(gameId, playId, frameId) %>%
    mutate(Highest = c('A','B', rep('-', n() - 2)))
  
  #new columns for higher DB
  #Y
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>% 
    mutate(HighDBY = EightTenthsY[match('A', Highest)]) %>%
    ungroup
  #X
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>% 
    mutate(HighDBX = EightTenthsX[match('A', Highest)]) %>%
    ungroup
  
  #new column for lower DB
  #Y
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>% 
    mutate(LowDBY = EightTenthsY[match('B', Highest)]) %>%
    ungroup
  #X
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>% 
    mutate(LowDBX = EightTenthsX[match('B', Highest)]) %>%
    ungroup

  #
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(Highest = case_when(Highest =="A" & LowDBX > HighDBX ~ "B",
                               Highest =="B" & LowDBX > HighDBX ~ "A", 
                               TRUE ~ Highest))
  
  ## account for observations where the safeties flipped depth
  #Y
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>% 
    mutate(HighDBY = EightTenthsY[match('A', Highest)]) %>%
    ungroup
  #X
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>% 
    mutate(HighDBX = EightTenthsX[match('A', Highest)]) %>%
    ungroup
  
  #new column for lower DB
  #Y
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>% 
    mutate(LowDBY = EightTenthsY[match('B', Highest)]) %>%
    ungroup
  #X
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>% 
    mutate(LowDBX = EightTenthsX[match('B', Highest)]) %>%
    ungroup
  
  #create channel safety (middle safety on the play?)
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(ChannelSafety = case_when(HighDBY < LeftAlleyLine & HighDBY > RightAlleyLine ~ 1,
                                     TRUE ~ 0))
  
  ##determine if column-safeties or high corners
  #left
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(LeftColumnSafety = case_when(LeftColumnChute == 1  & ChannelSafety == 1 ~ 0,
                                        LeftColumnChute == 1 & Highest=="A" & ChannelSafety == 0 ~ 1,
                                        LeftColumnChute == 1 & Highest=="B" & ChannelSafety == 0 ~ 1))
  #right
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(RightColumnSafety = case_when(RightColumnChute == 1  & ChannelSafety == 1 ~ 0,
                                         RightColumnChute == 1 & Highest=="A" & ChannelSafety == 0 ~ 1,
                                         RightColumnChute == 1 & Highest=="B" & ChannelSafety == 0 ~ 1))
  
  ##eliminate these column-safeties from columns
  #left
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(LeftColumn = case_when(LeftColumnSafety == 1 ~ 0,
                                  TRUE ~ LeftColumn))
  
  #Right
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(RightColumn = case_when(RightColumnSafety == 1 ~ 0,
                                   TRUE ~ RightColumn))
  
  #remove columnchute corners from the chute 
  #left
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(Chute = case_when(LeftColumnChute ==1 & LeftColumnSafety == 0 ~ 0,
                             TRUE ~ Chute))
  
  #right
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(Chute = case_when(RightColumnChute==1 & RightColumnSafety == 0 ~ 0,
                             TRUE ~ Chute))
  
  #redo highest player ID
  ##define two highest DBs 
  dat1 <- dat1 %>%
    arrange(gameId, playId, frameId, Chute != 1,DBPos != 'DB', desc(SnapX)) %>%
    group_by(gameId, playId, frameId) %>%
    mutate(Highest = c('A','B', rep('-', n() - 2)))
  
  #create new columns for higher DB
  #Y
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>% 
    mutate(HighDBY = EightTenthsY[match('A', Highest)]) %>%
    ungroup
  #X
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>% 
    mutate(HighDBX = EightTenthsX[match('A', Highest)]) %>%
    ungroup
  
  #new column for lower DB
  #Y
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>% 
    mutate(LowDBY = EightTenthsY[match('B', Highest)]) %>%
    ungroup
  #X
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>% 
    mutate(LowDBX = EightTenthsX[match('B', Highest)]) %>%
    ungroup
  #count column again
  #left
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(LeftColumnCount = sum(LeftColumn))
  
  #right
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(RightColumnCount = sum(RightColumn))
  
  dat2leftcolumn <- filter(dat1, LeftColumnCount==2)
  dat2rightcolumn <- filter(dat1, RightColumnCount==2)
  
  ### if still 2 in column, make the column defender the widest player
  ##create the indicator
  #left
  dat1 <- dat1 %>%
    arrange(gameId, playId, frameId, LeftColumn != 1, desc(SnapY)) %>%
    group_by(gameId, playId, frameId) %>%
    mutate(LeftColumnWidest = c(1, rep(0, n() - 1)))
  
  #right
  dat1 <- dat1 %>%
    arrange(gameId, playId, frameId, RightColumn != 1, SnapY) %>%
    group_by(gameId, playId, frameId) %>%
    mutate(RightColumnWidest = c(1, rep(0, n() - 1)))
  
  ## rename the inner player conflict defender (column label removed in next step)
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(LeftAlley = case_when(LeftColumn==1 & LeftColumnCount ==2 & LeftColumnWidest==0 ~ 1,
                                 TRUE ~ LeftAlley))
  
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(RightAlley = case_when(RightColumn==1 & RightColumnCount ==2 & RightColumnWidest==0 ~ 1,
                                  TRUE ~ RightAlley))
  
  ##in remaining observations with two players in column, make the widest DB the column player--set all others to 0
  #left
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(LeftColumn = case_when(LeftColumnCount==2 & LeftColumnWidest==1 ~ 1,
                                  LeftColumnCount==2 & LeftColumnWidest==0 ~ 0,
                                  TRUE ~ LeftColumn))
  #right
  dat1 <- dat1 %>% 
    group_by(gameId, playId) %>% 
    mutate(RightColumn = case_when(RightColumnCount==2 & RightColumnWidest==1 ~ 1,
                                   RightColumnCount==2 & RightColumnWidest==0 ~ 0,
                                   TRUE ~ RightColumn))
  ##count again
  #left
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(LeftAlleyCount = sum(LeftAlley)) 
  
  #right
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(RightAlleyCount = sum(RightAlley)) 
  
  datleftalley <- filter(dat1, LeftAlley == 1)
  datrightalley <- filter(dat1, RightAlley == 1)
  
  #count column again
  #left
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(LeftColumnCount = sum(LeftColumn))
  
  #right
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(RightColumnCount = sum(RightColumn))
  
  #delete three in column
  dat1 <- filter(dat1, RightColumnCount != 3)
  dat1 <- filter(dat1, LeftColumnCount != 3)
  
  #delete none in column 
  dat1 <- filter(dat1, RightColumnCount != 0)
  dat1 <- filter(dat1, LeftColumnCount != 0)
  
  #midpoint of safety x values
  dat1$SafetyPointX <- dat1$LowDBX
  dat1$SafetyPointY <- dat1$HighDBY
  
  ###Creating the contours
  ##create line segments
  #low DB to sideline
  dat1$LowDBtoRefPointSegment <- sqrt((dat1$LowDBX - dat1$SafetyPointX)^2 + (dat1$LowDBY - dat1$SafetyPointY)^2)
  #high DB to sideline
  dat1$HighDBtoRefPointSegment <- sqrt((dat1$HighDBX - dat1$SafetyPointX)^2 + (dat1$HighDBY - dat1$SafetyPointY)^2)
  #low DB to high DB
  dat1$LowDBHighDBSegment <- sqrt((dat1$HighDBX - dat1$LowDBX)^2 + (dat1$HighDBY - dat1$LowDBY)^2)
  
  ##determine the angle
  dat1 <- dat1 %>%
    mutate(Sine = HighDBtoRefPointSegment/LowDBHighDBSegment) %>%
    mutate(SafetyAngle = asin(Sine))
  # convert to degrees
  dat1$SafetyAngle <- (dat1$SafetyAngle*180)/pi
  
  #above 7 yards indicator 
  dat1 <- dat1 %>% 
    mutate(HighSafetyDepth = case_when(HighDBX >= BallSnapX + 7 ~ 'HighSafeHigh',
                                       HighDBX < BallSnapX + 7 ~ 'HighSafeLow'))
  
  dat1 <- dat1 %>% 
    mutate(LowSafetyDepth = case_when(LowDBX >= BallSnapX + 7 ~ 'LowSafeHigh',
                                      LowDBX < BallSnapX + 7 ~ 'LowSafeLow'))
  
  dat1 <- dat1 %>%
    mutate(SafetyOver7Count = case_when(HighSafetyDepth == 'HighSafeHigh' & LowSafetyDepth == 'LowSafeHigh' ~ 2,
                                        HighSafetyDepth == 'HighSafeHigh' & LowSafetyDepth == 'LowSafeLow'~ 1,
                                        HighSafetyDepth == 'HighSafeLow' & LowSafetyDepth == 'LowSafeLow' ~ 0))
  
  ##safeties in window
  #difference on each side
  #ID rushers and eliminate from alley
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(PassRusher = case_when(SecondAndHalfX < BallSnapX & OffDef == "Defense" ~ "PassRusher",
                                  SecondAndHalfX >= BallSnapX & OffDef == "Defense" ~ "Coverage",
                                  TRUE ~ "Offense"))
  
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(LeftAlley = case_when(LeftAlley == 1 & PassRusher == "PassRusher" ~ 0,
                                 TRUE ~ LeftAlley))
  
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(RightAlley = case_when(RightAlley == 1 & PassRusher == "PassRusher" ~ 0,
                                  TRUE ~ RightAlley))
  
  ##left difference
  #defense left alley count
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(LeftAlleyCount = sum(LeftAlley)) 
  #defense left column count
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(NewLeftColumnCount = sum(SnapY >= LeftColumnLine & OffDef == "Defense" & PassRusher=="Coverage")) 
  
  #sum defense left column and alley (alley tube + column tube = 'shield')
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(LeftShield = LeftAlleyCount + NewLeftColumnCount)
  
  #offense
  #offense left column count
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(LeftOffColumnCount = sum(SnapY >= LeftColumnLine & OffDef == "Offense"))
  #offense left alley count
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(LeftOffAlleyCount = sum(SnapY < LeftColumnLine & SnapY >= LeftAlleyLine & OffDef == "Offense"))
  #sum offense left shield
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(LeftOffShield = LeftOffColumnCount + LeftOffAlleyCount)
  #left defense minus offensive players outside tackle
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(LeftDBDiff = LeftShield - LeftOffShield)
  
  ##right diff 
  #defense
  #defense right alley count
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(RightAlleyCount = sum(RightAlley)) 
  #defense right column count
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(NewRightColumnCount = sum(SnapY <= RightColumnLine & OffDef == "Defense" & PassRusher =="Coverage")) 
  #sum right column and alley
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(RightShield = RightAlleyCount + NewRightColumnCount)
  
  #offense
  #offense right column count
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(RightOffColumnCount = sum(SnapY <= RightColumnLine & OffDef == "Offense"))
  #offense right alley count
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(RightOffAlleyCount = sum(SnapY > RightColumnLine & SnapY <= RightAlleyLine & OffDef == "Offense"))
  #sum offense right shield
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(RightOffShield = RightOffColumnCount + RightOffAlleyCount)
  #left defense minus offensive players outside tackle
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(RightDBDiff = RightShield - RightOffShield)
  
  #label diffs
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(LeftDiffLabel = case_when(LeftDBDiff < 1 ~ "OneHighIndicator",
                                     LeftDBDiff >= 1 ~ "TwoHighIndicator"))
  
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(RightDiffLabel = case_when(RightDBDiff < 1 ~ "OneHighIndicator",
                                      RightDBDiff >= 1 ~ "TwoHighIndicator"))
   
  #indicator 
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(DiffLabel = case_when(LeftDiffLabel == "OneHighIndicator" & RightDiffLabel == "OneHighIndicator" ~ "One",
                                 LeftDiffLabel == "OneHighIndicator" & RightDiffLabel == "TwoHighIndicator" ~ "Mix",
                                 LeftDiffLabel == "TwoHighIndicator" & RightDiffLabel == "OneHighIndicator" ~ "Mix",
                                 LeftDiffLabel == "TwoHighIndicator" & RightDiffLabel == "TwoHighIndicator" ~ "Two"))
  
  #add channel into the diff indicator
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>%
    mutate(DiffLabel = case_when(ChannelSafety == 1 & DiffLabel=="Mix" ~ "MixOne",
                                 ChannelSafety == 0 & DiffLabel=="Mix" ~ "MixTwo",
                                 TRUE ~ DiffLabel))
  
  ###using the safety angle, depth, and player difference to label the window
  dat1 <- dat1 %>%
    mutate(Window = case_when(SafetyOver7Count == 2 & DiffLabel == "Two" & SafetyAngle <= 30 ~ 2,
                              SafetyOver7Count == 2 & DiffLabel == "Two" & SafetyAngle > 30 ~ 1,
                              SafetyOver7Count == 2 & DiffLabel == "One" & SafetyAngle <= 20 ~ 2,
                              SafetyOver7Count == 2 & DiffLabel == "One" & SafetyAngle > 20 ~ 1,
                              SafetyOver7Count == 2 & DiffLabel == "MixOne" & SafetyAngle <= 22.5 ~ 2,
                              SafetyOver7Count == 2 & DiffLabel == "MixOne" & SafetyAngle > 22.5 ~ 1,
                              SafetyOver7Count == 2 & DiffLabel == "MixTwo" & SafetyAngle <= 27.5 ~ 2,
                              SafetyOver7Count == 2 & DiffLabel == "MixTwo" & SafetyAngle > 27.5 ~ 1,
                              SafetyOver7Count == 1 ~ 1,
                              SafetyOver7Count == 0 ~ 0))
  
  dat1 <- dat1 %>%
    mutate(Window = case_when(SafetyOver7Count == 2 & LowDBX >= 12 & DiffLabel == "Two"  & SafetyAngle <= 35 ~ 2,
                              SafetyOver7Count == 2 & LowDBX >= 12 & DiffLabel == "Two"  & SafetyAngle > 35 ~ 1,
                              SafetyOver7Count == 2 & LowDBX >= 12 & DiffLabel == "One"  & SafetyAngle <= 25 ~ 2,
                              SafetyOver7Count == 2 & LowDBX >= 12 & DiffLabel == "One"  & SafetyAngle > 25 ~ 1,
                              SafetyOver7Count == 2 & LowDBX >= 12 & DiffLabel == "MixOne"  & SafetyAngle <= 22.5 ~ 2,
                              SafetyOver7Count == 2 & LowDBX >= 12 & DiffLabel == "MixOne"  & SafetyAngle > 22.5 ~ 1,
                              SafetyOver7Count == 2 & LowDBX >= 12 & DiffLabel == "MixTwo"  & SafetyAngle <= 27.5 ~ 2,
                              SafetyOver7Count == 2 & LowDBX >= 12 & DiffLabel == "MixTwo"  & SafetyAngle > 27.5 ~ 1,
                              TRUE ~ Window))
  
  ## How many coverage defenders?
  dat1 <- dat1 %>%
    group_by(gameId, playId, frameId) %>%
    mutate(CoverageDefenders = sum(SecondAndHalfX > BallSnapX & OffDef == 'Defense'))
  
  ## Creating the shell
  ## corner depth (depth of column defenders)
  #Left
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>% 
    mutate(LeftCornerDepth = SnapX[match(1, LeftColumn)]) %>% 
    ungroup()
  
  #Right
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>% 
    mutate(RightCornerDepth = SnapX[match(1, RightColumn)]) %>% 
    ungroup()
  
  ##identify the shell
  dat1 <- dat1 %>%
    group_by(gameId, playId) %>% 
    mutate(Shell = case_when(Window == 0 ~ '0',
                             Window == 1 ~ '3',
                             Window == 2 & LeftCornerDepth < BallSnapX + 6 & RightCornerDepth < BallSnapX + 6  ~ '2', 
                             Window == 2 & LeftCornerDepth >= BallSnapX + 6 & RightCornerDepth < BallSnapX + 6  ~ '6',
                             Window == 2 & LeftCornerDepth < BallSnapX + 6 & RightCornerDepth >= BallSnapX + 6  ~ '6',
                             Window == 2 & LeftCornerDepth >= BallSnapX + 6 & RightCornerDepth >= BallSnapX + 6  ~ '4'))
  
  
  ##identify positions redo
  dat1 <- dat1 %>%
    mutate(GamePosition = case_when(LeftColumn == 1 ~ "Corner",
                                    RightColumn == 1 ~ "Corner",
                                    Highest == "A" ~ "Safety",
                                    Highest == "B" & Window == 2 ~ "Safety",
                                    Highest == "B" & Window == 1 ~ "SafetyTBD",
                                    Highest == "B" & Window == 0 ~ "Safety",
                                    LeftAlley == 1 & Highest == "-" ~ "Conflict",
                                    RightAlley == 1 & Highest == "-" ~ "Conflict",
                                    LeftAlleyLine > SnapY & SnapY > RightAlleyLine & Highest == "-" ~ "Adjacent"))
  # Determine SafetyTBD                                 
  dat1 <- dat1 %>%
    mutate(GamePosition = case_when(GamePosition == "SafetyTBD" & LeftAlley==1 ~ "SafetyConflict",
                                    GamePosition == "SafetyTBD" & RightAlley==1 ~ "SafetyConflict",
                                    GamePosition == "SafetyTBD" & LeftAlleyLine > SnapY & SnapY > RightAlleyLine ~ "SafetyAdjacent",
                                    TRUE ~ GamePosition))                                
  
  # Remove offense and pass rushers                             
  dat1 <- dat1 %>%
    mutate(GamePosition = case_when(OffDef=="Offense" ~ "Offense",
                                    PassRusher=="PassRusher" ~ "PassRusher",
                                    TRUE ~ GamePosition))
  
  #delete obsolete columns
  dat1 <- dat1 %>%
    select(-time, -SideDuring, -qb_team, -QBTeam, -uniqueplay, -L1OffY, -L2OffY, -L3OffY, -L4OffY, 
           -R1OffY, -R2OffY, -R3OffY, -R4OffY, -L1L2Diff, -R1R2Diff, -LeftSideline, -RightSideline, 
           -TightSplitLeft, -TightSplitRight, -LeftColumnDBCount, -RightColumnDBCount, -L2L3Diff, 
           -R2R3Diff, -L2OffInsideOT, -R2OffInsideOT, -LeftReceiverCount, -RightReceiverCount, 
           -L1InsideTE, -L2InsideTE, -L3InsideTE, -L4InsideTE, -R1InsideTE, -R2InsideTE, -R3InsideTE, 
           -R4InsideTE, -LeftReceiverCountOutsideTackle, -RightReceiverCountOutsideTackle, -LeftAlleyID, 
           -RightAlleyID, -LeftColumnHighest, -RightColumnHighest, -LeftColumnChute, -RightColumnChute, 
           -LeftColumnSafety, -RightColumnSafety, -LeftColumnWidest, -RightColumnWidest, 
           -NewLeftColumnCount, -LeftShield, -LeftOffColumnCount, -LeftOffAlleyCount, -LeftOffShield, 
           -LeftDBDiff, -NewRightColumnCount, -RightShield, -RightOffColumnCount, -RightOffAlleyCount, 
           -RightOffShield, -RightDBDiff)
  
return(dat1)
  
}

# 

## Adding Play Data 

AddsPlayData <- function(Offense, Defense, Ball, BallX, FirstDownLine) { # IMPORTANT: Names must be exact for this to work. Using prep_vis_data.r will ensure this. 
  list( geom_point( data = Offense , mapping =  aes(x = x, y = y) , shape = 1 , size = 1.5 , color = "orange") , 
        geom_point( data = Defense , mapping =  aes(x = x, y = y) , shape = 4 , size = 1.5 , color = "purple") , 
        geom_point( data = Ball , mapping =  aes(x = x, y = y) , shape = 21 , fill = "brown" , alpha = 1.0 , size = 2) , 
        geom_segment(aes(x = as.numeric(BallX) , y = 0, xend = as.numeric(BallX) , yend = 53.33) , color = "blue" , alpha = 1 , size = 1) , 
        geom_segment(aes(x = as.numeric(FirstDownLine) , y = 0, xend = as.numeric(FirstDownLine) , yend = 53.33) , color = "yellow" , alpha = 1 , size = 1) )
}

#

## Adding Throw Data 

AddsThrowData <- function(Offense_throw, Defense_throw, Ball_throw, BallX, FirstDownLine, target_player_location) { 
  list( geom_point( data = Offense_throw , mapping =  aes(x = x, y = y) , shape = 1 , size = 3 , color = "orange") ,  
        geom_point( data = Defense_throw , mapping =  aes(x = x, y = y) , shape = 4 , size = 3 , color = "purple") , 
        geom_point( data = Ball_throw , mapping =  aes(x = x, y = y) , shape = 21 , fill = "brown" , alpha = 1.0 , size = 4) , 
        geom_segment(aes(x = as.numeric(BallX) , y = 0, xend = as.numeric(BallX) , yend = 53.33) , color = "blue" , alpha = 1 , size = 1) , 
        geom_segment(aes(x = as.numeric(FirstDownLine) , y = 0, xend = as.numeric(FirstDownLine) , yend = 53.33) , color = "yellow" , alpha = 1 , size = 1) , 
        geom_circle(aes(x0 = target_player_location$x, y0 = target_player_location$y , r = 10) , fill = "red" , alpha = 0.15) , 
        geom_circle(aes(x0 = target_player_location$x, y0 = target_player_location$y , r = 5) , fill = "red" , alpha = 0.25)  )
}

# 

## Adding Shell ID Markers 

AddShellMarkers <- function(shell_features) { 
  list(  geom_segment(aes(x = shell_features$BallSnapX , y = shell_features$LeftColumnLine , 
                   xend =120 , yend = shell_features$LeftColumnLine) , alpha = 0.5 , color = "black") , 
         geom_segment(aes(x = shell_features$BallSnapX , y = shell_features$RightColumnLine , 
                     xend =120 , yend = shell_features$RightColumnLine) , alpha = 0.5 , color = "black") , 
         geom_segment(aes(x = shell_features$BallSnapX , y = shell_features$LeftAlleyLine , 
                     xend =120 , yend = shell_features$LeftAlleyLine) , alpha = 0.5 , color = "black") , 
         geom_segment(aes(x = shell_features$BallSnapX , y = shell_features$RightAlleyLine , 
                     xend =120 , yend = shell_features$RightAlleyLine) , alpha = 0.5 , color = "black") , 
         annotate("label", x = 60 , y = -5 , size = 2 , label = paste(
                    "Window:" , shell_features$Window[1] ,
                    ", Safety Angle:" , round(shell_features$SafetyAngle[1],0),
                    ", Shell:", shell_features$Shell,
                    ", Coverage Defenders:", shell_features$CoverageDefenders))
        ) 
  }

#

# Code Based in Part On (https://www.kaggle.com/tombliss/additional-data-coverage-schemes-for-week-1?select=targetedReceiver.csv)
xmin <- 10
xmax <- 110
ymin <- 0 
ymax <- 160 / 3 
hash.right <- 5
hash.left <- 5
hash.width <- 1

df.hash <- expand.grid(x = c(11:109 - 0.12) , y = c(0+1, 23.36667-0.25, 29.96667-0.20, 160/3-1.6))

AddFieldLines <- function() {
  NFLlogo <- readPNG("Data/nfl-logo.png") 
  NFLlogo <- rasterGrob(NFLlogo, interpolate=TRUE) 
  
  list(    geom_segment(aes(x = 15, y = 0, xend = 15, yend = 53.33) , color = "white" , alpha = 1 , size = 0.3) , # 5 Yard Line 
           geom_segment(aes(x = 20, y = 0, xend = 20, yend = 53.33) , color = "white" , alpha = 1 , size = 0.4) , # 10 Yard Line 
           geom_segment(aes(x = 25, y = 0, xend = 25, yend = 53.33) , color = "white" , alpha = 1 , size = 0.3) , # 15 Yard Line 
           geom_segment(aes(x = 30, y = 0, xend = 30, yend = 53.33) , color = "white" , alpha = 1 , size = 0.4) , # 20 Yard Line 
           geom_segment(aes(x = 35, y = 0, xend = 35, yend = 53.33) , color = "white" , alpha = 1 , size = 0.3) , # 25 Yard Line 
           geom_segment(aes(x = 40, y = 0, xend = 40, yend = 53.33) , color = "white" , alpha = 1 , size = 0.4) , # 30 Yard Line 
           geom_segment(aes(x = 45, y = 0, xend = 45, yend = 53.33) , color = "white" , alpha = 1 , size = 0.3) , # 35 Yard Line 
           geom_segment(aes(x = 50, y = 0, xend = 50, yend = 53.33) , color = "white" , alpha = 1 , size = 0.4) , # 40 Yard Line 
           geom_segment(aes(x = 55, y = 0, xend = 55, yend = 53.33) , color = "white" , alpha = 1 , size = 0.3) , # 45 Yard Line 
           geom_segment(aes(x = 60, y = 0, xend = 60, yend = 53.33) , color = "white" , alpha = 1 , size = 0.5) , # 50 Yard Line 
           geom_segment(aes(x = 65, y = 0, xend = 65, yend = 53.33) , color = "white" , alpha = 1 , size = 0.3) , # 45 Yard Line 
           geom_segment(aes(x = 70, y = 0, xend = 70, yend = 53.33) , color = "white" , alpha = 1 , size = 0.4) , # 40 Yard Line 
           geom_segment(aes(x = 75, y = 0, xend = 75, yend = 53.33) , color = "white" , alpha = 1 , size = 0.3) , # 35 Yard Line 
           geom_segment(aes(x = 80, y = 0, xend = 80, yend = 53.33) , color = "white" , alpha = 1 , size = 0.4) , # 30 Yard Line 
           geom_segment(aes(x = 85, y = 0, xend = 85, yend = 53.33) , color = "white" , alpha = 1 , size = 0.3) , # 25 Yard Line 
           geom_segment(aes(x = 90, y = 0, xend = 90, yend = 53.33) , color = "white" , alpha = 1 , size = 0.4) , # 20 Yard Line 
           geom_segment(aes(x = 95, y = 0, xend = 95, yend = 53.33) , color = "white" , alpha = 1 , size = 0.3) , # 15 Yard Line 
           geom_segment(aes(x = 100, y = 0, xend = 100, yend = 53.33) , color = "white" , alpha = 1 , size = 0.4) , # 10 Yard Line 
           geom_segment(aes(x = 105, y = 0, xend = 105, yend = 53.33) , color = "white" , alpha = 1 , size = 0.3) , # 5 Yard Line 
           xlim(-10, 130) , 
           ylim(-10, 63.3333) , 
           annotate("text", x = df.hash$x[df.hash$x < 20/2], color = "white" , size = 2 , 
                    y = df.hash$y[df.hash$x < 20/2], label = "|", hjust = 0, vjust = -0) , 
           annotate("text", x = df.hash$x[df.hash$x > 20/2], color = "white" , size = 2 , 
                    y = df.hash$y[df.hash$x > 20/2], label = "|", hjust = 0, vjust = -0) , 
           annotation_custom(NFLlogo, xmin=56.9, xmax=62.9, ymin=24, ymax=29.5 ) )
}


AddFieldColor <- function(x) { 
  list(    geom_rect(mapping=aes(xmin=0, xmax=120, ymin=0, ymax=53.33) , color = "black" ,  fill = "#00614c", alpha=0.7) , # Making Field Green
           geom_rect(mapping=aes(xmin=0, xmax=10, ymin=0, ymax=53.33) , color = "black" , fill = "#2F3BE4" , alpha = 0.4) , # Left End Zone 
           geom_rect(mapping=aes(xmin=110, xmax=120, ymin=0, ymax=53.33) , color = "black" , fill = "#2F3BE4" , alpha = 0.4) )  # Right End Zone 
  
}

AddYardNumbers <- function(x) {
  list(annotate(geom = "text", x = 20, y = 5, label = "1 0", color = "white"),
       annotate(geom = "text", x = 30, y = 5, label = "2 0", color = "white"),
       annotate(geom = "text", x = 40, y = 5, label = "3 0", color = "white"),
       annotate(geom = "text", x = 50, y = 5, label = "4 0", color = "white"),
       annotate(geom = "text", x = 60, y = 5, label = "5 0", color = "white"),
       annotate(geom = "text", x = 70, y = 5, label = "4 0", color = "white"),
       annotate(geom = "text", x = 80, y = 5, label = "3 0", color = "white"),
       annotate(geom = "text", x = 90, y = 5, label = "2 0", color = "white"),
       annotate(geom = "text", x = 100, y = 5, label = "1 0", color = "white"))
}

AddUpsideDownYardNumbers <- function(x) {
  list(annotate(geom = "text", x = 20, y = 48.33, label = "1 0", color = "white", angle = 180),
       annotate(geom = "text", x = 30, y = 48.33, label = "2 0", color = "white", angle = 180),
       annotate(geom = "text", x = 40, y = 48.33, label = "3 0", color = "white", angle = 180),
       annotate(geom = "text", x = 50, y = 48.33, label = "4 0", color = "white", angle = 180),
       annotate(geom = "text", x = 60, y = 48.33, label = "5 0", color = "white", angle = 180),
       annotate(geom = "text", x = 70, y = 48.33, label = "4 0", color = "white", angle = 180),
       annotate(geom = "text", x = 80, y = 48.33, label = "3 0", color = "white", angle = 180),
       annotate(geom = "text", x = 90, y = 48.33, label = "2 0", color = "white", angle = 180),
       annotate(geom = "text", x = 100, y = 48.33, label = "1 0", color = "white", angle = 180))
}


AddEndzones <- function(x) {
  list(annotate(geom = "text", x = 5, y = 27, label = "OFFENSE", color = "white", angle = 90),
       annotate(geom = "text", x = 115, y = 27, label = "DEFENSE", color = "white", angle = 270))
}

#

## Clear Figure Background 

ClearFigBackground <- function(){ 
  list( theme_bw() , 
        theme(panel.border = element_blank()) , 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) ) 
}

#

## Add a Circle (https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2)

createCircle <- function(center = c(0,0), diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

