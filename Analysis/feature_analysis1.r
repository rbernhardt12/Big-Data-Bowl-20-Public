### NFL Big Data Bowl 2020-21 
# Robert Bernhardt, Andrew Rogan, Daniel Weiss 
# December 2020 

## Analysis of Engineered Features 
# Objective: Analyze various features and observe their effects on football outcomes 

CompleteData <- rbind(Week1,Week2,Week3,Week4,Week5,Week6,Week7,Week8,Week9,
                      Week10,Week11,Week12,Week13,Week14,Week15,Week16,Week17) # Time intensive  

AnalysisData <- CompleteData 

ScriptName = paste(MainDir , "/Analysis/Functions/feature_eng1.r" , sep = "")
source(ScriptName) 

# 

## Section 
## Overall Summary Stats 

# Throw Distance/Target Depth Variables 
describe(ExtractedFeatures$ThrowDist)
describe(ExtractedFeatures$Passer2RecDistThrow)
describe(ExtractedFeatures$Passer2RecDistArrive)
describe(ExtractedFeatures$TargDownfieldDistThrow)
describe(ExtractedFeatures$TargDownfieldDistRec)

# Separation Distance Variables 
describe(ExtractedFeatures$PrimDefDistThrow)
describe(ExtractedFeatures$SecDefDistThrow)
describe(ExtractedFeatures$PrimDefDistArrive)
describe(ExtractedFeatures$SecDefDistArrive)

# Angular Separation Variables 
describe(ExtractedFeatures$PrimDefAngThrow)
describe(ExtractedFeatures$SecDefAngThrow)
describe(ExtractedFeatures$PrimBallRecAng)
describe(ExtractedFeatures$SecBallRecAng)

# Distance to Ball Variables (throw)
describe(ExtractedFeatures$TargDist2BallLandThrow)
describe(ExtractedFeatures$PrimDefDist2BallLandThrow)
describe(ExtractedFeatures$SecDefDist2BallLandThrow)

# Distance to Ball Variables (arrival)
describe(ExtractedFeatures$TargDist2BallLandArrive)
describe(ExtractedFeatures$PrimDefDist2BallLandArrive)
describe(ExtractedFeatures$SecDefDist2BallLandArrive)

# 

## Section 
## Pass Arrival Time Regressions 

reg <- lm(TimePassInAir ~ Passer2RecDistThrow , data = ExtractedFeatures) 
summary(reg)

reg <- lm(TimePassInAir ~ Passer2RecDistThrow + P2RDistThrowSq , data = ExtractedFeatures)  
summary(reg)
BIC(reg)
ExtractedFeatures$EstTimeinAir <- reg$fitted.values

ggplot() + geom_point( aes(x = Passer2RecDistThrow , y = TimePassInAir) , data = ExtractedFeatures) + 
           geom_line( aes(x = Passer2RecDistThrow , y = EstTimeinAir) , color = "red" , data = ExtractedFeatures)

Completions <- ExtractedFeatures %>% filter(Complete == 1) 

reg <- lm(TimePassInAir ~ Passer2RecDistThrow + P2RDistThrowSq , data = Completions) 
summary(reg)
BIC(reg)
Completions$EstTimeinAir <- reg$fitted.values 

ggplot() + geom_point( aes(x = Passer2RecDistThrow , y = TimePassInAir) , data = Completions) + 
  geom_line( aes(x = Passer2RecDistThrow , y = EstTimeinAir) , color = "red" , data = Completions)

# 

## Section 
## Separation Quality Regressions (Enhanced Separation)
  
# Primary Def. Regressions 
reg <- lm(PrimDefDistArrive ~ PrimDefDistThrow , data = ExtractedFeatures)
summary(reg)
BIC(reg)

reg <- lm(PrimDefDistArrive ~ PrimDefDistThrow + PrimDeltaSpeedThrow , data = ExtractedFeatures)
summary(reg)
BIC(reg)

reg <- lm(PrimDefDistArrive ~ PrimDefDistThrow + PrimDeltaSpeedThrow + PrimDeltaAccThrow, data = ExtractedFeatures)
summary(reg)
BIC(reg)

reg <- lm(PrimDefDistArrive ~ PrimDefDistThrow + PrimDeltaSpeedThrow + PrimDeltaAccThrow + PrimDeltaDirThrow , 
             data = ExtractedFeatures)
summary(reg)
BIC(reg)
ExtractedFeatures$PrimEnhancedSep <- reg$fitted.values 

# Secondary Def. Regressions 
reg <- lm(SecDefDistArrive ~ SecDefDistThrow , data = ExtractedFeatures)
summary(reg)
BIC(reg)

reg <- lm(SecDefDistArrive ~ SecDefDistThrow + SecDeltaSpeedThrow , data = ExtractedFeatures)
summary(reg)
BIC(reg)

reg <- lm(SecDefDistArrive ~ SecDefDistThrow + SecDeltaSpeedThrow + SecDeltaAccThrow, data = ExtractedFeatures)
summary(reg)
BIC(reg)

reg <- lm(SecDefDistArrive ~ SecDefDistThrow + SecDeltaSpeedThrow + SecDeltaAccThrow + SecDeltaDirThrow , 
          data = ExtractedFeatures)
summary(reg)
BIC(reg)

reg <- lm(SecDefDistArrive ~ SecDefDistThrow + SecDeltaSpeedThrow + SecDeltaDirThrow , 
           data = ExtractedFeatures)
summary(reg)
BIC(reg)
ExtractedFeatures$SecEnhancedSep <- reg$fitted.values 

# 

## Section 
## Pass Outcome Regressions 

# LPM , Primary & Secondary Defender
reg <- lm(Complete ~ PrimDefDistThrow + SecDefDistThrow + PrimDefAngThrow + SecDefAngThrow + 
            Passer2RecDistThrow , data = ExtractedFeatures)
summary(reg)
BIC(reg) 

reg <- lm(Complete ~ PrimEnhancedSep + SecEnhancedSep + PrimDefAngThrow + SecDefAngThrow + 
            Passer2RecDistThrow , data = ExtractedFeatures)
summary(reg) 
BIC(reg) 

reg <- lm(Complete ~ PrimDefDistThrow + PrimDeltaSpeedThrow + PrimDeltaAccThrow + PrimDeltaDirThrow + 
            SecDefDistThrow + SecDeltaSpeedThrow + SecDeltaDirThrow + 
            PrimDefAngThrow + SecDefAngThrow + Passer2RecDistThrow , data = ExtractedFeatures)
summary(reg)
BIC(reg)

# LPM , Primary Defender Only 

reg <- lm(Complete ~ PrimDefDistThrow + PrimDefAngThrow + 
            Passer2RecDistThrow , data = ExtractedFeatures)
summary(reg)
BIC(reg) 

reg <- lm(Complete ~ PrimEnhancedSep + PrimDefAngThrow + 
            Passer2RecDistThrow , data = ExtractedFeatures)
summary(reg)
BIC(reg) # Lower BIC than above or below regressions. 

reg <- lm(Complete ~ PrimDefDistThrow + PrimDeltaSpeedThrow + PrimDeltaAccThrow + PrimDeltaDirThrow + 
            PrimDefAngThrow, data = ExtractedFeatures)
summary(reg)
BIC(reg)

# LPM , Adding Player Controls & Passer to Rec Dist.  
reg <- lm(Complete ~ PrimEnhancedSep + PrimDefAngThrow + Passer2RecDistThrow + 
            BackfieldQB, data = ExtractedFeatures) # Omitted factor QB: Aaron Rodgers 
summary(reg)
BIC(reg)

reg <- lm(Complete ~ PrimEnhancedSep + PrimDefAngThrow + Passer2RecDistThrow + 
            BackfieldQB + targetName + PrimaryDefName , data = ExtractedFeatures)
summary(reg)
BIC(reg) 

# Adding Contextual Information 
reg <- lm(Complete ~ PrimEnhancedSep + PrimDefAngThrow + Passer2RecDistThrow + 
            as.factor(down)  + numberOfPassRushers , 
          data = ExtractedFeatures)
summary(reg)
BIC(reg) 

reg <- lm(Complete ~ PrimDefDistThrow + PrimDefAngThrow + Passer2RecDistThrow + 
            as.factor(down)  + numberOfPassRushers , 
          data = ExtractedFeatures)
summary(reg)
BIC(reg) 

# Adding Matchup Factor Variables 
reg <- lm(Complete ~ PrimEnhancedSep + PrimDefAngThrow + Passer2RecDistThrow + 
            as.factor(down)  + numberOfPassRushers + as.factor(targetPosition) + as.factor(PrimaryDefPosition) , 
          data = ExtractedFeatures)
summary(reg)
BIC(reg) # Omitted factor groups: QB as target & DB 
 
reg <- lm(Complete ~ PrimEnhancedSep + PrimDefAngThrow + Passer2RecDistThrow + 
            as.factor(down)  + numberOfPassRushers + as.factor(targetPosition) * as.factor(PrimaryDefPosition) , 
          data = ExtractedFeatures)
summary(reg)
BIC(reg) #

reg <- lm(Complete ~ 
            as.factor(down)  + numberOfPassRushers + as.factor(targetPosition) * as.factor(PrimaryDefPosition) , 
          data = ExtractedFeatures)
summary(reg)
BIC(reg) 

# 

## Section 
## Pass Outcome Regressions -- WR & TE Only 

ExtractedFeaturesRec <- filter(ExtractedFeatures , targetPosition == "WR" | targetPosition == "TE")

# Basic Regs, focus on Primary Defender 
reg <- lm(Complete ~ PrimDefDistThrow + PrimDefAngThrow + 
            Passer2RecDistThrow , data = ExtractedFeaturesRec)
summary(reg)
BIC(reg) 

reg <- lm(Complete ~ PrimEnhancedSep + PrimDefAngThrow + 
            Passer2RecDistThrow , data = ExtractedFeaturesRec)
summary(reg) 
BIC(reg) 

reg <- lm(Complete ~ PrimDefDistThrow + PrimDeltaSpeedThrow + PrimDeltaAccThrow + PrimDeltaDirThrow + 
            PrimDefAngThrow + SecDefAngThrow + Passer2RecDistThrow , data = ExtractedFeaturesRec)
summary(reg)
BIC(reg)

# Adding Matchup Factors 
reg <- lm(Complete ~ PrimEnhancedSep + PrimDefAngThrow + Passer2RecDistThrow + 
            as.factor(down)  + numberOfPassRushers + as.factor(targetPosition) + as.factor(PrimaryDefPosition) , 
          data = ExtractedFeaturesRec)
summary(reg)
BIC(reg) # Omitted factor groups: QB as target & DB 

reg <- lm(Complete ~ PrimEnhancedSep + PrimDefAngThrow + Passer2RecDistThrow + 
            as.factor(down)  + numberOfPassRushers + as.factor(targetPosition) * as.factor(PrimaryDefPosition) , 
          data = ExtractedFeaturesRec)
summary(reg)
BIC(reg) 

reg <- lm(Complete ~ 
            as.factor(down)  + numberOfPassRushers + as.factor(targetPosition) * as.factor(PrimaryDefPosition) , 
          data = ExtractedFeaturesRec)
summary(reg)
BIC(reg) 

# 

## Section 
## Final Regressions 

# Simple Model 
reg <- lm(Complete ~ PrimDefDistThrow + Passer2RecDistThrow , 
          data = ExtractedFeaturesRec)
summary(reg)
BIC(reg) 

reg <- lm(Complete ~ PrimEnhancedSep + Passer2RecDistThrow , 
          data = ExtractedFeaturesRec)
summary(reg)
BIC(reg) 

reg <- lm(Complete ~ PrimDefDistThrow + PrimDeltaSpeedThrow + PrimDeltaAccThrow + PrimDeltaDirThrow + 
          Passer2RecDistThrow , 
          data = ExtractedFeaturesRec)
summary(reg)
BIC(reg)

# Advanced Model 
reg <- lm(Complete ~ PrimEnhancedSep  + Passer2RecDistThrow +
          as.factor(CoverageFamily) + as.factor(PrimDefZone) + as.factor(Conditions), 
          data = ExtractedFeatures)
summary(reg)
BIC(reg) 

# Player Controls (Ball Skills) 
reg <- lm(Complete ~ PrimEnhancedSep  + Passer2RecDistThrow + 
            as.factor(CoverageFamily) + as.factor(PrimDefZone) + as.factor(Conditions) + 
            as.factor(PrimaryDefNameQ), 
          data = ExtractedFeaturesRec)
summary(reg)
BIC(reg) 

# 

## Section 
## Individual Player Summary Stats 

# Receivers 
TargetPlayerSumStats <- ExtractedFeatures %>% 
  ungroup %>% group_by(targetName) %>% 
  mutate(avgPrimSepThrow = mean(PrimDefDistThrow)) %>% 
  mutate(avgPrimSepArrive = mean(PrimDefDistArrive)) %>% 
  mutate(avgPrimEnhancedSep = mean(PrimEnhancedSep)) %>% 
  mutate(numTargets = 1) %>% mutate(numTargets = sum(numTargets)) %>% # Calculates number of targets 
  mutate(numCompletions = sum(Complete)) %>% # Calculates number of receptions 
  filter(numTargets >= 15) # Limits data to only players with a minimum number of targets 

max_count <- nrow(TargetPlayerSumStats)
TargetPlayerSumStats$count <- 1:max_count

TargetPlayerSumStats <- TargetPlayerSumStats %>% 
  ungroup %>% group_by(targetName) %>%
  mutate(min_count = min(count)) %>% 
  filter(min_count == count) %>% 
  select(targetName , avgPrimSepThrow  , avgPrimSepArrive , numCompletions , playerPlayCount , 
         avgPrimEnhancedSep , numTargets , targetPosition , targetPosBasic) %>% 
  mutate(avgGapClosed = avgPrimSepThrow - avgPrimSepArrive) %>% 
  mutate(avgEnhancedGapClosed = avgPrimEnhancedSep - avgPrimSepArrive) %>% 
  mutate(receptionPercent = 100 * numCompletions / numTargets) %>% 
  mutate(targetPercent = 100 * numTargets / playerPlayCount)

TargetSumStatsWR <- filter(TargetPlayerSumStats , targetPosBasic == "WR")
TargetSumStatsTE <- filter(TargetPlayerSumStats , targetPosBasic == "TE")
TargetSumStatsRB <- filter(TargetPlayerSumStats , targetPosBasic == "RB")

# Primary Defenders 
PrimaryDefSumStats <- ExtractedFeatures %>% 
  ungroup %>% group_by(PrimaryDefName) %>% 
  mutate(avgPrimSepThrow = mean(PrimDefDistThrow)) %>% 
  mutate(avgPrimSepArrive = mean(PrimDefDistArrive)) %>% 
  mutate(avgPrimEnhancedSep = mean(PrimEnhancedSep)) %>% 
  mutate(numTargets = 1) %>% mutate(numTargets = sum(numTargets)) %>% # Calculates number of targets defended 
  mutate(numCompletions = sum(Complete)) %>% # Calculates number of completions allowed 
  filter(numTargets >= 15) # Limits data to players with minimum number of targets defended 

max_count <- nrow(PrimaryDefSumStats)
PrimaryDefSumStats$count <- 1:max_count

PrimaryDefSumStats <- PrimaryDefSumStats %>% 
  ungroup %>% group_by(PrimaryDefName) %>%
  mutate(min_count = min(count)) %>% 
  filter(min_count == count) %>% 
  select(PrimaryDefName , avgPrimSepThrow  , avgPrimSepArrive , numCompletions , playerPlayCount , 
         avgPrimEnhancedSep , numTargets , PrimaryDefPosBasic , PrimaryDefPosition ) %>% 
  mutate(avgGapClosed = avgPrimSepThrow - avgPrimSepArrive) %>% 
  mutate(avgEnhancedGapClosed = avgPrimEnhancedSep - avgPrimSepArrive) %>% 
  mutate(receptionPercent = 100 * numCompletions / numTargets) %>% 
  mutate(targetPercent = 100 * numTargets / playerPlayCount)

PrimDefSumStatsDB <- filter(PrimaryDefSumStats , PrimaryDefPosBasic == "DB")
PrimDefSumStatsLB <- filter(PrimaryDefSumStats , PrimaryDefPosBasic == "LB")

# 

print("Feature Analysis 1 Completed")

