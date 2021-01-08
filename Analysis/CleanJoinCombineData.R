#Load packages

library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(plm)
library(foreign)
library(ggthemes)
library(magick)
library(fastDummies)
library(stringr)
library(broom)
library(magrittr)
library(Hmisc)
library(psych)

#Import
setwd("/Users/andrewrogan/Documents/Big-Data-Bowl-20/Data")

datsalary <- read.csv("SalaryCapData2018.csv")
datcombineresults <- read.csv("CombineResults.csv")
datplayers <- read.csv("players.csv")
datadd <- read.csv("measurablefixed.csv")
datdraft <- read.csv("DraftPickData.csv")
datroto <- read.csv("MeasurablesRotowire.csv")
dat2017 <- read.csv("CombineProDay2017Redo.csv")
datbrutedata <- read.csv("brutedataredo.csv")
datsepstats <- read.csv("DbSumStatsBackup.csv")

#filter 
#datsepstats <- filter(datsepstats, numTargets>15)

## clean salary data
# remove extra unjury information
datsalary <- subset(datsalary, !startsWith(displayName, "("))

#mark duplicates and subset
datsalaryrepeats <- datsalary %>% 
  group_by(displayName) %>% 
  filter(n()>1)

#drop these observations
datsalary <-datsalary[!(datsalary$displayName=="Andre Smith"),]
datsalary <-datsalary[!(datsalary$displayName=="Brandon Williams"),]
datsalary <-datsalary[!(datsalary$displayName=="Brian Allen"),]
datsalary <-datsalary[!(datsalary$displayName=="Chris Jones"),]
datsalary <-datsalary[!(datsalary$displayName=="Kyle Fuller"),]
datsalary <-datsalary[!(datsalary$displayName=="Marcus Williams"),]
datsalary <-datsalary[!(datsalary$displayName=="Michael Thomas"),]
datsalary <-datsalary[!(datsalary$displayName=="Ryan Griffin"),]

write.csv(datsalaryrepeats, "salaryrepeats.csv")

## clean up combine results data
# make college names consistent (change Combine Results names to match players.csv names)
datcombineresults <- datcombineresults %>%
  mutate(collegeName = case_when(collegeName == "Abilene Christian (TX)" ~ "Abilene Christian",
                                 collegeName == "Albany (NY)" ~ "Albany, N.Y.",
                                 collegeName == "Alcorn State (MS)" ~ "Alcorn State",
                                 collegeName == "Appalachian State (NC)" ~ "Appalachian State",
                                 collegeName == "Ashland (OH)" ~ "Ashland",
                                 collegeName == "Ball State (IN)" ~ "Ball State",
                                 collegeName == "Bethune-Cookman (FL)" ~ "Bethune Cookman",
                                 collegeName == "Bowie State (MD)" ~ "Bowie State",
                                 collegeName == "Brown (RI)" ~ "Brown",
                                 collegeName == "UC-Davis" ~ "California-Davis",
                                 collegeName == "Central Missouri" ~ "Central Missouri State",
                                 collegeName == "Tennessee-Chattanooga" ~ "Chattanooga",
                                 collegeName == "Central Missouri" ~ "Central Missouri State",
                                 collegeName == "Citadel (SC)" ~ "Citadel",
                                 collegeName == "Coastal Carolina (SC)" ~ "Coastal Carolina",
                                 collegeName == "Columbia (NY)" ~ "Columbia",
                                 collegeName == "Drake (IA)" ~ "Drake",
                                 collegeName == "East Carolina (NC)" ~ "East Carolina",
                                 collegeName == "East Central (OK)" ~ "East Central",
                                 collegeName == "Ferris State (MI)" ~ "Ferris State",
                                 collegeName == "Fordham (NY)" ~ "Fordham",
                                 collegeName == "Fresno State (CA)" ~ "Fresno State",
                                 collegeName == "Grambling State (LA)" ~ "Grambling",
                                 collegeName == "Grand Valley State (MI)" ~ "Grand Valley State",
                                 collegeName == "Hillsdale (MI)" ~ "Hillsdale",
                                 collegeName == "Howard (DC)" ~ "Howard",
                                 collegeName == "Jacksonville State (AL)" ~ "Jacksonville State",
                                 collegeName == "James Madison (VA)" ~ "James Madison",
                                 collegeName == "Kent State (OH)" ~ "Kent State",
                                 collegeName == "Laval" ~ "Laval, Can.",
                                 collegeName == "Liberty (VA)" ~ "Liberty",
                                 collegeName == "Lindenwood (MO)" ~ "Lindenwood",
                                 collegeName == "Marshall (WV)" ~ "Marshall",
                                 collegeName == "Midwestern State (TX)" ~ "Midwestern State",
                                 collegeName == "Mount Union (OH)" ~ "Mount Union",
                                 collegeName == "Nevada Las Vegas" ~ "Nevada-Las Vegas",
                                 collegeName == "Newberry (SC)" ~ "Newberry",
                                 collegeName == "Old Dominion (VA)" ~ "Old Dominion",
                                 collegeName == "Pittsburg State (KS)" ~ "Pittsburg State",
                                 collegeName == "Portland State (OR)" ~ "Portland State",
                                 collegeName == "Regina (Canada)" ~ "Regina, Can.",
                                 collegeName == "Rice (TX)" ~ "Rice",
                                 collegeName == "Richmond (VA)" ~ "Richmond",
                                 collegeName == "Sacramento State (CA)" ~ "Sacramento State",
                                 collegeName == "Saginaw Valley State (MI)" ~ "Saginaw Valley State",
                                 collegeName == "Sam Houston State (TX)" ~ "Sam Houston State",
                                 collegeName == "Samford (AL)" ~ "Samford",
                                 collegeName == "Shepherd (WV)" ~ "Shepherd",
                                 collegeName == "Southeastern Louisiana" ~ "Southeast Louisiana",
                                 collegeName == "Southern Methodist (TX)" ~ "Southern Methodist",
                                 collegeName == "Stephen F. Austin (TX)" ~ "Stephen F. Austin",
                                 collegeName == "Tiffin (OH)" ~ "Tiffin University",
                                 collegeName == "Towson (MD)" ~ "Towson",
                                 collegeName == "Troy (AL)" ~ "Troy",
                                 collegeName == "Tulane (LA)" ~ "Tulane",
                                 collegeName == "Tulsa (OK)" ~ "Tulsa",
                                 collegeName == "Valdosta State (GA)" ~ "Valdosta State",
                                 collegeName == "Wagner (NY)" ~ "Wagner",
                                 collegeName == "Weber State (UT)" ~ "Weber State",
                                 collegeName == "William & Mary (VA)" ~ "William & Mary",
                                 collegeName == "Youngstown State (OH)" ~ "Youngstown State",
                                 TRUE ~ collegeName))

datcombineresults <- datcombineresults %>%
  mutate(displayName = case_when(displayName=="TreDavious White" ~ "Tre'Davious White",
                                   TRUE ~ displayName))

## clean players.csv data
# edit inconsistent names in players.csv
datplayers <- datplayers %>%
  mutate(collegeName = case_when(collegeName == "California, Pa." ~ "California (PA)",
                                 collegeName == "Grambling State" ~ "Grambling",
                                 collegeName == "LSU" ~ "Louisiana State",
                                 collegeName == "Miami" ~ "Miami (FL)",
                                 collegeName == "Miami (Fla.)" ~ "Miami (FL)",
                                 collegeName == "Miami, O." ~ "Miami (OH)",
                                 collegeName == "Middle Tennessee" ~ "Middle Tennessee State",
                                 collegeName == "Monmouth, N.J." ~ "Monmouth (NJ)",
                                 collegeName == "Monmouth (N.J.)" ~ "Monmouth (NJ)",
                                 collegeName == "Northwestern (la)" ~ "Northwestern (LA)",
                                 collegeName == "Ohio U." ~ "Ohio",
                                 collegeName == "Prairie View" ~ "Prairie View A&M",
                                 collegeName == "Southeastern Louisiana" ~ "Southeast Louisiana",
                                 collegeName == "Southern U." ~ "Southern (LA)",
                                 collegeName == "USC" ~ "Southern California",
                                 TRUE ~ collegeName))

## joining
#concatenate name and college
datcombineresults <- datcombineresults %>%
  mutate(NameCollege = paste0(displayName, collegeName))

datplayers <- datplayers %>%
  mutate(NameCollege = paste0(displayName, collegeName))

###join players.csv and measurables file                
datplayersmeasurables <- right_join(datcombineresults, datplayers, by = "NameCollege")  

#rename
datplayersmeasurables <- rename(datplayersmeasurables, displayName = displayName.y)
datplayersmeasurables <- rename(datplayersmeasurables, collegeName = collegeName.y)

#delete extra columns
datplayersmeasurables$collegeName.x <- NULL
datplayersmeasurables$displayName.x <- NULL
datplayersmeasurables$NameCollege <- NULL

#mark duplicates and subset
datplayersmeasurablesrepeats <- datplayersmeasurables %>% 
  group_by(displayName) %>% 
  filter(n()>1)

#drop duplicates
datplayersmeasurables <-datplayersmeasurables[!(datplayersmeasurables$displayName=="Mike Davis"),]
write.csv(datplayersmeasurablesrepeats, "measurablerepeats.csv")

#combine salary data with measurables data
datfinal <- right_join(datsalary, datplayersmeasurables, by = c('displayName'))

#rbind these to the datfinal set
datadd <- filter(datadd, displayName== "Mike Davis")
datfinal <- rbind(datfinal, datadd)

###a bit more cleaning
#remove dollar signs and commas
datfinal$BaseSalary <- gsub("\\$|,", "", datfinal$BaseSalary)
datfinal$SigningBonus <- gsub("\\$|,", "", datfinal$SigningBonus)
datfinal$RosterBonus <- gsub("\\$|,", "", datfinal$RosterBonus)
datfinal$OptionBonus <- gsub("\\$|,", "", datfinal$OptionBonus)
datfinal$WorkoutBonus <- gsub("\\$|,", "", datfinal$WorkoutBonus)
datfinal$RestructuringBonus <- gsub("\\$|,", "", datfinal$RestructuringBonus)
datfinal$Misc. <- gsub("\\$|,", "", datfinal$Misc.)
datfinal$CapHit <- gsub("\\$|,", "", datfinal$CapHit)
datfinal$Misc. <- gsub("\\$|,", "", datfinal$Misc.)
datfinal$TeamBaseSalary <- gsub("\\$|,", "", datfinal$TeamBaseSalary)
datfinal$TeamSigningBonus <- gsub("\\$|,", "", datfinal$TeamSigningBonus)
datfinal$TeamCapHit <- gsub("\\$|,", "", datfinal$TeamCapHit)

#make numeric
datfinal$BaseSalary <- as.numeric(datfinal$BaseSalary)
datfinal$SigningBonus <- as.numeric(datfinal$SigningBonus)
datfinal$RosterBonus <- as.numeric(datfinal$RosterBonus)
datfinal$OptionBonus <- as.numeric(datfinal$OptionBonus)
datfinal$WorkoutBonus <- as.numeric(datfinal$WorkoutBonus)
datfinal$RestructuringBonus <- as.numeric(datfinal$RestructuringBonus)
datfinal$Misc. <- as.numeric(datfinal$Misc.)
datfinal$CapHit <- as.numeric(datfinal$CapHit)
datfinal$Misc. <- as.numeric(datfinal$Misc.)
datfinal$TeamBaseSalary <- as.numeric(datfinal$TeamBaseSalary)
datfinal$TeamSigningBonus <- as.numeric(datfinal$TeamSigningBonus)
datfinal$TeamCapHit <- as.numeric(datfinal$TeamCapHit)

#fix dead cap (have to treat this special to make it negative)
datfinal$DeadCap <- gsub("[$),]", "", datfinal$DeadCap)
datfinal$DeadCap <- as.numeric(gsub("^\\s*[(]", "-", gsub("[$),]", "", datfinal$DeadCap)))

#make height consistent and rename
datfinal <- datfinal %>% 
  mutate(BDBHeight = case_when(height == '5-6' ~ '66',
                            height == '5-7' ~ '67',
                            height == '5-8' ~ '68',
                            height == '5-9' ~ '69',
                            height == '5-10' ~ '70',
                            height == '5-11' ~ '71',
                            height == '6-0' ~ '72',
                            height == '6-1' ~ '73',
                            height == '6-2' ~ '74',
                            height == '6-3' ~ '75',
                            height == '6-4' ~ '76',
                            height == '6-5' ~ '77',
                            height == '6-6' ~ '78',
                            height == '6-7' ~ '79',
                            TRUE ~ height))
#make numeric variable
datfinal$BDBHeight <- as.numeric(datfinal$BDBHeight)
#delete old column
datfinal$height <- NULL

#rename weight
datfinal$BDBWeight <- datfinal$weight 
datfinal$weight  <- NULL

#rename shuttle
datfinal$ProShuttle <-datfinal$Shuttle
datfinal$Shuttle <- NULL

#rename forty
datfinal$FortyYardDash <-datfinal$X40Yard
datfinal$X40Yard <- NULL

#rename three cone
datfinal$ThreeConeDrill <- datfinal$X3Cone
datfinal$X3Cone <- NULL

#rename 60-yd shuttle 
datfinal$SixtyYardShuttle <- datfinal$X60YdShuttle
datfinal$X60YdShuttle <- NULL

#create DB Position
datfinal <- datfinal %>%
  mutate(DBPos = case_when(Position == "CB" ~ "DB",
                           Position == "FS" ~ "DB",
                           Position == "SS" ~ "DB",
                           Position == "S" ~ "DB",
                           TRUE ~ Position))

##clean draft pick data
datdraft <- rename(datdraft, displayName = player)
datdraft <- rename(datdraft, collegeName = college)
datdraft <- filter(datdraft, year < 2019)
datdraft <- datdraft[ -c(1, 3:4,6, 16:61) ]

## joining
#concatenate name and college
datdraft <- datdraft %>%
  mutate(NameCollege = paste0(displayName, collegeName))

datfinal <- datfinal %>%
  mutate(NameCollege = paste0(displayName, collegeName))

#Join draft pick data 
datfinal <- right_join(datdraft, datfinal, by = c('displayName', 'collegeName'))  

datfinal <- datfinal %>% 
  left_join(datdraft, by = "NameCollege") %>% 
  mutate(FortyYdDash = coalesce(FortyYardDash, forty)) %>% 
  mutate(FiveTenFiveShuttle = coalesce(ProShuttle, shuttle)) %>% 
  mutate(Vertical = coalesce(VertLeap, vertical)) %>% 
  mutate(ThreeConeDrill1 = coalesce(ThreeConeDrill, threecone)) %>% 
  mutate(BroadJump1 = coalesce(BroadJump, broad)) %>%
  mutate(BenchPress1 = coalesce(BenchPress, bench)) %>% 
  mutate(Height1 = coalesce(Height, height_inches)) %>% 
  mutate(Weight1 = coalesce(Weight, weight)) %>% 
  select(-FortyYardDash, -forty, -ProShuttle, -shuttle, -VertLeap, -vertical, -ThreeConeDrill, -BroadJump, -broad, -BenchPress, -bench, -Height, -height_inches, -Weight, -weight, -threecone)

#drop college name.y
datfinal$collegeName.y <- NULL
datfinal$NameCollege <- NULL
datfinal$displayName.y <- NULL

#rename again
datfinal <- rename(datfinal, BroadJump = BroadJump1)
datfinal <- rename(datfinal, ThreeConeDrill = ThreeConeDrill1)
datfinal <- rename(datfinal, ProShuttle = FiveTenFiveShuttle)
datfinal <- rename(datfinal, BenchPress = BenchPress1)
datfinal <- rename(datfinal, collegeName = collegeName.x)
datfinal <- rename(datfinal, displayName = displayName.x)
datfinal <- rename(datfinal, Height = Height1)
datfinal <- rename(datfinal, Weight = Weight1)

datfinalrepeats <- datfinal %>% 
  group_by(displayName) %>% 
  filter(n()>1)

# eliminate duplicate
datfinal$duplicate <- duplicated(datfinal[,1])
datfinal <-datfinal[!(datfinal$displayName=="Derrick Johnson" & datfinal$duplicate ==TRUE),]

#drop duplicate column
datfinal$duplicate <- NULL

datfinalrepeats <- datfinal %>% 
  group_by(displayName) %>% 
  filter(n()>1)

#subset and print players who didn't attend combine
datnocombine <- filter(datfinal, DBPos == "DB"|DBPos == "WR"|DBPos =="TE")
datnoarms <- filter(datnocombine, is.na(ArmLength))
datnoheight <- (filter(datnocombine, is.na(Height)))
datnoarmsCB <- filter(datnoarms, Position=="CB")

datfinal <- datfinal %>%
  mutate(BruteForce = case_when(Position == "CB" & is.na(ArmLength) ~ 1,
                                TRUE ~ 0))
datbruteforce <- filter(datfinal, BruteForce==1)

#print no combine
#write.csv(datbruteforce, "bruteforcearms.csv")
#write.csv(datnoarms, "datnoarms.csv")
#write.csv(datnoarmsCB, "datnoarmscb.csv")

#delete brute force observations
datfinal <- filter(datfinal, BruteForce != 1)

#merge in brute force data
datfinal <- rbind(datfinal, datbrutedata)

#merge height columns
datfinal <- datfinal %>%
  mutate(Height = case_when(is.na(Height) ~ BDBHeight,
                            TRUE ~ Height))

#merge weight columns
datfinal <- datfinal %>%
  mutate(Weight = case_when(is.na(Weight) ~ BDBWeight,
                            TRUE ~ Weight))

#years in league 
datfinal <- datfinal %>%
  mutate(YearsInLeague = (2018 - DraftYear) + 1)

##create BMI
#convert inches to meters
datfinal <- datfinal %>%
  mutate(HeightMeters = Height * .0254)

#convert pounds to kg
datfinal <- datfinal %>%
  mutate(WeightKG = Weight * 0.453592)

#BMI
datfinal <- datfinal %>%
  mutate(BMI = WeightKG/(HeightMeters)^2)

#Height to Arm Length Ratio
datfinal <- datfinal %>%
  mutate(HeightArmRatio = Height/ArmLength)

#rename 
datfinal <- rename(datfinal, FortyYardDash = FortyYdDash)

PrimDefSumStatsDB <- rename(PrimDefSumStatsDB, displayName = PrimaryDefName)

#DB subset
datDB <- filter(datfinal, DBPos == "DB")
datCB <- filter(datfinal, Position == "CB")

#join
datfull <- datfinal %>% 
  left_join(PrimDefSumStatsDB, by = "displayName")

datfull <- datfinal %>% 
  left_join(datsepstats, by = "displayName")

#DB subset
datfullDB <- filter(datfinal, DBPos == "DB")
datfullCB <- filter(datfull, Position == "CB")

##Separation Tables
#datfullCB
separationCB <- lm(avgPrimEnhancedSep ~ Height + BMI + BenchPress + FortyYardDash + BroadJump, data=datfullCB)
summary(separationCB)
tidy_lmfit <- tidy(separationCB)
tidy_lmfit
separationCB <- lm(avgPrimEnhancedSep ~ ArmLength + BMI + BenchPress + FortyYardDash + BroadJump, data=datfullCB)
tidy_lmfit <- tidy(separationCB)
tidy_lmfit
separationCB <- lm(avgPrimEnhancedSep ~ Height +  ArmLength + BMI + BenchPress + FortyYardDash + BroadJump, data=datfullCB)
tidy_lmfit <- tidy(separationCB)
tidy_lmfit

#close the gap
closethegapCB <- lm( ~ Height + BMI + BenchPress + FortyYardDash + BroadJump, data=datfullCB)
summary(closethegapCB)
tidy_lmfit <- tidy(closethegapCB)
tidy_lmfit
closethegapCB <- lm(CapHit ~ ArmLength + BMI + BenchPress + FortyYardDash + BroadJump, data=datfullCB)
tidy_lmfit <- tidy(closethegapCB)
tidy_lmfit
closethegapCB <- lm(CapHit ~ Height +  ArmLength + BMI + BenchPress + FortyYardDash + BroadJump, data=datfullCB)
tidy_lmfit <- tidy(closethegapCB)
tidy_lmfit

#ball skills 
ballskillsCB <- lm( ~ Height + BMI + BenchPress + FortyYardDash + BroadJump, data=datfullCB)
summary(ballskillsCB)
tidy_lmfit <- tidy(ballskillsCB)
tidy_lmfit
ballskillsCB <- lm(CapHit ~ ArmLength + BMI + BenchPress + FortyYardDash + BroadJump, data=datfullCB)
tidy_lmfit <- tidy(ballskillsCB)
tidy_lmfit
ballskillsCB <- lm(CapHit ~ Height +  ArmLength + BMI + BenchPress + FortyYardDash + BroadJump, data=datfullCB)
tidy_lmfit <- tidy(ballskillsCB)
tidy_lmfit

### salary regressions
## subset
datCByoung <- filter(datCB, YearsInLeague<=4)
datCBold <- filter(datCB, YearsInLeague>4)
##all cbs
#Height
armlengthCB <- lm(CapHit ~ Height + BMI + BenchPress + FortyYardDash + BroadJump, data=datCB)
summary(armlengthCB)
tidy_lmfit <- tidy(armlengthCB)
tidy_lmfit
#arm length
armlengthCB <- lm(CapHit ~ ArmLength + BMI + BenchPress + FortyYardDash + BroadJump, data=datCB)
summary(armlengthCB)
summary(armlengthCB)
tidy_lmfit <- tidy(armlengthCB)
tidy_lmfit

#Young corners 
armlengthCByoung <- lm(pick ~ ArmLength + BMI + BenchPress + FortyYardDash + Vertical, data=datCByoung)
summary(armlengthCByoung)
armlengthCByoung <- lm(pick ~ Height + BMI + BenchPress + FortyYardDash + Vertical, data=datCByoung)
summary(armlengthCByoung)

#Old corners
armlengthCBold <- lm(CapHit ~ ArmLength + BMI + BenchPress + FortyYardDash + Vertical , data=datCBold)
summary(armlengthCBold)
armlengthCBold <- lm(CapHit ~ Height + BMI + BenchPress + FortyYardDash + Vertical, data=datCBold)
summary(armlengthCBold)

# Additional Regressions 

armlength <- lm(BaseSalary ~ ArmLength + Weight + Vertical + BenchPress + FortyYardDash, data=datfinal)
armlength <- lm(BaseSalary ~ ArmLength + Weight + BenchPress + FortyYardDash + ProShuttle + BroadJump, data=datfinal)
armlength <- lm(BaseSalary ~ ArmLength + Height + Weight + BenchPress + FortyYardDash + ProShuttle + BroadJump, data=datfinal)
armlength <- lm(BaseSalary ~ Height + Weight + BenchPress + FortyYardDash + ProShuttle + BroadJump, data=datfinal)


summary(armlength)

armlengthCB <- lm(CapHit ~ ArmLength + Weight + BenchPress + FortyYardDash + ProShuttle + BroadJump, data=datCB)
summary(armlengthCB)
armlengthCB <- lm(CapHit ~ ArmLength + Height + Weight + BenchPress + FortyYardDash + ProShuttle + BroadJump, data=datCB)
summary(armlengthCB)
armlengthCB <- lm(CapHit ~ ArmLength + Height + Weight + BenchPress + FortyYardDash + BroadJump, data=datCB)
summary(armlengthCB)
armlengthCB <- lm(CapHit ~ Height + Weight + BenchPress + FortyYardDash + BroadJump, data=datCB)
summary(armlengthCB)


armlengthCB <- lm(CapHit ~ ArmLength  + BMI + BenchPress + FortyYardDash + BroadJump + pick, data=datCB)
summary(armlengthCB)
armlengthCB <- lm(pick ~ ArmLength + BMI + BenchPress + FortyYardDash + BroadJump, data=datCB)
summary(armlengthCB)

armlengthDB <- lm(CapHit ~ ArmLength + Weight + BenchPress + FortyYardDash + ProShuttle + BroadJump, data=datCB)
summary(armlengthDB)
armlengthDB <- lm(CapHit ~ ArmLength + Height + Weight + BenchPress + FortyYardDash + ProShuttle + BroadJump, data=datDB)
summary(armlengthDB)
armlengthDB <- lm(CapHit ~  Height + Weight + BenchPress + FortyYardDash  + Vertical, data=datDB)
summary(armlengthDB)
armlengthDB <- lm(pick ~  Height + Weight + BenchPress + FortyYardDash  + Vertical, data=datDB)
summary(armlengthDB)
armlengthDB <- lm(CapHit ~ Height + BMI + BenchPress + FortyYardDash + ProShuttle + BroadJump + Vertical, data=datDB)
summary(armlengthDB)
armlengthDB <- lm(CapHit ~ Height*BMI + BenchPress + FortyYardDash + ProShuttle + BroadJump + Vertical, data=datDB)
summary(armlengthDB)
armlengthDB <- lm(CapHit ~ Height + BMI + BenchPress + FortyYardDash + ProShuttle + BroadJump + pick, data=datDB)
summary(armlengthDB)
armlengthDB <- lm(CapHit ~ Height + BMI + BenchPress + FortyYardDash + ProShuttle + BroadJump + pick + DraftYear, data=datDB)
summary(armlengthDB)
armlengthDB <- lm(CapHit ~ Height+ ArmLength + BMI + BenchPress + FortyYardDash + ProShuttle + BroadJump + pick + YearsInLeague, data=datDB)
summary(armlengthDB)

armlengthCB <- lm(CapHit ~ Height+ ArmLength + BMI + BenchPress + FortyYardDash + ProShuttle + BroadJump + pick + YearsInLeague, data=datCB)
summary(armlengthCB)

armlengthCB <- lm(BaseSalary ~ ArmLength + FortyYardDash + Vertical, data=datCB)
summary(armlengthCB)

armlengthCByoung <- lm(pick ~ ArmLength + FortyYardDash + Vertical, data=datCByoung)
summary(armlengthCByoung)

armlengthCByoung <- lm(pick ~ ArmLength + Weight + FortyYardDash + Vertical, data=datCByoung)
summary(armlengthCByoung)


armlengthCByoung <- lm(pick ~ Height + Weight + FortyYardDash + Vertical, data=datCByoung)
summary(armlengthCByoung)

armlengthCByoung <- lm(pick ~ Height + Weight + FortyYardDash + Vertical, data=datCByoung)
summary(armlengthCByoung)

#this one too
armlengthCByoung <- lm(pick ~ Height + BMI + FortyYardDash + Vertical, data=datCByoung)
summary(armlengthCByoung)
armlengthCByoung <- lm(CapHit ~ Height + BMI + FortyYardDash + Vertical, data=datCByoung)
summary(armlengthCByoung)


#old 
armlengthCBold <- lm(CapHit ~ ArmLength + BMI + FortyYardDash + Vertical , data=datCBold)
summary(armlengthCBold)
armlengthCBold <- lm(CapHit ~ Height + BMI + FortyYardDash + Vertical, data=datCBold)
summary(armlengthCBold)


armlengthCBold <- lm(CapHit ~ ArmLength + BMI + FortyYardDash + Vertical + pick + YearsInLeague, data=datCBold)
summary(armlengthCBold)
armlengthCBold <- lm(CapHit ~ Height + BMI + FortyYardDash + Vertical + pick + YearsInLeague, data=datCBold)
summary(armlengthCBold)
