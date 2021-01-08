### NFL Big Data Bowl 2020-21 
# Robert Bernhardt, Andrew Rogan, Daniel Weiss 
# October 2020 

## Import Data File 
# Data obtained from https://www.kaggle.com/c/nfl-big-data-bowl-2021/data
# Additional data obtained from https://www.kaggle.com/tombliss/additional-data-coverage-schemes-for-week-1?select=coverages_week1.csv
# Additional data obtained from https://www.kaggle.com/pauls60r/nfl-2018-weather-data  

## Data on Games, Players, Plays

ListVec = c("games" , "plays" , "players")

for (idx in 1:length(ListVec)){
  FileName = paste(MainDir , "/Data/" , ListVec[idx] , ".csv" , sep = "")
  assign(ListVec[idx] , read.csv(FileName))
}
print("Imported Game, Play, Players Data")

## Weekly Player Tracking Data 

for (idx in 1:17) {
  FileName = paste(MainDir , "/Data/week" , idx , ".csv" , sep = "")
  assign( paste("Week" , idx , sep = "" ) , read.csv(FileName)) 
  if (idx == 1 | idx == 5 | idx == 9 | idx == 13 | idx == 17){ 
    print(paste("Imported Week" , idx , "Data"))
    }
}

## Additional Data (Coverage & Targeted Receivers) 

FileName = paste(MainDir , "/Data/coverages_week1.csv" , sep = "")
CoveragesWeek1 <- read.csv(FileName)

FileName = paste(MainDir , "/Data/targetedReceiver.csv" , sep = "")
TargetedReceiver <- read.csv(FileName) 

print("Imported Coverage, Target Data") 

print("Data Importation Complete")
