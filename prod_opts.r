### NFL Big Data Bowl 2020-21 
# Robert Bernhardt, Andrew Rogan, Daniel Weiss 
# October 2020 

## Production Options 

## Establish Packages & Libraries 
library(ggplot2)
library(tidyverse)
library(dplyr)
library(png)
library(grid)
library(magrittr)
library(ggforce)
library(psych)
library(jpeg)

## Adjusting Options 
options(max.print=2500)

## User-Made Functions

# Clear 
clear <- function() { 
  cat("\f")
}

# ISID (Jared E. Knowles , https://rdrr.io/cran/eeptools/src/R/isid.R)
isid <- function(data, vars, verbose = FALSE){
  unique <- nrow(data[!duplicated(data[, vars]),])
  total <- nrow(data)
  if(verbose == FALSE){
    unique == total
  } else{
    cat("Are variables a unique ID?\n")
    print(unique == total)
    cat("Variables define this many unique rows:\n")
    print(unique)
    cat("There are this many total rows in the data:\n")
    print(total)
  }
}

# Select Groups (https://stackoverflow.com/questions/26503350/how-to-extract-one-specific-group-in-dplyr)
select_groups <- function(data, groups, ...) 
  data[sort(unlist(attr(data, "indices")[ groups ])) + 1, ]

# Determine Angle Based on the Law of Cosines 
LawOfCosines <- function(a,b,c) { # Law of Cosines Function 
  # a, b, c are distances. The segment opposite the angle in question is c. 
  angle = acos( (a^2+b^2-c^2) / (2 * a * b)) * 180 / pi # Returns angle in degrees 
  return(angle) 
}

# Determine Difference in Orientation or Direction 
OrientationDiff <- function(dir1,dir2) { 
  
  dir_max = max(dir1,dir2)
  dir_min = min(dir1,dir2)
  diff = dir_max - dir_min 
  
  if (diff > 180) { 
    dir_max = dir_max - 360
    diff = dir_min - dir_max 
  }
  
  comment = paste("Dir 1:" , dir1 , "Dir 2:" , dir2 , "Dir Max:" , dir_max , "Difference:" , diff)
  return(diff)
}


