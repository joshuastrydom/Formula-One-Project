# Formula-One-Project
## Predictive model for the Formula One champion driver

## Packages
library(tidyverse)
library(dplyr)
library(lubridate)
library(glue)

## Data collection
library(readr)
circuits <- read_csv("F1 data/circuits.csv")
constructor_results <- read_csv("F1 data/constructor_results.csv")
constructor_standings <- read_csv("F1 data/constructor_standings.csv")
constructors <- read_csv("F1 data/constructors.csv")
driver_standings <- read_csv("F1 data/driver_standings.csv")
drivers <- read_csv("F1 data/drivers.csv")
F1_Constructor_Standings11 <- read_delim("F1 data/F1_Constructor_Standings11.csv", 
    delim = ";", escape_double = FALSE, trim_ws = TRUE)
F1_Driver_Standings11 <- read_delim("F1 data/F1_Driver_Standings11.csv", 
    delim = ";", escape_double = FALSE, trim_ws = TRUE)
F1_Driver_Standings <- read_csv("F1 data/F1_Driver_Standings.csv")
F1_Race_Results <- read_csv("F1 data/F1_Race_Results.csv")
races <- read_csv("F1 data/races.csv") 
results <- read_csv("F1 data/results.csv")
status <- read_csv("F1 data/status.csv") 
qualifying <- read_csv("F1 data/qualifying.csv") 

## Functions
### Function for filtering of races by their respective names
race_byname <- function(data1=data1, data2=data2, data3=data3, data4=data4){
    RD <- left_join(
        data1,
        data2,
        by = "driverId"
    ) |> arrange(resultId)
    Rc <- left_join(
        RD,
        data3,
        by = "constructorId"
    ) |> arrange(resultId)
    Rs <- left_join(
        Rc,
        data4,
        by = "statusId"
    ) |> arrange(resultId)
    Rs[, c(1,2,3,7,10,18,21,23,28,31)] |> filter(raceId > 859)
}
race_byname <- race_byname(data1=results, data2=drivers, data3=constructors, data4=status)
### Function for filtering of races by year
race_byrace <- left_join(
    race_byname,
    races,
    by = "raceId") |> arrange(resultId)
df_race_byrace <- data.frame(race_byrace)

df_race_byrace_x <- function(data = data, year = year){
    X <- data |> filter(year == year)
    X[,c(1:5, 7:10, 14)]
}
### Function for race winners by track
race_winners_by_track_func <- function(data = data, position = position, year = year){
    Y <- data |> filter(position == 1, year > 2011) |> arrange(year)
    Y[, c(1,2,3,7,21,23,32,33,34,35)]
}