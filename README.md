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
### Function for drivers and constructors champions in each year
F1_Standings <- function(data1=data1, data2=data2, Year=Year, Pos=Pos){
    D <- data1 |> filter(Year >= 2012) |> filter(Pos == 1)
    C <- data2 |> filter(Year >= 2012) |> filter(Pos == 1)
    DC <- full_join(
        D, 
        C,
        by = "Year") |> arrange("Pos")
    names(DC)[1] <- "Position"
    names(DC)[2] <- "Drivers Championship"
    names(DC)[8] <- "Constructors Championship"
    names(DC)[5] <- "Driver Points"
    names(DC)[9] <- "Constructor Points"
    DC[, c(1,2,5,6,8,9)]
}
### Function for determining the race order each year
raceorder <- function(data1=data1, data2=data2, year=year){
    order <- data1[1:5] |> filter(year==year)
    order
    won <- left_join(
        order,
        data2,
        by = "round"
    ) |> filter(year.x==year, year.y==year)
    won
}
### Function for determining how many points the Champion driver achieved
winners_points <- function(data=data, year=year){
    W <- data |> filter(Year==year)
    WP <- W |> filter(Points == max(Points))
    WP
}
### Function for tracks at which the champion driver scored points but never won on
scored_points <- function(data1=data1, data2=data2, year=year){
    S <- data1 |> filter(year==year) |> arrange("Driver")
    C <- left_join(
        S,
        data2,
        by = "Driver"
    ) |> filter(Year.x == year, Year.y == year)
    names(C)[4] <- "Year"
    C[ , c(1,2,4,5,6)]
}
### Functions for points scored at certain stages
#### Function for points scored for 5 races
scored_points_F5 <- function(data=data, year=year){
    data |> filter(Round <= 5)
}
#### Function for points scored for 10 races
scored_points_F10 <- function(data=data, year=year){
    data |> filter(Round <= 10)
}
#### Function for points scored after 5 races
scored_points_A5 <- function(data=data, year=year){
    data |> filter(Round > 5)
}
#### Function for points scored after 10 races 
scored_points_A10 <- function(data=data, year=year){
    data |> filter(Round > 10)
}
### Function for number of points attained at certain stages
#### Function for points attained for 5 races
points_scored_F5 <- function(data1=data1){
    PS_F5 <- sum(data1$Points)
    PS_F5
}
#### Function for points attained for 10 races
points_scored_F10 <- function(data1=data1){
    PS_F10 <- sum(data1$Points)
    PS_F10
}
#### Function for points attained after 5 races
points_scored_A5 <- function(data1=data1){
    PS_A5 <- sum(data1$Points)
    PS_A5
}
#### Function for points attained after 10 races
points_scored_A10 <- function(data1=data1){
    PS_A10 <- sum(data1$Points)
    PS_A10
}
### Function for tracks at which the champion driver won on
won_races <- function(data1=data1, data2=data2, year=year){
    S <- data1 |> filter(year==year) |> arrange("Driver")
    C <- left_join(
        S,
        data2,
        by = "Driver"
    ) |> filter(Year.x == year, Year.y == year)
    names(C)[4] <- "Year"
    C[ , c(1,2,4,5,6)]
}
### Function for points scored by winning
#### Function for points scored by winning for 5 races
won_races_F5 <- function(data=data, year=year){
    data |> filter(Round <= 5)
}
#### Function for points scored by winning for 10 races
won_races_F10 <- function(data=data, year=year){
    data |> filter(Round <= 10)
}
#### Function for points scored by winning after 5 races
won_races_A5 <- function(data=data, year=year){
    data |> filter(Round > 5)
}
#### Function for points scored by winning after 10 races
won_races_A10 <- function(data=data, year=year){
    data |> filter(Round > 10)
}