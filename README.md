# Formula-One-Project
## Predictive model for the Formula One champion driver

## Packages
library(tidyverse)
library(dplyr)
library(lubridate)
library(glue)
library(readr)  
library(purrr)  

## Data collection
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
points_per_driver <- aggregate(race_byrace$points,
                               by = list(race_byrace$surname, race_byrace$year),
                               FUN = sum)
df_points_per_driver <- data.frame(points_per_driver)
names(df_points_per_driver)[1] <- "Driver"
names(df_points_per_driver)[2] <- "Year"
names(df_points_per_driver)[3] <- "Points"

winners_points <- function(data=data, year=year){
    W <- data |> filter(Year==year)
    WP <- W |> filter(Points == max(Points))
    WP
}
### Function for tracks at which the champion driver scored points or won on
df_totalpoints <- data.frame(df_race_byrace[,c(5,8,9,11,12,13,14)])
names(df_totalpoints)[1] <- "Points"
names(df_totalpoints)[2] <- "Driver"
names(df_totalpoints)[3] <- "Constructor"
names(df_totalpoints)[4] <- "Year"
names(df_totalpoints)[5] <- "Round"
names(df_totalpoints)[6] <- "TrackId"
names(df_totalpoints)[7] <- "Track"

total_points <- function(data1=data1, data2=data2, year=year){
    P <- data1 |> filter(year==year) |> arrange("Driver")
    TP <- left_join(
        P,
        data2,
        by = "Driver"
    ) |> filter(Year.x == year, Year.y == year)
    names(TP)[4] <- "Year"
    TP[ , c(1,2,4,5,6,7)]
}
total_points_2012 <- total_points(data1=df_totalpoints, data2=champions, year=2012)
total_points_2013 <- total_points(data1=df_totalpoints, data2=champions, year=2013)
total_points_2014 <- total_points(data1=df_totalpoints, data2=champions, year=2014)
total_points_2015 <- total_points(data1=df_totalpoints, data2=champions, year=2015)
total_points_2016 <- total_points(data1=df_totalpoints, data2=champions, year=2016)
total_points_2017 <- total_points(data1=df_totalpoints, data2=champions, year=2017)
total_points_2018 <- total_points(data1=df_totalpoints, data2=champions, year=2018)
total_points_2019 <- total_points(data1=df_totalpoints, data2=champions, year=2019)
total_points_2020 <- total_points(data1=df_totalpoints, data2=champions, year=2020)
total_points_2021 <- total_points(data1=df_totalpoints, data2=champions, year=2021)
totalpoints_bound <- rbind(total_points_2012, total_points_2013, total_points_2014, total_points_2015, total_points_2016, total_points_2017, total_points_2018, total_points_2019, total_points_2020, total_points_2021) |> arrange("Year")
totalpoints_bound$TrackId <- as.character(totalpoints_bound$TrackId)
### Function for tracks at which the champion driver scored points but never won on
scorer <- df_race_byrace |> filter(points > 0) |> filter(points < 25)
df_scorer_points <- data.frame(scorer[,c(5,8,9,11,12,13, 14)])
names(df_scorer_points)[1] <- "Points"
names(df_scorer_points)[2] <- "Driver"
names(df_scorer_points)[3] <- "Constructor"
names(df_scorer_points)[4] <- "Year"
names(df_scorer_points)[5] <- "Round"
names(df_scorer_points)[6] <- "TrackId"
names(df_scorer_points)[7] <- "Track"
champions <- F1_Standings_Final[, c(4,7)]

scored_points <- function(data1=data1, data2=data2, year=year){
    S <- data1 |> filter(year==year) |> arrange("Driver")
    C <- left_join(
        S,
        data2,
        by = "Driver"
    ) |> filter(Year.x == year, Year.y == year)
    names(C)[4] <- "Year"
    C[ , c(1,2,4,5,6,7)]
}
scored_points_2012 <- scored_points(data1 = df_scorer_points, data2 = champions, year = 2012)
scored_points_2013 <-scored_points(data1 = df_scorer_points, data2 = champions, year = 2013)
scored_points_2014 <-scored_points(data1 = df_scorer_points, data2 = champions, year = 2014)
scored_points_2015 <-scored_points(data1 = df_scorer_points, data2 = champions, year = 2015)
scored_points_2016 <-scored_points(data1 = df_scorer_points, data2 = champions, year = 2016)
scored_points_2017 <-scored_points(data1 = df_scorer_points, data2 = champions, year = 2017)
scored_points_2018 <-scored_points(data1 = df_scorer_points, data2 = champions, year = 2018)
scored_points_2019 <-scored_points(data1 = df_scorer_points, data2 = champions, year = 2019)
scored_points_2020 <-scored_points(data1 = df_scorer_points, data2 = champions, year = 2020)
scored_points_2021 <-scored_points(data1 = df_scorer_points, data2 = champions, year = 2021)
scoredpoints_bound <- rbind(scored_points_2012, scored_points_2013, scored_points_2014, scored_points_2015, scored_points_2016, scored_points_2017, scored_points_2018, scored_points_2019, scored_points_2020, scored_points_2021) |> arrange("Year")
scoredpoints_bound$TrackId <- as.character(scoredpoints_bound$TrackId)
### Functions for data frames of points scored for certain stages
* Function for points scored for 5 races  
scored_points_F5 <- function(data=data, year=year){
    data |> filter(Round <= 5)
}
* Function for points scored for 10 races  
scored_points_F10 <- function(data=data, year=year){
    data |> filter(Round <= 10)
}
* Function for points scored after 5 races  
scored_points_A5 <- function(data=data, year=year){
    data |> filter(Round > 5)
}
* Function for points scored after 10 races  
scored_points_A10 <- function(data=data, year=year){
    data |> filter(Round > 10)
}
### Function for number (value) of points attained for certain stages
* Function for points attained for 5 races  
points_scored_F5 <- function(data1=data1){
    PS_F5 <- sum(data1$Points)
    PS_F5
}
* Function for points attained for 10 races  
points_scored_F10 <- function(data1=data1){
    PS_F10 <- sum(data1$Points)
    PS_F10
}
* Function for points attained after 5 races  
points_scored_A5 <- function(data1=data1){
    PS_A5 <- sum(data1$Points)
    PS_A5
}
* Function for points attained after 10 races  
points_scored_A10 <- function(data1=data1){
    PS_A10 <- sum(data1$Points)
    PS_A10
}
### Function for tracks at which the champion driver won on
winner <- df_race_byrace |> filter(position == 1)
df_winner_points <- data.frame(winner[,c(5,8,9,11,12,13,14)])
names(df_winner_points)[1] <- "Points"
names(df_winner_points)[2] <- "Driver"
names(df_winner_points)[3] <- "Constructor"
names(df_winner_points)[4] <- "Year"
names(df_winner_points)[5] <- "Round"
names(df_winner_points)[6] <- "TrackId"
names(df_winner_points)[7] <- "Track"

won_races <- function(data1=data1, data2=data2, year=year){
    S <- data1 |> filter(year==year) |> arrange("Driver")
    C <- left_join(
        S,
        data2,
        by = "Driver"
    ) |> filter(Year.x == year, Year.y == year)
    names(C)[4] <- "Year"
    C[ , c(1,2,4,5,6,7)]
}
won_races_2012 <- won_races(data1 = df_winner_points, data2 = champions, year = 2012)
won_races_2013 <-won_races(data1 = df_winner_points, data2 = champions, year = 2013)
won_races_2014 <-won_races(data1 = df_winner_points, data2 = champions, year = 2014)
won_races_2015 <-won_races(data1 = df_winner_points, data2 = champions, year = 2015)
won_races_2016 <-won_races(data1 = df_winner_points, data2 = champions, year = 2016)
won_races_2017 <-won_races(data1 = df_winner_points, data2 = champions, year = 2017)
won_races_2018 <-won_races(data1 = df_winner_points, data2 = champions, year = 2018)
won_races_2019 <-won_races(data1 = df_winner_points, data2 = champions, year = 2019)
won_races_2020 <-won_races(data1 = df_winner_points, data2 = champions, year = 2020)
won_races_2021 <-won_races(data1 = df_winner_points, data2 = champions, year = 2021)
wonraces_bound <- rbind(won_races_2012, won_races_2013, won_races_2014, won_races_2015, won_races_2016, won_races_2017, won_races_2018, won_races_2019, won_races_2020, won_races_2021) |> arrange("Year")
wonraces_bound$TrackId <- as.character(wonraces_bound$TrackId)
### Function for data frames of points scored by winning for certain stages
* Function for points scored by winning for 5 races  
won_races_F5 <- function(data=data, year=year){
    data |> filter(Round <= 5)
}
* Function for points scored by winning for 10 races  
won_races_F10 <- function(data=data, year=year){
    data |> filter(Round <= 10)
}
* Function for points scored by winning after 5 races  
won_races_A5 <- function(data=data, year=year){
    data |> filter(Round > 5)
}
* Function for points scored by winning after 10 races  
won_races_A10 <- function(data=data, year=year){
    data |> filter(Round > 10)
}
### Function for number (value) of points attained by winning for certain stages
* Function for points attained by winning for 5 races  
points_won_F5 <- function(data1=data1){
    PW_F5 <- sum(data1$Points)
    PW_F5
}
* Function for points attained by winning for 10 races  
points_won_F10 <- function(data1=data1){
    PW_F10 <- sum(data1$Points)
    PW_F10
}
* Function for points attained by winning after 5 races  
points_won_A5 <- function(data1=data1){
    PW_A5 <- sum(data1$Points)
    PW_A5
}
* Function for points attained by winning after 10 races  
points_won_A10 <- function(data1=data1){
    PW_A10 <- sum(data1$Points)
    PW_A10
}
### Function for total number of points attained by winning or scoring at certain stages
* Data to be used in totalpoint functions
winnerspoints <- rbind(winners_points_2012, winners_points_2013, winners_points_2014, winners_points_2015, winners_points_2016, winners_points_2017, winners_points_2018, winners_points_2019, winners_points_2020, winners_points_2021)
pointvalues_F5 <- c(points_scored_F5_2012, points_scored_F5_2013, points_scored_F5_2014, points_scored_F5_2015, points_scored_F5_2016, points_scored_F5_2017, points_scored_F5_2018, points_scored_F5_2019, points_scored_F5_2020, points_scored_F5_2021)
pointvalues_F10 <- c(points_scored_F10_2012, points_scored_F10_2013, points_scored_F10_2014, points_scored_F10_2015, points_scored_F10_2016, points_scored_F10_2017, points_scored_F10_2018, points_scored_F10_2019, points_scored_F10_2020, points_scored_F5_2021)
pointvalues_A5 <- c(points_scored_A5_2012, points_scored_A5_2013, points_scored_A5_2014, points_scored_A5_2015, points_scored_A5_2016, points_scored_A5_2017, points_scored_A5_2018, points_scored_A5_2019, points_scored_A5_2020, points_scored_F5_2021)
pointvalues_A10 <- c(points_scored_A10_2012, points_scored_A10_2013, points_scored_A10_2014, points_scored_A10_2015, points_scored_A10_2016, points_scored_A10_2017, points_scored_A10_2018, points_scored_A10_2019, points_scored_A10_2020, points_scored_A5_2021)
winvalues_F5 <- c(points_won_F5_2012, points_won_F5_2013, points_won_F5_2014, points_won_F5_2015, points_won_F5_2016, points_won_F5_2017, points_won_F5_2018, points_won_F5_2019, points_won_F5_2020, points_won_F5_2021)
winvalues_F10 <- c(points_won_F10_2012, points_won_F10_2013, points_won_F10_2014, points_won_F10_2015, points_won_F10_2016, points_won_F10_2017, points_won_F10_2018, points_won_F10_2019, points_won_F10_2020, points_won_F10_2021)
winvalues_A5 <- c(points_won_A5_2012, points_won_A5_2013, points_won_A5_2014, points_won_A5_2015, points_won_A5_2016, points_won_A5_2017, points_won_A5_2018, points_won_A5_2019, points_won_A5_2020, points_won_A5_2021)
winvalues_A10 <- c(points_won_A10_2012, points_won_A10_2013, points_won_A10_2014, points_won_A10_2015, points_won_A10_2016, points_won_A10_2017, points_won_A10_2018, points_won_A10_2019, points_won_A10_2020, points_won_A10_2021)
pointvalues <- data.frame(winnerspoints, pointvalues_F5, pointvalues_F10, pointvalues_A5, pointvalues_A10, winvalues_F5, winvalues_F10, winvalues_A5, winvalues_A10)
* Function for total points (value) attained for 5 races  
totalpoints_F5 <- function(data=data, year=Year){
    P_F5 <- data |> filter(year==Year)
    TP_F5 <- P_F5[,4] + P_F5[,8]
    TP_F5
}
* Function for total points (value) attained for 10 races  
totalpoints_F10 <- function(data=data, year=Year){
    P_F10 <- data |> filter(year==Year)
    TP_F10 <- P_F10[,5] + P_F10[,9]
    TP_F10
}
* Function for total points (value) attained after 5 races  
totalpoints_A5 <- function(data=data, year=Year){
    P_A5 <- data |> filter(year==Year)
    TP_A5 <- P_A5[,6] + P_A5[,10]
    TP_A5
}
* Function for total points (value) attained after 10 races 
totalpoints_A10 <- function(data=data, year=Year){
    P_A10 <- data |> filter(year==Year)
    TP_A10 <- P_A10[,7] + P_A10[,11]
    TP_A10
}
### Function for accumulation of points over the season
accumpoints <- function(data1=data1, data2=data2, year=Year){
    A <- data1 |> filter(year==Year) |> arrange("Driver")
    P <- left_join(
        A,
        data2,
        by = "Driver"
    ) |> filter(Year.x == year, Year.y == year)
    names(P)[4] <- "Year"
    P$Accumulated.Points = cumsum(P$Points)
    P[, c(1,2,4,5,6,7,9)]
}
accumpoints_2012 <- accumpoints(data1=df_totalpoints, data2=champions, year=2012)
accumpoints_2013 <- accumpoints(data1=df_totalpoints, data2=champions, year=2013)
accumpoints_2014 <- accumpoints(data1=df_totalpoints, data2=champions, year=2014)
accumpoints_2015 <- accumpoints(data1=df_totalpoints, data2=champions, year=2015)
accumpoints_2016 <- accumpoints(data1=df_totalpoints, data2=champions, year=2016)
accumpoints_2017 <- accumpoints(data1=df_totalpoints, data2=champions, year=2017)
accumpoints_2018 <- accumpoints(data1=df_totalpoints, data2=champions, year=2018)
accumpoints_2019 <- accumpoints(data1=df_totalpoints, data2=champions, year=2019)
accumpoints_2020 <- accumpoints(data1=df_totalpoints, data2=champions, year=2020)
accumpoints_2021 <- accumpoints(data1=df_totalpoints, data2=champions, year=2021)

accumpoints_bound <- rbind(accumpoints_2012, accumpoints_2013, accumpoints_2014, accumpoints_2015, accumpoints_2016, accumpoints_2017, accumpoints_2018, accumpoints_2019, accumpoints_2020, accumpoints_2021) |> arrange("Year")
accumpoints_bound$TrackId <- as.character(accumpoints_bound$TrackId)
* Function for accumulation of points during the first 10 rounds  
accumpoints_F10 <- function(data1=data1, data2=data2, year=Year){
    A <- data1 |> filter(year==Year) |> filter(Round <= 10) |> arrange("Driver")
    P <- left_join(
        A,
        data2,
        by = "Driver"
    ) |> filter(Year.x == year, Year.y == year)
    names(P)[4] <- "Year"
    P$Accumulated.Points = cumsum(P$Points)
    P[, c(1,2,4,5,6,8)]
}
* Function for accumulation of points after the first 10 rounds  
accumpoints_A10 <- function(data1=data1, data2=data2, year=Year){
    A <- data1 |> filter(year==Year) |> filter(Round > 10) |> arrange("Driver")
    P <- left_join(
        A,
        data2,
        by = "Driver"
    ) |> filter(Year.x == year, Year.y == year)
    names(P)[4] <- "Year"
    P$Accumulated.Points = cumsum(P$Points)
    P[, c(1,2,4,5,6,8)]
}
### Functions for plotting 
* Function for plotting of total points  
plottotalpoints <- function(data1=data1, year=Year){
    PTP <- data1 |> arrange(Year) |> filter(year == Year) 
    plot <- PTP |> 
        ggplot() +
        geom_point(aes(x=Round, y=Points, color = TrackId), alpha = 0.8, size = 3) +
        theme_bw() +
        theme(legend.background = element_rect(fill = "white", size = 1, color = "white"), 
              legend.text = element_text(size = 6), 
              legend.title = element_text(size = 10, face = "bold"),
              plot.title = element_text(size = 12, face = "bold"),
              aspect.ratio = 9 / 16,
              legend.position = "bottom") +
        guides(color = guide_legend(override.aes = list(size = 4))) +
        labs(title = "Total points for the champion")
    plot
}
* Function for plotting of total points for 10 races  
plottotalpoints_F10 <- function(data1=data1, year=Year){
    PTP <- data1 |> arrange(Year) |> filter(year == Year) |> filter(Round <= 10)
    plot <- PTP |> 
        ggplot() +
        geom_point(aes(x=Round, y=Points, color = TrackId), alpha = 0.8, size = 3) +
        theme_bw() +
        theme(legend.background = element_rect(fill = "white", size = 1, color = "white"), 
              legend.text = element_text(size = 6), 
              legend.title = element_text(size = 10, face = "bold"),
              plot.title = element_text(size = 12, face = "bold"),
              aspect.ratio = 9 / 16,
              legend.position = "bottom") +
        guides(color = guide_legend(override.aes = list(size = 4))) +
        labs(title = "Total points for the champion for 10 races")
    plot
}
* Function for plotting of total points after the tenth race  
plottotalpoints_A10 <- function(data1=data1, year=Year){
    PTP <- data1 |> arrange(Year) |> filter(year == Year) |> filter(Round > 10)
    plot <- PTP |> 
        ggplot() +
        geom_point(aes(x=Round, y=Points, color = TrackId), alpha = 0.8, size = 3) +
        theme_bw() +
        theme(legend.background = element_rect(fill = "white", size = 1, color = "white"), 
              legend.text = element_text(size = 6), 
              legend.title = element_text(size = 10, face = "bold"),
              plot.title = element_text(size = 12, face = "bold"),
              aspect.ratio = 9 / 16,
              legend.position = "bottom") +
        guides(color = guide_legend(override.aes = list(size = 4))) +
        labs(title = "Total points for the champion after the tenth race")
    plot
}
* Function for plotting of 'scoring' points  
plotscoring <- function(data1=data1, year=Year){
    PS <- data1 |> arrange(Year) |> filter(year == Year) 
    plot <- PS |> 
        ggplot() +
        geom_point(aes(x=Round, y=Points, color = TrackId), alpha = 0.8, size = 3) +
        theme_bw() +
        theme(legend.background = element_rect(fill = "white", size = 1, color = "white"), 
              legend.text = element_text(size = 6), 
              legend.title = element_text(size = 10, face = "bold"),
              plot.title = element_text(size = 12, face = "bold"),
              aspect.ratio = 9 / 16,
              legend.position = "bottom") +
        guides(color = guide_legend(override.aes = list(size = 4))) +
        labs(title = "Total 'scoring' points for the champion")
    plot
}
* Function for plotting of 'winning' points  
plotwinning <- function(data1=data1, year=Year){
    PW <- data1 |> arrange(Year) |> filter(year == Year) 
    plot <- PW |> 
        ggplot() +
        geom_point(aes(x=Round, y=Points, color = TrackId), alpha = 0.8, size = 3) +
        theme_bw() +
        theme(legend.background = element_rect(fill = "white", size = 1, color = "white"), 
              legend.text = element_text(size = 6), 
              legend.title = element_text(size = 10, face = "bold"),
              plot.title = element_text(size = 12, face = "bold"),
              aspect.ratio = 9 / 16,
              legend.position = "bottom") +
        guides(color = guide_legend(override.aes = list(size = 4))) +
        labs(title = "Total 'winning' points for the champion")
    plot
}
* Function for plotting of accumulation of points  
plotaccumpoints <- function(data1=data1, year=Year){
    PAP <- data1 |> arrange(Year) |> filter(year == Year) 
    plot <- PAP |> 
        ggplot() +
        geom_point(aes(x = Round, y = Accumulated.Points, color = TrackId), alpha = 0.8, size = 3) +
        theme_bw() +
        theme(legend.background = element_rect(fill = "white", size = 1, color = "white"), 
              legend.text = element_text(size = 6), 
              legend.title = element_text(size = 10, face = "bold"),
              plot.title = element_text(size = 12, face = "bold"),
              aspect.ratio = 9 / 16,
              legend.position = "bottom") +
        guides(color = guide_legend(override.aes = list(size = 4))) +
        labs(title = "Total accumulated points for the champion")
    plot
}
* Function for plotting of accumulation of points for the first 10 races  
plotaccumpoints_F10 <- function(data1=data1, year=Year){
    PAP <- data1 |> arrange(Year) |> filter(year == Year) |> filter(Round <= 10)
    plot <- PAP |> 
        ggplot() +
        geom_point(aes(x = Round, y = Accumulated.Points, color = TrackId), alpha = 0.8, size = 3) +
        theme_bw() +
        theme(legend.background = element_rect(fill = "white", size = 1, color = "white"), 
              legend.text = element_text(size = 6), 
              legend.title = element_text(size = 10, face = "bold"),
              plot.title = element_text(size = 12, face = "bold"),
              aspect.ratio = 9 / 16,
              legend.position = "bottom") +
        guides(color = guide_legend(override.aes = list(size = 4))) +
        labs(title = "Total accumulated points for the champion for 10 races")
    plot
}
* Function for plotting of accumulation of points after the first 10 races  
plotaccumpoints_A10 <- function(data1=data1, year=Year){
    PAP <- data1 |> arrange(Year) |> filter(year == Year) |> filter(Round > 10)
    plot <- PAP |> 
        ggplot() +
        geom_point(aes(x = Round, y = Accumulated.Points, color = TrackId), alpha = 0.8, size = 3) +
        theme_bw() +
        theme(legend.background = element_rect(fill = "white", size = 1, color = "white"), 
              legend.text = element_text(size = 6), 
              legend.title = element_text(size = 10, face = "bold"),
              plot.title = element_text(size = 12, face = "bold"),
              aspect.ratio = 9 / 16,
              legend.position = "bottom") +
        guides(color = guide_legend(override.aes = list(size = 4))) +
        labs(title = "Total accumulated points for the champion after the first 10 races")
    plot
}