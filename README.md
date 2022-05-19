# Formula-One-Project
## Predictive model for the Formula One champion driver

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