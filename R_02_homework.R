#R - Homework 02

# Using the SQLite database contained in the 'homework.db' file, calculate the MEDIAN number
# of arrivals and departures at Los Angeles Airport for each of the 12 months of the year.
# Then save the result to a CSV file.
#
# Note that for each month you have information about several types of flights:
# you need to pre-sum them.

library(DBI)
library(RSQLite)
library(dbplyr)
library(dplyr)

R.02.database <- 'R_02_homework_database.db'
drv <- dbDriver("SQLite")

# Display mode:
con <- dbConnect(drv, R.02.database, flags = SQLITE_RO) 

# DBI (database interface):
dbGetQuery(con, "SELECT *
                FROM flights
                WHERE ReportPeriod = '01/01/2006 12:00:00 AM'")

# Create table with flights info:
flights <-  tbl(con, 'flights') %>% 
            as_tibble()

# Add package lubridate to work with dates:
library(lubridate)

# Replace ReportPeriod with Date column (date format), add column Year and Month:
flights <- flights %>%  transmute(Date = mdy(substr(ReportPeriod,1,10)),
                                  Year = year(Date),
                                  Month = month(Date),
                                  FlightType,
                                  Arrival_Departure,
                                  Domestic_International,
                                  FlightOpsCount)

        # Summarize flights in Month/Year/Arrival_Departure buckets:
flights <-  flights %>% group_by(Month,Year,Arrival_Departure) %>%
                        summarise(Flights_Sum = sum(FlightOpsCount)) %>%
        # Group by Month to calculate median (based on Flight_Sum column):
                        group_by(Month,Arrival_Departure) %>%
                        summarise(Flights_Median = median(Flights_Sum))


# Change Month column format:
month.name[(flights$Month)] # Creates vector that translates number into full month name
month.abb[(flights$Month)] # that would create 'Jan' instead of 'January'

flights$Month <- month.name[(flights$Month)] # Assign vector to dataset, replacing numbers with month names 

# Create .csv file with final data (w/o row index):
write.csv(flights, 'R_02_homework.csv', row.names = F)

