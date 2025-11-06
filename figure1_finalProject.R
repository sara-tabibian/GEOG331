#load first month of first year (jan 2014) for testing purposes
datW <- read.csv("Z:\\stabibian\\github\\finalProject\\projectData\\fleet-monthly-csvs-10-v3-2014\\fleet-monthly-csvs-10-v3-2014-01-01.csv", stringsAsFactors = FALSE)

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("lubridate")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)

datW <- datW %>%
  mutate(
    date = as.Date(date),
    month = floor_date(date, "month"),
    dark_activity = ifelse(fishing_hours > 0 & mmsi_present == 0, 1, 0)
  )

tunaGear <- c("tuna_purse_seines", "drifting_longlines", "pole_and_line", "trawlers")
pacific_datW <- datW %>%
  filter (geartype %in% tunaGear,
          cell_ll_lon > 120 & cell_ll_lon < -120 | cell_ll_lon < -140, # coordinate area of the pacific 
          cell_ll_lat < 23.5 & cell_ll_lat > -23.5) #tropic pacific belt

ggplot(pacific_datW, aes(x = cell_ll_lon, y = cell_ll_lat)) + geom_bin2d(bins = 60, aes(weight = fishing_hours)) +
  scale_fill_viridis_c(name = "Fishing hours") +
  labs (
    title = "Tuna fishing effort in the pacific islands region",
    subtitle = "aggregated by grid cell (lighter = more hours)",
    x = "longitude", y = "latitude"
  ) + 
  theme_minimal()