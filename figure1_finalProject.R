#rm(list = ls())
#gc()

#load first month of first year (jan 2014) for testing purposes
datW <- read.csv("Z:\\stabibian\\github\\finalProject\\projectData\\fleet-monthly-csvs-10-v3-2014\\fleet-monthly-csvs-10-v3-2014-01-01.csv", stringsAsFactors = FALSE)

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("tidyverse")
#install.packages("terra")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(terra)
library(readxl)
library(purrr)
library(stringr)


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


#load all datasets
setwd("Z:\\stabibian\\github\\finalProject\\projectData\\fleet-monthly\\extracted_monthly")
xlsx_files <- list.files(
  path = ".",
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

length(xlsx_files)
head(xlsx_files)

fleet_data <- map_df(
  xlsx_files,
  \(path){
    df <- read_csv(path)
    
    ym <- str_extract(basename(path), "\\d{4}-\\d{2}") #tbh i have no idea what's going on here but it works
    
    df |> mutate(year_month = ym)
  }
)

fleet_data

#prep dataset 
fleet_data <- fleet_data %>%
  mutate(
    date = as.Date(date),
    month = floor_date(date, "month"),
    year = year(date),
    geartype = tolower(geartype),
    mmsi_present = ifelse(is.na(mmsi_present), 0, mmsi_present)
  )


#proxies for suspicion ##
df_tuna <- fleet_data %>% filter(geartype %in% tunaGear)

df_tuna <- df_tuna %>%
  mutate(dark_activity = ifelse(fishing_hours > 0 & mmsi_present == 0, 1, 0))

df_tuna <- df_tuna %>%
  mutate(illegal_gear_flag = ifelse(!(geartype %in% tunaGear), 1, 0))

cell_stats <- df_tuna %>%
  group_by(cell_ll_lat, cell_ll_lon) %>%
  summarise(mean_hours = mean(fishing_hours),
            sd_hours = sd(fishing_hours))

df_tuna <- df_tuna %>%
  left_join(cell_stats, by = c("cell_ll_lat", "cell_ll_lon")) %>%
  mutate(intense_fishing = ifelse(fishing_hours > mean_hours + 2*sd_hours, 1, 0))

#build suspicious score
df_tuna <- df_tuna %>%
  mutate(
    sus_score = 
      1.5 * dark_activity + 1.0 * intense_fishing
  )

monthly_sus <- df_tuna %>%
  group_by(month) %>%
  summarise(sus_hours = sum(sus_score, na.rm = TRUE)) ##

ggplot(monthly_sus, aes(x=month, y=sus_hours)) + 
  geom_line(color= "red", linewidth=1.1) +
  theme_minimal() +
  labs (
    title = "suspicious fishing activity",
    y="suspicious score (sum per month)",
    x="month"
  )
            
#load tuna prices
fao_raw <- read_csv("Z:\\stabibian\\github\\finalProject\\projectData\\FAO_fish_price_index_Sep2025 (1).csv", 
                    skip = 3, col_names = TRUE)



fao_tuna <- fao_raw %>%
 rename(
   date_raw = "Date",
   fao_index = "FAO Fish Price Index",
   tuna_price = "Tuna"
) %>%


select(date_raw, tuna_price) ##

#fao_tuna %>%
  #distinct(date_raw) %>%
  #slice(1:20)

#parse FAO dates
fao_tuna <- fao_tuna %>%
  mutate(
    date_clean = date_raw %>%
      str_trim() %>%
      str_replace_all("[^A-Za-z0-9-]", ""),
    date = parse_date_time(
      date_clean,
      orders = c("y-b", "yb")
    ) %>% as.Date(),
    tuna_price = as.numeric(tuna_price)
  ) %>%
  filter(!is.na(date), !is.na(tuna_price))

#aggregate to monthly
monthly_tuna <- fao_tuna %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    tuna_price = mean(tuna_price, na.rm = TRUE),
    .groups = "drop"
  )

#join
combined <- monthly_sus %>%
  inner_join(monthly_tuna, by = "month")

glimpse(combined)

combined_scale <- combined %>%
  mutate(
    tuna_z = as.numeric(scale(tuna_price)),
    sus_z = as.numeric(scale(sus_hours))
  ) %>%
  select(month, tuna_z, sus_z) %>%
  pivot_longer(
    cols = c(tuna_z, sus_z),
    names_to = "series",
    values_to = "value"
  )

#create graph of combined scale
ggplot(combined_scale, aes(x = month, y = value, color = series)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(tuna_z = "#0072B2", sus_z = "#D55E00"),
    labels = c(
      tuna_z = "Tuna price (z-score)",
      sus_z  = "Suspicious score (z-score)"
    ),
    name = NULL
  ) +
  labs(
    title = "Tuna prices vs suspicious fishing score",
    subtitle = "standardized (z-scores) for direct comparison",
    x = "Month",
    y = "Standardized Value (z-score)"
  ) +
  theme_grey(base_size = 13)


ggplot(fao_tuna, aes(x= date, y = tuna_price)) +
  geom_line(color = "blue", linewidth = 1) +
  labs (
    title = "tuna prices (2014-2024)",
    x = "Date",
    y = "Tuna Price"
  ) + 
  theme_minimal(base_size = 13)

#create plot that compares tuna prices with suspicious score





#load bathymetry data
bathy <- rast("Z:\\stabibian\\github\\finalProject\\projectData\\bathymetry.tif")
#crop to pacific islands
bathy_pi <- ext(120, 180, -30, 30)
bathy_crop <- crop(bathy, bathy_pi)
plot(bathy_crop)

#bathy_df <- as.data.frame(bathy_crop, xy = TRUE, na.rm = TRUE)
#colnames(bathy_df) <- c("x", "y", "depth")

#pts <- vect(tunaGear, geom=c("cell_ll_lon", "cell_ll_lat"), crs="EPSG:4326")
#tunaGear$depth_m <- terra::extract(bathy, pts)[,2]

