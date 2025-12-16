rm(list = ls())
gc()

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

#create hotspot map based on data from one month (jan 2014) for testing purposes
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
#low key don't hotspot include in final analysis

#load all datasets
setwd("Z:\\stabibian\\github\\finalProject\\projectData\\fleet-monthly\\extracted_monthly")
xlsx_files <- list.files(
  path = ".",
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

fleet_data <- map_df(
  xlsx_files,
  \(path){
    df <- read_csv(path)
    
    ym <- str_extract(basename(path), "\\d{4}-\\d{2}") #tbh i have no idea what's going on here but it works
    
    df |> mutate(year_month = ym)
  }
)

fleet_data

#prep dataset for analysis
fleet_data <- fleet_data %>%
  mutate(
    date = as.Date(date),
    month = floor_date(date, "month"),
    year = year(date),
    geartype = tolower(geartype),
    mmsi_present = ifelse(is.na(mmsi_present), 0, mmsi_present)
  )


#proxies for suspicion #
df_tuna <- fleet_data %>% filter(geartype %in% tunaGear)

df_tuna <- df_tuna %>%
  mutate(dark_activity = ifelse(fishing_hours > 0 & mmsi_present == 0, 1, 0))

df_tuna <- df_tuna %>%
  mutate(illegal_gear_flag = ifelse(!(geartype %in% tunaGear), 1, 0))

cell_stats <- df_tuna %>%
  group_by(cell_ll_lat, cell_ll_lon) %>%
  summarise(mean_hours = mean(fishing_hours),
            sd_hours = sd(fishing_hours))

#join df_tuna with cell_stats
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

#create plot for sus_score
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


#prep dataset
fao_tuna <- fao_raw %>%
 rename(
   date_raw = "Date",
   fao_index = "FAO Fish Price Index",
   tuna_price = "Tuna"
) %>%


select(date_raw, tuna_price)


#parse FAO dates
fao_tuna <- fao_tuna %>%
  mutate(
    date_clean = date_raw %>%
      str_trim() %>%
      str_replace_all("[^A-Za-z0-9-]", ""), #lowercase
    date = parse_date_time(
      date_clean,
      orders = c("y-b", "yb") #fix date format
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

#join to make a combined graph on same scale
combined <- monthly_sus %>%
  inner_join(monthly_tuna, by = "month")

glimpse(combined)

combined_scale <- combined %>%
  mutate(
    tuna_z = as.numeric(scale(tuna_price)), #puts these two very different variables on same scale
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

#run analysis on tuna prices vs sus fishing

#linear regression
lm <- lm(sus_hours ~ tuna_price, data = combined)
summary(lm)

#create price_regime column for simplicity
#create 80th percentile threshold (80% over the mean of 2014-2024)
p80 <- quantile(combined$tuna_price, 0.80, na.rm = TRUE)

#create high/low label for each month
combined <- combined %>%
  mutate(
    price_regime = ifelse(tuna_price >= p80, "High Price", "Low/Normal Price")
  )

table(combined$price_regime)

#welch t test
t.test(sus_hours ~ price_regime, data = combined) #p-value=0.31

#visual comparison box plot
ggplot(combined, aes(x = price_regime,y = sus_hours, fill = price_regime)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Suspicious Fishing During High vs Low Tuna Price Periods",
    x = "Price Regime",
    y = "Monthly Suspicious Score"
  ) +
  scale_fill_manual(values = c("Low/Normal Price"="grey70","High Price"="red")) +
  theme_minimal()


#plot of just tuna price index
ggplot(fao_tuna, aes(x= date, y = tuna_price)) +
  geom_line(color = "blue", linewidth = 1) +
  labs (
    title = "tuna prices (2014-2024)",
    x = "Date",
    y = "Tuna Price"
  ) + 
  theme_minimal(base_size = 13)



#lag graph
combined <- combined %>%
  arrange(month) %>%
  mutate(
    price_high = ifelse(price_regime == "High Price", 1, 0)
  )


#check

table(combined$price_regime, combined$price_high)

lags <- 0:6 #0-6 months lag
lag_df <- map_dfr(lags, \(L){
  tmp <- combined %>%
    mutate(price_high_lag = dplyr::lag(price_high, n = L))
  
  tibble(
    lag = L,
    cor = cor(tmp$sus_hours, tmp$price_high_lag, use = "complete.obs")
  )
})
                  

lag_df

#plot lag graph
ggplot(lag_df, aes(x = lag, y = cor)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Relationship Lag Between High Price Months and Suspicious Fishing",
    x = "lag in months (price leads to suspicious activity)",
    y = "correlation (r)"
  ) + 
  
  theme_minimal(base_size = 13)

#create lag graph for 1 month lag (most significant at 1 month)

lag_chosen <- 1

lag_plot_df <- combined %>%
  arrange(month) %>%
  mutate(
    price_high = ifelse(price_regime == "High Price", 1, 0),
    price_high_lag = dplyr::lag(price_high, n = lag_chosen)
  ) 


#fit the regression model
fit <- lm(sus_hours ~ price_high_lag, data = lag_plot_df)
summary_fit <- summary(fit)
summary(fit)

slope <- coef(fit)["price_high_lag"]
r2 <- summary_fit$r.squared
p_value <- summary_fit$coefficients["price_high_lag", "Pr(>|t|)"]

#round
slope_txt <- round(slope, 2)
r2_txt <- round(r2, 2)
p_txt <- signif(p_value, 3)
p_label <- if (p_value < 0.001) "< 0.001" else as.character(p_txt)

lag_plot <- ggplot(lag_plot_df, aes(x = price_high_lag, y = sus_hours)) +
  geom_jitter(width = 0.05, height = 0, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "magenta", linewidth = 1) +
  scale_x_continuous(breaks = c(0,1),
                     labels = c("Low/Normal Price", "High Price")) +
labs(
  title = paste("Suspicious Fishing vs High Price Indicator (Lag =", lag_chosen, "month)" ),
  subtitle = paste0("linear model: sus_hours ~ price_high_lag | ",
  "slope = ", slope_txt, #put results directly onto graph
  ", R^2 = ", r2_txt,
  ", p = ", p_label
),
  x = "high price indicator (0 = low price month, 1 = high price month",
  y = "suspicious score (sus_hours)"
) +
  theme_minimal(base_size = 13)
  
print(lag_plot)



###################################################################################################################
#create hotspot map# 
##################################################################################################################

##note*** don't include in final analysis

#define pacific island region

df_tuna_pac <- df_tuna %>%
  filter(
    geartype %in% tunaGear,
    cell_ll_lat > -30, cell_ll_lat < 30,
    (cell_ll_lon >= 120 & cell_ll_lon <= 180) |
      (cell_ll_lon >= -180 & cell_ll_lon <= -140)
  ) %>%
  
  filter(sus_score > 0)

#check if sus_score is there and numeric

#names(df_tuna_pac)
#str(df_tuna_pac$sus_score)

#prep bathy for ggplot -- was orginally going to use terra::density but it's not working
#load bathymetry data
bathy <- terra::rast("Z:\\stabibian\\github\\finalProject\\projectData\\bathymetry.tif")

#crop to pacific islands
bathy_pi <- terra::ext(120, 180, -30, 30)
bathy_crop <- terra::crop(bathy, bathy_pi)

#convert to a data frame so that colunms lat, lon, and depth in same space as fishing data
bathy_df <- as.data.frame(bathy_crop, xy = TRUE, na.rm = TRUE)
colnames(bathy_df) <- c("lon", "lat", "depth")


#use ggplot to deal with kernel density

ggplot() +
  geom_raster(data = bathy_df,
              aes(x = lon, y = lat, fill = depth)) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1,
    name = "Depth (m)"
  ) +
  
  #hot spot later and kernel density of sus score
  
  stat_density_2d(
    data = df_tuna_pac,
    aes(x = cell_ll_lon,
        y = cell_ll_lat,
        weight = sus_score,
        fill = after_stat(level)),
    geom = "polygon",
    alpha = 0.6,
    color = NA
  ) +
  
  scale_fill_viridis_c(
    option = "magma",
    name = "Suspicious\nDensity",
    guide = "colorbar"
  ) +
  
  coord_quickmap(xlim = c(120, 180),
                 ylim = c(-30, 30),
                 expand = FALSE) +
  
  labs(
    title = "Hotspots of Suspicious Tuna Fishing Activity",
    subtitle = "kernel density of composite suspicious score over Pacific",
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_minimal(base_size = 13)
  



