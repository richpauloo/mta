library(tidyverse)
library(sf)
library(mapview)
library(glue)
library(ggdist)
library(tidycensus)
library(mapgl)
library(htmltools)

# Data accessed 2024-10-13 from: list links.
# station entrance/exit
sta <- read_csv("data/MTA_Subway_Entrances_and_Exits__2024_20241013.csv") %>% 
  select(-entrance_georeference) %>% 
  janitor::clean_names() %>% 
  mutate(entrance_type2 = ifelse(entrance_type == "Elevator", "Elevator", "Other")) %>% 
  st_as_sf(coords = c("entrance_longitude", "entrance_latitude"), crs = 4329) 

sta %>% 
  filter(entrance_type2 == "Elevator") %>% 
  mapview(zcol = "entrance_type2")

# weekly ridership
wr <- read_csv("data/MTA_NYCT_MetroCard_History__2010_-_2021_20241013.csv") %>% 
  janitor::clean_names()

wr %>% 
  filter(station %in% wr$station[2:3]) %>% 
  mutate(
    from_date = mdy(from_date),
    year = year(from_date),
    month = month(from_date)
  ) %>% 
  group_by(year, month, station) %>% 
  summarize(full_fare = sum(full_fare, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(dt = ymd(glue("{year}-{month}-15"))) %>% 
  ggplot(aes(station, full_fare)) +
  stat_halfeye(
    aes(y = full_fare),
    orientation = "vertical",
    color = "transparent",
    .width = c(0.5, 0.8, 1),
    slab_alpha = 0.3,
    fill = "grey",               # Set the color of the half distribution
    point_interval = "median_qi", # Only show median as point
    point_size = 3               # Set size of median point
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    color = "black",
    size = 2 # Set size of the median point
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major.y = element_blank()
  )


# hourly ridership
hr <- data.table::fread("data/MTA_Subway_Hourly_Ridership__Beginning_February_2022_20241013.csv")

# by borough
hrb <- hr %>% 
  # head(100000) %>%
  mutate(
    transit_timestamp = mdy_hms(transit_timestamp),
    hour = hour(transit_timestamp), 
    dow  = wday(transit_timestamp, label = TRUE),
    fare_group = case_when(
      fare_class_category %in% c("OMNY - Seniors & Disability", 
                                 "Metrocard - Seniors & Disability") ~ "Seniors & Disability",
      fare_class_category %in% c("Metrocard - Full Fare", "Metrocard - Unlimited 7-Day", 
                                 "Metrocard - Unlimited 30-Day", "OMNY - Full Fare", 
                                 "Metrocard - Other", "OMNY - Other") ~ "Full Fare",
      fare_class_category %in% c("Metrocard - Students", "OMNY - Students") ~ "Students",
      fare_class_category %in% c("Metrocard - Fair Fare", "OMNY - Fair Fare") ~ "Fair Fare",
      TRUE ~ "Other" # optional to catch anything not covered by the case_when
    )
  ) %>% 
  filter(
    transit_timestamp >= ymd_hms("2023-01-01 00:00:00") & 
    transit_timestamp <= ymd_hms("2023-12-31 24:00:00")
  ) %>% 
  group_by(fare_group, dow, hour, borough) %>% 
  summarize(ridership = sum(ridership)/52) %>% 
  ungroup()

hrb %>% 
  group_by(dow, hour, borough) %>% 
  mutate(total_ridership = sum(ridership), 
         ridership_proportion = ridership / total_ridership) %>% 
  ungroup() %>% 
  filter(fare_group != "Full Fare") %>%
  ggplot(aes(hour, ridership_proportion, color = borough)) +
  geom_line() +
  facet_grid(fare_group~dow) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major.y = element_blank()
  )


# by station - this is for the inset plots per station
hrs <- hr %>% 
  # head(100000) %>%
  mutate(
    transit_timestamp = mdy_hms(transit_timestamp),
    hour = hour(transit_timestamp),
    dow  = wday(transit_timestamp, label = TRUE),
    fare_group = case_when(
      fare_class_category %in% c("OMNY - Seniors & Disability", 
                                 "Metrocard - Seniors & Disability") ~ "Seniors & Disability",
      fare_class_category %in% c("Metrocard - Full Fare", "Metrocard - Unlimited 7-Day", 
                                 "Metrocard - Unlimited 30-Day", "OMNY - Full Fare", 
                                 "Metrocard - Other", "OMNY - Other") ~ "Full Fare",
      fare_class_category %in% c("Metrocard - Students", "OMNY - Students") ~ "Students",
      fare_class_category %in% c("Metrocard - Fair Fare", "OMNY - Fair Fare") ~ "Fair Fare",
      TRUE ~ "Other" # optional to catch anything not covered by the case_when
    )
  ) %>% 
  filter(
    transit_timestamp >= ymd_hms("2023-01-01 00:00:00") & 
    transit_timestamp <= ymd_hms("2023-12-31 24:00:00")
  ) %>% 
  group_by(fare_group, dow, hour, station_complex, station_complex_id) %>% 
  summarize(ridership = mean(ridership)) %>% 
  ungroup()

hrs <- hrs %>% 
  group_by(dow, hour, station_complex, station_complex_id) %>% 
  mutate(total_ridership = sum(ridership), 
         ridership_proportion = ridership / total_ridership) %>% 
  ungroup() 

hrs <- hrs %>%
  mutate(fare_group = factor(
    fare_group,
    levels = c("Seniors & Disability", "Fair Fare", "Full Fare", "Students")
  ))


# hrs %>% write_csv("out/hourly_station_ridership.csv")



# annual summary ----------------------------------------------------------

# this is for the map
hrs_annual <- hr %>% 
  # head(10000000) %>% 
  mutate(
    transit_timestamp = mdy_hms(transit_timestamp),
    hour = hour(transit_timestamp),
    dow  = wday(transit_timestamp, label = TRUE),
    fare_group = case_when(
      fare_class_category %in% c("OMNY - Seniors & Disability", 
                                 "Metrocard - Seniors & Disability") ~ "Seniors & Disability",
      fare_class_category %in% c("Metrocard - Full Fare", "Metrocard - Unlimited 7-Day", 
                                 "Metrocard - Unlimited 30-Day", "OMNY - Full Fare", 
                                 "Metrocard - Other", "OMNY - Other") ~ "Full Fare",
      fare_class_category %in% c("Metrocard - Students", "OMNY - Students") ~ "Students",
      fare_class_category %in% c("Metrocard - Fair Fare", "OMNY - Fair Fare") ~ "Fair Fare",
      TRUE ~ "Other" # optional to catch anything not covered by the case_when
    )
  ) %>% 
  filter(
    transit_timestamp >= ymd_hms("2023-01-01 00:00:00") & 
    transit_timestamp <= ymd_hms("2023-12-31 24:00:00")
  ) %>% 
  group_by(fare_group, station_complex, station_complex_id) %>% 
  summarize(ridership = sum(ridership, na.rm = TRUE)) %>% 
  ungroup()

hrs_annual <- hrs_annual %>% 
  group_by(station_complex, station_complex_id) %>% 
  mutate(total_ridership = sum(ridership), 
         ridership_proportion = ridership / total_ridership) %>% 
  ungroup() 

hrs_annual %>% 
  filter(station_complex %in% unique(hr$station_complex)[1:30]) %>%
  ggplot(aes(ridership, station_complex, fill = fare_group)) +
  geom_col() +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major.y = element_blank()
  )


# join station data to hourly summary -------------------------------------

sta_complex_sf <- hr %>% 
  group_by(station_complex_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269) %>% 
  select(station_complex_id)

# Who takes transit (full fare, seniors/disability, students, fair fare) by station complex
sta_ridership <- hrs_annual %>% 
  left_join(sta_complex_sf) 
  
sta_seniors <- sta_ridership %>% 
  filter(fare_group == "Seniors & Disability") %>%
  st_as_sf() %>% 
  rename(
    seniors_prop_sta = ridership_proportion,
    seniors_ridership_sta = ridership
  )

sta_fair <- sta_ridership %>% 
  filter(fare_group == "Fair Fare") %>%
  st_as_sf() %>% 
  rename(
    low_income_prop_sta = ridership_proportion,
    low_income_ridership_sta = ridership
  )

sta_seniors %>% mapview(zcol = "seniors_prop_sta")
sta_fair %>% mapview(zcol = "low_income_prop_sta")



# ACS ---------------------------------------------------------------------


variables <- c(
  median_age       = "B01002_001", 
  median_income    = "B19013_001",
  total_population = "B17021_001", 
  below_poverty    = "B17021_002"
)

# Get the most recent ACS data with spatial polygons for New York City
# TODO: refine analysis to the ~13k block groups, geography = "block group"
nyc_acs <- get_acs(
  geography = "tract",                   # Get data at the census tract level
  variables = variables,                 # Age and income variables
  state = "NY",                          # State of New York
  county = c("Bronx", "Kings", "New York", "Queens", "Richmond"),  # NYC boroughs
  year = 2022,                           # Most recent ACS year
  survey = "acs5",                       # 5-year ACS survey
  geometry = TRUE                        # Include spatial data (polygons)
)

# Define the breaks and custom labels
breaks <- seq(0, 1, by = 0.2)
labels <- c("0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", "0.8-1")

nyc <- nyc_acs %>% 
  pivot_wider(names_from = variable, values_from = c(estimate, moe)) %>% 
  mutate(
    estimate_below_poverty_prop = estimate_below_poverty/estimate_total_population,
    estimate_below_poverty_prop_bin = cut(estimate_below_poverty_prop, 
                                          breaks = breaks, 
                                          labels = labels, 
                                          include.lowest = TRUE)
  ) 

nyc %>% mapview(zcol = "estimate_median_income")
nyc %>% mapview(zcol = "estimate_median_age")
nyc %>% mapview(zcol = "estimate_total_population")
nyc %>% mapview(zcol = "estimate_below_poverty")
nyc %>% mapview(zcol = "estimate_below_poverty_prop")
nyc %>% mapview(zcol = "estimate_below_poverty_prop_bin")

# add population seniors

# Variables for men and women 65+
variables <- c(
  males_65_to_66 = "B01001_020",
  males_67_to_69 = "B01001_021",
  males_70_to_74 = "B01001_022",
  males_75_to_79 = "B01001_023",
  males_80_to_84 = "B01001_024",
  males_85_and_over = "B01001_025",
  females_65_to_66 = "B01001_044",
  females_67_to_69 = "B01001_045",
  females_70_to_74 = "B01001_046",
  females_75_to_79 = "B01001_047",
  females_80_to_84 = "B01001_048",
  females_85_and_over = "B01001_049"
)

# Get the data for New York City
nyc_seniors_combined <- get_acs(
  geography = "tract",
  variables = variables,
  state = "NY",
  county = c("Bronx", "Kings", "New York", "Queens", "Richmond"),  # NYC boroughs
  year = 2022,
  survey = "acs5"
)

# Summarize the total senior population (65+) by GEOID
nyc_seniors_combined_totals <- nyc_seniors_combined %>%
  group_by(GEOID) %>%
  summarize(seniors_population_acs = sum(estimate, na.rm = TRUE))

# join back to nyc
nyc <- nyc %>% 
  left_join(nyc_seniors_combined_totals) %>% 
  mutate(seniors_prop_acs = seniors_population_acs/estimate_total_population)


# add nearest station to tracts -------------------------------------------

# Find the index of the nearest subway station for each polygon in 'nyc'
nearest_station_index <- st_nearest_feature(nyc, sta_complex_sf)

# Use the index to add the closest subway station information to the 'nyc' polygons
nyc <- nyc %>%
  mutate(station_complex_id = sta_complex_sf$station_complex_id[nearest_station_index])

# add seniors proportion from station data
nyc <- nyc %>% 
  left_join(
    sta_seniors %>% 
      select(-fare_group) %>% 
      st_drop_geometry()
  ) 

# add low income proportion from station data
nyc <- nyc %>% 
  rename(low_income_prop_acs = estimate_below_poverty_prop) %>%
  left_join(
    sta_fair %>% 
      select(-fare_group) %>% 
      st_drop_geometry()
  ) 


# regressions for senior and low income -----------------------------------

nyc %>% 
  ggplot(aes(seniors_prop_acs, seniors_prop_sta)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

nyc %>% 
  ggplot(aes(low_income_prop_acs, low_income_prop_sta)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)


# cleaning for linear model
nyc <- nyc %>% 
  # rm 4 rows where total population is 0, so seniors prop is infinite
  filter(estimate_total_population > 0 & seniors_prop_acs < 0.9) 

nyc <- nyc %>% 
    mutate(
    residual_senior = residuals(lm(seniors_prop_sta ~ seniors_prop_acs, data = nyc)),
    residual_low_income = residuals(lm(low_income_prop_sta ~ low_income_prop_acs, data = nyc)) 
  ) %>% 
  # Get the standard error from the model for residual_senior and residual_low_income
  mutate(
    se_senior = summary(lm(seniors_prop_sta ~ seniors_prop_acs, data = nyc))$sigma,
    se_low_income = summary(lm(low_income_prop_sta ~ low_income_prop_acs, data = nyc))$sigma
  ) %>%
  # Create categories based on residuals using standard error (SE) thresholds
  mutate(
    seniors_category = case_when(
      # residual_senior > 2 * se_senior ~ "Highly Overutilized",
      residual_senior > se_senior ~ "Above Average",
      residual_senior >= -se_senior & residual_senior <= se_senior ~ "Average",  # Utilized condition fixed
      # residual_senior < -2 * se_senior ~ "Highly Underutilized",  # Check for highly underutilized first
      residual_senior < -se_senior ~ "Below Average"
    ),
    low_income_category = case_when(
      # residual_low_income > 2 * se_low_income ~ "Highly Overutilized",
      residual_low_income > se_low_income ~ "Above Average",
      residual_low_income >= -se_low_income & residual_low_income <= se_low_income ~ "Average",  # Utilized condition fixed
      # residual_low_income < -2 * se_low_income ~ "Highly Underutilized",  # Check for highly underutilized first
      residual_low_income < -se_low_income ~ "Below Average"
    )
  )


lvls <- c("Above Average", "Average", "Below Average")
nyc <- nyc %>% 
  # Convert utilization categories to an ordered factor
  mutate(
    seniors_category = factor(seniors_category, 
                              levels = lvls, 
                              ordered = TRUE),
    low_income_category = factor(low_income_category, 
                                 levels = lvls, 
                                 ordered = TRUE)
  )
  
nyc %>% 
  ggplot(aes(low_income_prop_acs, low_income_prop_sta)) +
  geom_point(aes(color = low_income_category)) + 
  geom_smooth(method = "lm", se = FALSE)

nyc %>% 
  ggplot(aes(low_income_prop_acs, low_income_prop_sta)) +
  geom_point(aes(color = residual_low_income)) + 
  scale_color_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  geom_smooth(method = "lm", se = FALSE)

nyc %>% 
  ggplot(aes(seniors_prop_acs, seniors_prop_sta)) +
  geom_point(aes(color = seniors_category)) + 
  geom_smooth(method = "lm", se = FALSE)

nyc %>% 
  ggplot(aes(seniors_prop_acs, seniors_prop_sta)) +
  geom_point(aes(color = residual_senior)) + 
  scale_color_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  geom_smooth(method = "lm", se = FALSE)


# popup plots for final map -----------------------------------------------

sta_mapgl <- hr %>% 
  group_by(station_complex_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269) %>% 
  select(station_complex_id, station_complex)

bnyc <- st_read("data/Borough Boundaries.geojson") %>% 
  st_transform(crs = 4269) %>% 
  rmapshaper::ms_simplify(keep_shapes = TRUE)

crs <- "+proj=omerc +lonc=90 +lat_0=40 +gamma=143 +alpha=57"

plots <- vector("list", nrow(sta_mapgl))
for(i in seq_along(sta_mapgl$station_complex)){
  station = sta_mapgl$station_complex[i]
  p1 = hrs %>% 
    filter(station_complex == station) %>%
    mutate(dow_upscale = ifelse(dow %in% c("Sat","Sun"),"weekend","weekday")) %>% 
    ggplot(aes(hour, ridership, group = dow, color = dow_upscale)) +
    geom_line(alpha = 0.7, key_glyph = "timeseries") +
    scale_color_manual(values = c("#984ea3", "#377eb8")) +
    scale_x_continuous(
      limits = c(0, 25),
      breaks = seq(0, 24, by = 6),  # Breaks every 6 hours
      labels = sprintf("%02d:00", seq(0, 24, by = 6))  # Format labels as "HH:00"
    ) +
    facet_wrap(~fare_group, ncol = 1, scales = "free") +
    labs(
      title = " ",
      subtitle = " ",
      y = NULL,
      x = NULL,
      color = ""
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(), 
      legend.position = "bottom"
    )
  
  p2 = ggplot() + 
    geom_sf(data = bnyc, fill = "grey90") + 
    geom_sf(data = sta_mapgl, color = "#4daf4a", cex = 0.8, alpha = 0.5, pch =20) +
    geom_sf(data = filter(sta_mapgl, station_complex == station), 
            color = "#e41a1c", cex = 2, alpha = 0.8) +
    coord_sf(crs = crs) +
    labs(
      title = glue("{station} Average hourly ridership"),
      subtitle = "Data from 2023-01-01 to 2023-12-31"
    ) +
    theme_void()
  
  plots[[i]] = p2 + p1 + plot_layout(widths = c(1, 2))
  
}

pdf("out/station_hourly_avg_per_rider.pdf", height = 6, width = 12)
plots
dev.off()



# final map ---------------------------------------------------------------

cat_key <- c("Above Average" = "🔵", "Average" = "⚪", "Below Average" = "🔴")

nyc <- nyc %>% 
  mutate(
    NAME = str_replace(NAME, "; New York$", ""),
    tooltip_senior = glue(
      "<b>{NAME}</b><br>
      Closest Ⓜ️ station: <b>{station_complex}</b><br><hr>
      {cat_key[seniors_category]} Relative to all other tracts, 🧓🏼 senior ridership is <b>{seniors_category}</b>.<br><br>
      There are <b>{seniors_population_acs} seniors</b> in this tract<br>
      (<b>{round(seniors_prop_acs*100, 0)}%</b> of tract population).<br><br>
      Seniors make up <b>{round(seniors_prop_sta*100, 0)}%</b> of daily riders at
      the closest station, <b>{station_complex}</b>."
    ),
    tooltip_low_income = glue(
      "<b>{NAME}</b><br>
      Closest Ⓜ️ station: <b>{station_complex}</b><br><hr>
      {cat_key[low_income_category]} Relative to all other tracts, 🎟 fair fare ridership is <b>{low_income_category}</b>.<br><br>
      There are <b>{estimate_below_poverty} people below poverty</b> in this tract<br>
      (<b>{round(low_income_prop_acs*100, 0)}%</b> of tract population).<br><br>
      People below poverty make up <b>{round(low_income_prop_sta*100, 0)}%</b> of daily riders at
      the closest station, <b>{station_complex}</b>."
    ),
    tooltip_unified = glue(
      "<b><i>{NAME}</i></b><br>
      Closest Ⓜ️ station: <b>{station_complex}</b><br><hr>
      {cat_key[seniors_category]} Relative to all other tracts, 🧓🏼 senior ridership is <b>{seniors_category}</b>.<br><br>
      There are <b>{seniors_population_acs} seniors</b> in this tract<br>
      (<b>{round(seniors_prop_acs*100, 0)}%</b> of tract population).<br><br>
      Seniors make up <b>{round(seniors_prop_sta*100, 0)}%</b> of daily riders at
      the closest station.<br><hr>
      {cat_key[low_income_category]} Relative to all other tracts, 🎟️ fair fare ridership is <b>{low_income_category}</b>.<br><br>
      There are <b>{estimate_below_poverty} people below poverty</b> in this tract
      (<b>{round(low_income_prop_acs*100, 0)}%</b> of tract population).<br><br>
      People below poverty make up <b>{round(low_income_prop_sta*100, 0)}%</b> of daily riders at
      the closest station."
    )
  )

# write data for dashboard
write_rds(sta_mapgl, "out/sta_mapgl.rds")
write_rds(nyc, "out/nyc.rds")

