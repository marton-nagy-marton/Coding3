## DATA LOADING
library(tidytuesdayR)
raw_data <- tidytuesdayR::tt_load('2023-06-20')

library(data.table)
ufo_sightings <- data.table(raw_data$`ufo_sightings`)
places <- data.table(raw_data$`places`)
day_parts_map <- data.table(raw_data$`day_parts_map`)

rm(raw_data)

## PART 1: DATA TABLE EXERCISES

## 1. How many UFO sightings were there in total?
ufo_sightings[, .N]

## 2. Number of sightings per country, top 10
ufo_sightings[, .N, by = country_code][order(-N)][1:10]

## 3. Avg. duration of sightings during the night
ufo_sightings[day_part == 'night', .(avg_duration_sec = mean(duration_seconds))]

## 4. Avg. duration and number of sightings in the US, by states
ufo_sightings[country_code == 'US', .(avg_duration_sec = round(mean(duration_seconds)),
                                      n_obs = .N), 
              by = state][order(-avg_duration_sec)]

## 5. Most seen shape in Canada per state
ufo_sightings[country_code == 'CA' & !is.na(shape) & shape != 'unknown',
              .N, by = .(state, shape)][, .SD[which.max(N)], by = state][order(-N)]

## 6. Avg. duration and number of sightings in Europe by year and month (from 2020)
(
  merge(ufo_sightings, places, by = c('city', 'state', 'country_code'))
  [year(reported_date_time) > 2019 & grepl('Europe',timezone),
    .(avg_duration_sec = round(mean(duration_seconds)), n_obs = .N),
    by = .(year(reported_date_time), month(reported_date_time))][order(-year, -month)]
)

## 7. US cities with the highest number of sightings per capita (and total sightings)
ufo_sightings[, city_upper := toupper(city)]
unique(
  merge(ufo_sightings, places, by = c('city', 'state', 'country_code'))
  [country_code == 'US' & population != 0, .(sightings_pc = .N / population, .N), by = .(state, city_upper)][order(-sightings_pc)]
)[1:10]

## 8. Mean duration of sightings conditional on hour of the day
ufo_sightings[, .(avg_duration_sec = mean(duration_seconds)), by = hour(reported_date_time)][order(hour)]

## 9. Sightings (count and pctg of country total) by day part in the US and Canada
(
  ufo_sightings[country_code %in% c('US', 'CA') & !is.na(day_part),
              .N, by = .(country_code, day_part)]
  [, pctg := round(100 * N / sum(N), 2), by = country_code]
  [order(day_part, -country_code)]
)

## 10. Number and average duration of sightings by elevation category
places[, elevation_cat := cut(elevation_m,
                              breaks = c(-100, 200, 500, 1500, Inf),
                              labels = c('plains', 'hill', 'mountain', 'high mountain'))]
(
  merge(ufo_sightings, places, by = c('city', 'state', 'country_code'))
  [!is.na(elevation_cat), .(n_obs = .N, avg_duration_sec = round(mean(duration_seconds))), by = elevation_cat]
)


## PART 2: DATA VISUALIZATIONS
library(ggplot2)

# 1. Number of sightings by states in the US
library(maps)
library(dplyr)

# Look-up table to match state names and codes
state_lookup <- data.frame(
  state_code = state.abb,
  state_name = tolower(state.name)
)

# Aggregate sightings by state and map to full names
state_sightings <- ufo_sightings[country_code == 'US', .N, by = state]
state_sightings <- state_sightings %>% left_join(state_lookup, by = c("state" = "state_code"))

# US states map data (uses full state names)
us_states <- map_data("state")

# Merge UFO data with map data
map_data <- us_states %>% left_join(state_sightings, by = c("region" = "state_name"))

# Plotting
ggplot(map_data, aes(long, lat, group = group, fill = N)) +
  geom_polygon(color = "black", size = 0.2) +
  scale_fill_distiller(name = 'No. of sightings', type = 'seq', palette = 'RdPu', direction = 1, na.value = 'grey90') +
  labs(title = "No. of UFO sightings by state in the US") +
  theme_void()

rm(us_states, map_data, state_lookup, state_sightings)

# 2. Distribution of sighting duration by elevation category
ggplot(merge(ufo_sightings, places, by = c('city', 'state', 'country_code'))[!is.na(duration_seconds) & !is.na(elevation_cat)],
       aes(x = duration_seconds, color = elevation_cat, fill = elevation_cat)) +
  geom_density(alpha = 0.25) +
  scale_x_log10() +
  theme_classic() +
  scale_color_brewer(name = 'Elevation category', type = 'seq', palette = 'BuPu', aesthetics = c('color', 'fill')) +
  xlab('Duration in seconds (logarithmic scale)') +
  ylab('Density')+
  ggtitle('Distribution of sighting duration by elevation category') +
  theme(legend.position = 'top', plot.title = element_text(hjust = 0.5))

# 3. Distribution of sighting duration in the top 10 countries by number of sightings
ggplot(merge(ufo_sightings, ufo_sightings[, .N, by = country_code][order(-N)][1:10]),
       aes(y = duration_seconds, x = reorder(country_code, -N))) +
  geom_violin(fill = "skyblue", alpha = 0.5) +
  geom_boxplot(width = 0.1, fill = "blue", color = "black", outlier.size = 0.5, alpha = 0.7) +
  scale_y_log10() +
  theme_light() +
  ylab('Duration in seconds (logarithmic scale)') +
  xlab('Country (ordered by no. of sightings)') +
  ggtitle('Distribution of sighting duration by country', subtitle = 'Only TOP10 countries by no. of sightings shown.') +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# 4. Avg. sighting duration and number of sightings by month
monthly_stats <- ufo_sightings[,.(avg_duration = mean(duration_seconds, na.rm = TRUE), sightings = .N),
                               by = .(year = year(reported_date_time), month = month(reported_date_time))][order(year, month)][year > 2012]
monthly_stats[, year_month := sprintf("%d-%02d", year, month)]

ggplot(monthly_stats, aes(x = year_month)) +
  geom_bar(aes(y = sightings), stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_line(aes(y = avg_duration / 60), 
            color = "navy", size = 1.2, group = 1) +
  scale_y_continuous(name = "Number of sightings",
                     sec.axis = sec_axis(~ ., name = "Avg. duration in minutes",)) +
  scale_x_discrete(breaks = monthly_stats$year_month[seq(1, nrow(monthly_stats), by = 6)]) +
  labs(title = "No. of sightings and average sighting duration per year and month",
       subtitle = '(2013-2023)',
       x = "Year and month",
       y = "Number of sightings") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y.right = element_text(color = "navy"),
        axis.text.x = element_text(angle = 45, hjust = 1))
      
rm(monthly_stats)

# 5. Hourly distribution of no. of sightings, by month
ufo_sightings[, hour := hour(reported_date_time)]
ufo_sightings[, month := month(reported_date_time)]

ggplot(ufo_sightings[, .N, by = .(month, hour)], aes(x = hour, y = N)) +
  geom_col(color = 'skyblue', fill = 'skyblue') +
  facet_wrap(~month) +
  theme_minimal() +
  labs(title = 'Hourly distribution of no. of sightings by month',
       x = 'Hour of the day',
       y = 'No. of sightings') +
  theme(plot.title = element_text(hjust = 0.5))

# 6. Marking places in the world where there was a UFO sighting
world <- map_data('world')
ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region), color = 'black', fill = 'lightgrey') +
  geom_point(data = merge(ufo_sightings, places, by = c('city', 'state', 'country_code'))[, .N, by = .(latitude, longitude)],
             aes(y = latitude, x = longitude, fill = N, color = N), alpha = 0.2) +
  theme_void() +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_distiller(name = 'No. of sightings', type = 'seq', palette = 'Reds', direction = 1, aesthetics = c('color', 'fill')) +
  ggtitle('Geographical distribution of UFO sightings')

rm(world)

# 7. Population vs. no. of sightings
ggplot(merge(ufo_sightings, places, by = c('city', 'state', 'country_code'))[, .(pop = mean(population), .N), by = .(latitude, longitude)],
       aes(x = pop, y = N)) +
  geom_point(color = 'grey', alpha = 0.1, size = 1) +
  geom_smooth(method = 'lm') +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = 'Log-log regression of number of sightings on population',
       y = 'Number of sightings (logarithmic scale)',
       x = 'Population (logarithmic scale)') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 8. Elevation vs. number of sightings in the US
ggplot(merge(ufo_sightings, places, by = c('city', 'state', 'country_code'))[country_code == 'US', .(elev = mean(elevation_m), .N), by = .(country_code, latitude, longitude)],
       aes(x = elev, y = N)) +
  geom_point(color = 'grey', alpha = 0.1, size = 1) +
  geom_smooth() +
  scale_y_log10() +
  labs(title = 'Loess regression of number of sightings on elevation in the US',
       y = 'Number of sightings (logarithmic scale)',
       x = 'Elevation (meter)') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

  