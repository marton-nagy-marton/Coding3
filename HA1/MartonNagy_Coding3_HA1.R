# Author: Márton Nagy
# Course: Coding 3

# Note: Charts have been designed to be readable on full screen view.
# Answers are provided in comments if the task was not a visualization.

library(data.table)
library(nycflights13)
dt <- data.table(flights)

# 1. How many flights originated from JFK?
dt[origin == 'JFK', .(from_JFK_count = .N)]
# Answer: 111,279

# 2. Count the number of flights per month.
dt[, .(count_flights = .N), by = month][order(month)]
# Answer:
# month count_flights
# <int>         <int>
# 1:     1         27004
# 2:     2         24951
# 3:     3         28834
# 4:     4         28330
# 5:     5         28796
# 6:     6         28243
# 7:     7         29425
# 8:     8         29327
# 9:     9         27574
# 10:    10         28889
# 11:    11         27268
# 12:    12         28135

# 3. Visualize the number of flights per destination.
library(ggplot2)
   
ggplot(dt[, .(count_flights = .N), by = dest], aes(x = count_flights, y = reorder(dest, count_flights))) +
  geom_bar(stat = 'identity', fill = 'navy') +
  theme_bw() +
  ylab('Destination') +
  xlab('Number of flights') +
  ggtitle('Number of flights per destination') +
  theme(text = element_text(size = 8))

# 4. Count the number of flights with an arrival delay of more than 100 mins.
dt[arr_delay > 100, .(hundredminplus_delay_count = .N)]
# Answer: 13,887

# 5. Visualize the maximum arrival delay per destination.
ggplot(dt[, .(max_delay = max(arr_delay, na.rm = TRUE)), by = dest][!is.na(max_delay)],
       aes(x = max_delay, y = reorder(dest, max_delay))) +
  geom_bar(stat = 'identity', fill = 'navy') +
  theme_bw() +
  ylab('Destination') +
  xlab('Maximum delay (min)') +
  ggtitle('Maximum delay per destination', subtitle = '(missing values removed)') +
  theme(text = element_text(size = 8))

# 6. Aggregate the min and max arrival delay per origin.
dt[, .(min_delay = min(arr_delay, na.rm = TRUE), max_delay = max(arr_delay, na.rm = TRUE)), by = origin]
# Answer:
# origin min_delay max_delay
# <char>     <num>     <num>
# 1:    EWR       -86      1109
# 2:    LGA       -68       915
# 3:    JFK       -79      1272

# 7. Visualize the distribution of the arrival delay per origin.
ggplot(dt, aes(x = arr_delay)) +
  geom_density(fill = 'navy', alpha = 0.5) +
  facet_wrap(~origin) +
  theme_bw() +
  ylab('Density') +
  xlab('Arrival delay (min)') +
  ggtitle('Distribution of arrival delays by origin', subtitle = '(missing values removed)')

# 8. Visualize the distribution of the arrival delay per destination.
ggplot(dt, aes(x = arr_delay)) +
  geom_density(fill = 'navy', alpha = 0.5) +
  facet_wrap(~dest) +
  theme_bw() +
  ylab('Density') +
  xlab('Arrival delay (min)') +
  ggtitle('Distribution of arrival delays by destination', subtitle = '(missing values removed)')

# 9. List the top 5 destinations being the furthest from NYC!
tail(unique(dt[origin == 'JFK', .(dest, distance)])[order(distance)],5)
# Answer:
# dest distance
# <char>    <num>
# 1:    SMF     2521
# 2:    SJC     2569
# 3:    OAK     2576
# 4:    SFO     2586
# 5:    HNL     4983

# 10. How many flights were scheduled to departure before 11 am?
dt[sched_dep_time %/% 100 < 11, .(before11am_dept = .N)]
# Answer: 114,988
# Note: Using the 'hour' column instead of extracting the hour by integer division
# from the 'sched_dep_time' would yield the same results.
