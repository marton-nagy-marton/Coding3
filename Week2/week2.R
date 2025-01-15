# cleaning up the environment
rm(list = ls())
source("http://bit.ly/CEU-R-heights-2018")

# Mean of heights and some viz
mean(heights, na.rm = TRUE)

library(ggplot2)
hist(heights)
ggplot(data.frame(heights), aes(heights)) + geom_histogram()

# by default, ls() only lists visible variables
rm(list = ls())
?ls
ls(all.names = TRUE)
.secret

library(data.table)
?fread
# fread is much faster than read.csv

bookings <- fread('http://bit.ly/CEU-R-hotels-2018-prices')
features <- fread('http://bit.ly/CEU-R-hotels-2018-features')
