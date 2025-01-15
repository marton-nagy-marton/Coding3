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

## TODO count the number of bookings below 100 EUR without an offer

bookings[price < 100 & offer == 0, .N]

## TODO compute the average price of the bookings below 100 EUR

bookings[price < 100, .(avg_price = mean(price))]

## TODO compute the average price of bookings on weekends

bookings[weekend == 1, .(avg_price = mean(price))]

## TODO compute the average price of bookings on weekdays

bookings[weekend == 0, .(avg_price = mean(price))]

## TODO include nnights, holiday and year as well in the aggregate variables

bookings[, .(avg_price = mean(price), n_obs = .N), by = .(weekend, nnights, holiday, year)]

## TODO avg price per number of stars

?merge

merge(x = bookings, y = features, by = 'hotel_id')[, .(avg_price = mean(price)), by = stars][order(stars)]

## another syntax x[y, rolling = ...]

## TODO why inner join excludes 3 rows
bookings[is.na(hotel_id)]

bookings[!(hotel_id %in% features$hotel_id)]      

features[hotel_id == 2]

(merge(x = bookings, y = features, by = 'hotel_id')
[, .(avg_price = mean(price/nnights), n_obs = .N), by = stars]
[order(stars)])

library(ggplot2)
ggplot(merge(x = bookings, y = features, by = 'hotel_id')[stars == 2.5], aes(price)) +
  geom_boxplot()

merge(bookings, features)[stars == 2.5][, mean(price), by = nnights]

dt <- merge(bookings, features)
dt$price_per_night <- dt$price / dt$nnights
# or another way
dt[, price_per_night := price/nnights]

dt[, .(avg_ppn = mean(price_per_night)), by = stars][order(stars)]

dt[, .(avg_ppn = weighted.mean(price_per_night, nnights)), by = stars][order(stars)]                                       

## TODO hotels dataset: features + avg. price per night

hotels <- merge(features, bookings[, .(price_per_night = mean(price / nnights), bookings = .N), by = hotel_id])
hotels[, .(avg_ppn = weighted.mean(price_per_night, bookings)), by = stars][order(stars)]

## TODO viz on avg price per night per stars

(
  ggplot(hotels[!is.na(stars), .(avg_ppn = weighted.mean(price_per_night, bookings)), by = stars][order(stars)],
       aes(x = factor(stars), y = avg_ppn)) +
    geom_col() +
    theme_bw() +
    xlab('Number of stars') +
    ylab('Average price per night')
)
## TODO viz on avg price per night per stars split by country
(
  ggplot(hotels[!is.na(stars), .(avg_ppn = weighted.mean(price_per_night, bookings)), by = .(stars, country)][order(stars)],
         aes(x = factor(stars), y = avg_ppn)) +
    geom_col() +
    theme_bw() +
    xlab('Number of stars') +
    ylab('Average price per night') +
    facet_wrap(~country, scales = 'free_y')
)

?facet_wrap

## aggregated dataset by country: avg price, ratings, stars

dt_country <- hotels[, .(avg_ppn = weighted.mean(price_per_night, bookings), avg_rating = mean(rating, na.rm = TRUE), avg_stars = mean(stars, na.rm = TRUE)), by = country]

## countries with above avg ratings

hotels[, mean(rating, na.rm = TRUE)]

dt_country[avg_rating > hotels[, mean(rating, na.rm = TRUE)], country]

hotels[, pricecat := cut(price_per_night, 3)]
hotels[, .N, by = pricecat]

hotels[, pricecat := cut(price_per_night, c(0, 100, 250, Inf), labels = c('cheap', 'avg', 'expensive'))]
hotels[, .N, by = pricecat]

hotels[, pricecat := cut(price_per_night,
                         c(0, quantile(price_per_night, 1/3), quantile(price_per_night, 2/3), Inf),
                         labels = c('cheap', 'avg', 'expensive'))]
hotels[, .N, by = pricecat]

# assign cutpoints by within country quantiles
hotels[, lower := quantile(price_per_night, 1/3), by = country]
hotels[, upper := quantile(price_per_night, 2/3), by = country]

#does not run because some countries has same lowe and upper bounds
hotels[, pricecat := cut(price_per_night,
                         c(0, lower[1], upper[1], Inf),
                         labels = c('cheap', 'avg', 'expensive')),
       by = country]

hotels[, .(0, lower[1], upper[1], Inf), by = country]

hotels$pricecat <- NULL
hotels[lower != upper, pricecat := cut(price_per_night,
                         c(0, lower[1], upper[1], Inf),
                         labels = c('cheap', 'avg', 'expensive')),
       by = country]

# flag of japan modelling exercise

points <- data.table(x = rep(1:100, 100), y = rep(1:100, each = 100), color = 'white')

points[(x - 50)^2 + (y - 50)^2 <= 50, color := 'red']

ggplot(points, aes(x, y, color = color)) +
  geom_point() +
  theme_void() +
  scale_color_manual(values = c('red', 'white')) +
  theme(legend.position = 'none')

points[, is_red := as.integer(color == 'red')]

points[, color := factor(color)]
fit <- glm(color ~ x + y, points, family = binomial(link = logit))
summary(fit)
predict(fit, type = 'response')

points$pred <- predict(fit, type = 'response')

ggplot(points, aes(x, y, color = factor(round(pred)))) +
  geom_point() +
  theme_void() +
  scale_color_manual(values = c('red', 'white')) +
  theme(legend.position = 'none')

library(rpart)
fit <- rpart(color ~ x + y, points)
fit

plot(fit)
text(fit)

predict(fit, type = 'class')
points$pred <- predict(fit, type = 'class')

ggplot(points, aes(x, y, color = pred)) +
  geom_point() +
  theme_void() +
  scale_color_manual(values = c('red', 'white')) +
  theme(legend.position = 'none')

ggplot(points, aes(x, y)) +
  geom_tile(aes(fill = pred), alpha = 0.5) +
  geom_tile(aes(fill = color), alpha = 0.5) +
  theme_void() +
  scale_fill_manual(values = c('red', 'white')) +
  theme(legend.position = 'none')

library(partykit)
plot(as.party(fit))

fit <- rpart(color ~ x + y, points, control = rpart.control(minsplit = 1, cp = 0))

?rpart

plot(as.party(fit))

points$pred <- predict(fit, type = 'class')
ggplot(points, aes(x, y)) +
  geom_tile(aes(fill = pred), alpha = 0.5) +
  geom_tile(aes(fill = color), alpha = 0.5) +
  theme_void() +
  scale_fill_manual(values = c('red', 'white')) +
  theme(legend.position = 'none')

library(randomForest)

fit <- randomForest(color ~ x + y, points)

fit

points$pred <- predict(fit, type = 'class')
ggplot(points, aes(x, y)) +
  geom_tile(aes(fill = pred), alpha = 0.5) +
  geom_tile(aes(fill = color), alpha = 0.5) +
  theme_void() +
  scale_fill_manual(values = c('red', 'white')) +
  theme(legend.position = 'none')
