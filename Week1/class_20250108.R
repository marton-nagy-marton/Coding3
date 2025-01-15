1 + 3

2 ^3

2 ** 3

45**2

(20 + 25) ** 2

((2+0+2+5) * (2-0-2-5))**2

sum((1:9)^3) #sum up the cubes of numbers 1-9

sum(1:9)^2

as.Date("2025-01-01")

str(as.Date("2025-01-01")) #structure of an object

str(1:9)

str(42)

as.Date("2025-01-01"):as.Date("2025-01-03")

str(as.Date("2025-01-01"):as.Date("2025-01-03"))

seq(as.Date("2025-01-01"), as.Date("2025-01-03"), by = '1 day')

# assigning a value to a variable
dates <- seq(as.Date("2025-01-01"), as.Date("2025-01-03"), by = '1 day')

#first chars of the day names
substr(weekdays(dates), 1, 1)

# !!! INDEXING STARTS WITH 1 !!!
dates[1]

# -------------------------
pi
pi*2

letters

LETTERS

letters[1:5]

letters[seq(1, 10, by = 2)]

x <- 2

#n random numbers from uniform distribution
hist(runif(10000))

?runif

# Exercise: draw the sine wave between 0 and pi*2

sine_wave <- sin(seq(0, pi * 2, by = 0.01))

plot(x = seq(0, pi * 2, by = 0.01),
     y = sine_wave,
     type = 'l',
     main = 'sin(x), xϵ[0,2π]',
     xlab = 'x',
     ylab = 'sin(x)'
     )

?plot

?curve

curve(sin, to = 2 * pi)
curve(cos, add = TRUE, col = 'red')

# Brownian motion / random walk
x <- 0
set.seed(42)
for (i in 1:100) {
  if (runif(1) < 0.5) {
    x <- x - 1
  } else {
    x <- x + 1
  }
}
x

set.seed(42)
plot(1:1000, cumsum((runif(1000) < 0.5) * 2 - 1), type ='s')

set.seed(42)
x <- sum((runif(100) < 0.5) * 2 - 1)
for (i in 1:1000) {
  x <- c(x, sum((runif(100) < 0.5) * 2 - 1))
}
hist(x)

x <- round(runif(1e5)) * 2 - 1
m <- matrix(x, nrow = 100)
hist(apply(m, 1, sum))

#-------------
#ANALYTICS

h <- c(174, 170, 160)
w <- c(90, 80, 70)

plot(h, w, main = 'Height vs. weight', ylab = 'Weight', xlab = 'Height')

min(h)
max(h)
range(h)
diff(range(h))
mean(h)
median(h)
summary(h)

cor(w,h)
lm(w ~ h)

fit <- lm(w ~ h)

fit

predict(fit, newdata = list(h = 172))

predict(fit, newdata = list(h = 52))

# BMI

b <- w / (h/100) ** 2

df <- data.frame(height = h, weight = w)

df[1, 2] # first row, second column

df$weight[1]

df$bmi <- b

df <- read.csv('http://bit.ly/CEU-R-heights')

df$bmi <- (df$weightLb * 0.45) / ((df$heightIn * 0.0254) ** 2)

df$height <- df$heightIn * 0.0254
df$weight <- df$weightLb *0.45

df$weightLb <- NULL
df$heightIn <- NULL

plot(df)

summary(df)

#pairsD3 package
library(pairsD3)
pairsD3(df)

library('GGally') #quotes are optional
ggpairs(df)

library(ggplot2)
ggplot(df, aes(x = height)) +
  geom_histogram(color = 'red', fill = 'orange') +
  theme_bw()

p <- ggplot(df, aes(x = height))
p + geom_histogram()
p + geom_density() + theme_bw()

p <- p + geom_histogram()

p + theme_bw() + scale_y_log10()

ggplot(df, aes(x = sex)) + geom_bar()

ggplot(df, aes(x = height, y = weight)) +
  geom_point(aes(color = sex)) +
  geom_smooth(method = 'lm', se = FALSE, col = 'black') +
  theme_bw()

ggplot(df, aes(x = height, y = weight, color = sex)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  theme_bw()

ggplot(df, aes(x = height, y = weight)) +
  geom_point(aes(color = sex)) +
  geom_smooth(method = 'lm', se = FALSE, col = 'black') +
  geom_smooth(aes(color = sex), method = 'lm', se = FALSE) +
  theme_bw()

#boxplots of heights by sex
ggplot(df, aes(x = sex, y = height)) +
  geom_boxplot() +
  theme_bw()

ggplot(df, aes(y = height)) +
  geom_boxplot() +
  facet_wrap(~sex)

ggplot(df, aes(x = height, y = weight)) +
  geom_point() +
  geom_smooth(method = 'lm') + 
  facet_wrap(~sex)

ggplot(df, aes(x = sex, y = height)) +
  geom_violin() +
  geom_boxplot() +
  geom_jitter() +
  theme_bw()

ggplot(df, aes(x = height, fill = sex)) +
  geom_density(alpha = 0.25) +
  theme_bw() +
  ggtitle('Height of males and females') +
  theme(legend.position = 'top') +
  xlab('Height (m)') +
  ylab('Density')

#exercise: plot the number of boys and girls above/below 160 cm

df$below_160 <- NULL
df$height_cat <- df$height < 1.6
df$height_cat <- cut(df$height, 
                     breaks = c(0, 1.6, Inf),
                     labels = c("short", "tall"))

ggplot(df, aes(x = sex, fill = height_cat)) +
  geom_bar() +
  theme_bw()

#mean of height by gender
mean(df$height[df$sex == 'f'])
mean(df$height[df$sex == 'm'])

aggregate(height ~ sex, FUN = mean, data = df)

library(data.table)
dt <- data.table(df)

dt[1] #we are referencing only the row index
# this was not possible with data frames

dt[sex == 'f']

dt[ageYear == min(ageYear)][order(bmi)]

dt[sex == 'f', hist(height)] #filter and evaluate an expression

dt[, mean(height), by = sex]

dt[, mean(height), by = sex][sex == 'f']


# count males and females above/below 1.6m
dt[, .N, by = .(height_cat, sex)]
dt[, .(count = .N), by = .(gender = sex, height_cat)]
dt[, .(count = .N), by = .(gender = sex, hc = height < 1.6)]

dt[height < 1.6, length(height), by = sex]
dt[height > 1.6, length(height), by = sex]

dt[height < 1.6, list(count = .N), by = sex] #list in R is like a dict in py
dt[height > 1.6, list(count = .N), by = sex]

dt[height < 1.6, .(count = .N), by = .(gender = sex)] #list can be abbr with dot
dt[height > 1.6, .(count = .N), by = .(gender = sex)]

# dt[i,j,by]
# i : filtering
# j : operations
# by : grouping

#exercise: count the number of people below/above 12 years
dt[, .(count = .N), by = .(agegroup = ageYear < 12)]

dt[, .(count = .N), 
   by = .(agegroup = cut(ageYear, breaks = c(0, 12, Inf), labels = c('young', 'old')))]

#avg weight of high bmi (above 25) people
dt[bmi > 25, mean(weight)]

#categorize people by bmi to underweight normal and overweight
dt$bmi_cat <- cut(dt$bmi, 
                     breaks = c(0, 18.5, 25, Inf),
                     labels = c('underweight', 'normal', 'overweight'))

dt[, bmi_cat := cut(dt$bmi, 
                    breaks = c(0, 18.5, 25, Inf),
                    labels = c('underweight', 'normal', 'overweight'))]

#stacked bar chart for bmi cat split by gender
ggplot(dt, aes(x = sex, fill = bmi_cat)) +
  geom_bar() +
  theme_bw()

dtagg <- dt[, .N, by = .(sex, bmi_cat)]

ggplot(dtagg, aes(x = sex, fill = bmi_cat, y = N)) +
  geom_bar(stat = 'identity') +
  theme_bw()

ggplot(dtagg, aes(x = sex, fill = bmi_cat, y = N)) +
  geom_col() +
  theme_bw()
