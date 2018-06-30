# set working directory to git repository
# change parameters if needed
home <- Sys.getenv("USERPROFILE")
repo <- "/repos/cafe-statistics"
setwd(paste(home, repo, sep=""))
getwd()

# make sure you have readxl installed and loaded
install.packages("readxl")
library("readxl")

## LOADING
# load dataset with correct attribute types and NA value parsing
cafe.data <- read_excel("cafedata.xls", na = "na")
View(cafe.data)

## DESCRIPTIVE STATISTICS
# descriptive statistics for total coffee and soda sales || day-independent
sodas <- cafe.data$Sodas
coffees <- cafe.data$Coffees
col.names <- c('Variable', 'N', 'NA', 'Mean', 'SE Mean', 'StDev', 'Minimum', 'Q1', 'Median', 'Q3', 'Maximum')
vars <- c('Sodas', 'Coffees')
n <- c(length(na.omit(sodas)), length(na.omit(coffees)))
na <- c(sum(is.na(sodas)), sum(is.na(coffees)))
mean <- c(mean(sodas, na.rm=TRUE), mean(coffees, na.rm=TRUE))
sd <- c(sd(cafe.data$Sodas, na.rm=TRUE), sd(cafe.data$Coffees, na.rm=TRUE))
semean <- c(sd[1] / sqrt(length(sodas[!is.na(sodas)])), 
            sd[2] / sqrt(length(coffees[!is.na(coffees)])))
min <- c(min(sodas, na.rm=TRUE), min(coffees, na.rm=TRUE))
max <- c(max(sodas, na.rm=TRUE), max(coffees, na.rm=TRUE))
quantiles.sodas <- quantile(sodas, na.rm=TRUE)
quantiles.coffees <- quantile(coffees, na.rm=TRUE)
q1 <- c(quantiles.sodas[2], quantiles.coffees[2])
median <- c(quantiles.sodas[3], quantiles.coffees[3])
q3 <- c(quantiles.sodas[4], quantiles.coffees[4])
desc.stats <- data.frame(vars, n, na, mean, sd, semean, min, q1, median, q3, max)
names(desc.stats) <- col.names
View(desc.stats)

# descriptive statistics for total soda sales || day-dependent
days.ordered <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri')
sodas.n <- c(length(na.omit(cafe.data[cafe.data$`Day Code` == 1, ]$Sodas)),
             length(na.omit(cafe.data[cafe.data$`Day Code` == 2, ]$Sodas)),
             length(na.omit(cafe.data[cafe.data$`Day Code` == 3, ]$Sodas)),
             length(na.omit(cafe.data[cafe.data$`Day Code` == 4, ]$Sodas)),
             length(na.omit(cafe.data[cafe.data$`Day Code` == 5, ]$Sodas)))
names(sodas.n) <- days.ordered
sodas.na <- c(sum(is.na(cafe.data[cafe.data$`Day Code` == 1, ]$Sodas)),
              sum(is.na(cafe.data[cafe.data$`Day Code` == 2, ]$Sodas)),
              sum(is.na(cafe.data[cafe.data$`Day Code` == 3, ]$Sodas)),
              sum(is.na(cafe.data[cafe.data$`Day Code` == 4, ]$Sodas)),
              sum(is.na(cafe.data[cafe.data$`Day Code` == 5, ]$Sodas)))
names(sodas.na) <- days.ordered
sodas.mean <- tapply(cafe.data$Sodas, cafe.data$`Day Code`, mean, na.rm=TRUE)
names(sodas.mean) <- days.ordered
sodas.stdev <- tapply(cafe.data$Sodas, cafe.data$`Day Code`, sd, na.rm=TRUE)
names(sodas.stdev) <- days.ordered
tmp.lengths <- c(length(na.omit(cafe.data[cafe.data$`Day Code` == 1, ]$Sodas)),
                 length(na.omit(cafe.data[cafe.data$`Day Code` == 2, ]$Sodas)),
                 length(na.omit(cafe.data[cafe.data$`Day Code` == 3, ]$Sodas)),
                 length(na.omit(cafe.data[cafe.data$`Day Code` == 4, ]$Sodas)),
                 length(na.omit(cafe.data[cafe.data$`Day Code` == 5, ]$Sodas)))
sodas.semean <- sodas.stdev / sqrt(tmp.lengths)
names(sodas.semean) <- days.ordered
sodas.min <- tapply(cafe.data$Sodas, cafe.data$`Day Code`, min, na.rm=TRUE)
names(sodas.min) <- days.ordered
sodas.quantiles <- tapply(cafe.data$Sodas, cafe.data$`Day Code`, quantile, na.rm=TRUE)
sodas.q1 <- c(sodas.quantiles$`1`[2],
              sodas.quantiles$`2`[2],
              sodas.quantiles$`3`[2],
              sodas.quantiles$`4`[2],
              sodas.quantiles$`5`[2])
names(sodas.q1) <- days.ordered
sodas.median <- c(sodas.quantiles$`1`[3],
              sodas.quantiles$`2`[3],
              sodas.quantiles$`3`[3],
              sodas.quantiles$`4`[3],
              sodas.quantiles$`5`[3])
names(sodas.median) <- days.ordered
sodas.q3 <- c(sodas.quantiles$`1`[4],
              sodas.quantiles$`2`[4],
              sodas.quantiles$`3`[4],
              sodas.quantiles$`4`[4],
              sodas.quantiles$`5`[4])
names(sodas.q3) <- days.ordered
sodas.max <- tapply(cafe.data$Sodas, cafe.data$`Day Code`, max, na.rm=TRUE)
names(sodas.max) <- days.ordered
desc.sodas.days <- data.frame(days.ordered, sodas.n, sodas.na, sodas.mean, sodas.semean, sodas.stdev, 
                              sodas.min, sodas.q1, sodas.median, sodas.q3, sodas.max)
day.col.names <- col.names <- c('Day Of Week', 'N', 'NA', 'Mean', 'SE Mean', 'StDev', 
                                'Minimum', 'Q1', 'Median', 'Q3', 'Maximum')
names(desc.sodas.days) <- day.col.names
View(desc.sodas.days)

# descriptive statistics for total coffee sales || day-dependent
days.ordered <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri')
coffees.n <- c(length(na.omit(cafe.data[cafe.data$`Day Code` == 1, ]$Coffees)),
             length(na.omit(cafe.data[cafe.data$`Day Code` == 2, ]$Coffees)),
             length(na.omit(cafe.data[cafe.data$`Day Code` == 3, ]$Coffees)),
             length(na.omit(cafe.data[cafe.data$`Day Code` == 4, ]$Coffees)),
             length(na.omit(cafe.data[cafe.data$`Day Code` == 5, ]$Coffees)))
names(coffees.n) <- days.ordered
coffees.na <- c(sum(is.na(cafe.data[cafe.data$`Day Code` == 1, ]$Coffees)),
              sum(is.na(cafe.data[cafe.data$`Day Code` == 2, ]$Coffees)),
              sum(is.na(cafe.data[cafe.data$`Day Code` == 3, ]$Coffees)),
              sum(is.na(cafe.data[cafe.data$`Day Code` == 4, ]$Coffees)),
              sum(is.na(cafe.data[cafe.data$`Day Code` == 5, ]$Coffees)))
names(coffees.na) <- days.ordered
coffees.mean <- tapply(cafe.data$Coffees, cafe.data$`Day Code`, mean, na.rm=TRUE)
names(coffees.mean) <- days.ordered
coffees.stdev <- tapply(cafe.data$Coffees, cafe.data$`Day Code`, sd, na.rm=TRUE)
names(coffees.stdev) <- days.ordered
tmp.lengths <- c(length(na.omit(cafe.data[cafe.data$`Day Code` == 1, ]$Coffees)),
                 length(na.omit(cafe.data[cafe.data$`Day Code` == 2, ]$Coffees)),
                 length(na.omit(cafe.data[cafe.data$`Day Code` == 3, ]$Coffees)),
                 length(na.omit(cafe.data[cafe.data$`Day Code` == 4, ]$Coffees)),
                 length(na.omit(cafe.data[cafe.data$`Day Code` == 5, ]$Coffees)))
coffees.semean <- coffees.stdev / sqrt(tmp.lengths)
names(coffees.semean) <- days.ordered
coffees.min <- tapply(cafe.data$Coffees, cafe.data$`Day Code`, min, na.rm=TRUE)
names(coffees.min) <- days.ordered
coffees.quantiles <- tapply(cafe.data$Coffees, cafe.data$`Day Code`, quantile, na.rm=TRUE)
coffees.q1 <- c(coffees.quantiles$`1`[2],
                coffees.quantiles$`2`[2],
                coffees.quantiles$`3`[2],
                coffees.quantiles$`4`[2],
                coffees.quantiles$`5`[2])
names(coffees.q1) <- days.ordered
coffees.median <- c(coffees.quantiles$`1`[3],
                    coffees.quantiles$`2`[3],
                    coffees.quantiles$`3`[3],
                    coffees.quantiles$`4`[3],
                    coffees.quantiles$`5`[3])
names(coffees.median) <- days.ordered
coffees.q3 <- c(coffees.quantiles$`1`[4],
                coffees.quantiles$`2`[4],
                coffees.quantiles$`3`[4],
                coffees.quantiles$`4`[4],
                coffees.quantiles$`5`[4])
names(coffees.q3) <- days.ordered
coffees.max <- tapply(cafe.data$Coffees, cafe.data$`Day Code`, max, na.rm=TRUE)
names(coffees.max) <- days.ordered
desc.coffees.days <- data.frame(days.ordered, coffees.n, coffees.na, coffees.mean, coffees.semean, coffees.stdev, 
                           coffees.min, coffees.q1, coffees.median, coffees.q3, coffees.max)
day.col.names <- col.names <- c('Day Of Week', 'N', 'NA', 'Mean', 'SE Mean', 'StDev', 
                                'Minimum', 'Q1', 'Median', 'Q3', 'Maximum')
names(desc.coffees.days) <- day.col.names
View(desc.coffees.days)

# TODO: take some time to analyze these results, add some comments


## TIME-SERIES
# time-series plots and analysis of coffee and soda sales
install.packages('xts')
require(graphics)
require(xts)

# construct time-series objects
sodas.ts <- xts(cafe.data$Sodas, cafe.data$Date)
coffees.ts <- xts(cafe.data$Coffees, cafe.data$Date)
both.ts <- merge.xts(sodas.ts, coffees.ts)

# plot both time-series for analysis
# TODO: make it pretty
plot.xts(both.ts)

# TODO: analyze further