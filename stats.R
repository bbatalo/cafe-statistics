# set working directory to git repository
# change parameters if needed
home <- Sys.getenv("USERPROFILE")
repo <- "/repos/cafe-statistics"
setwd(paste(home, repo, sep=""))
getwd()

# make sure you have readxl installed and loaded
install.packages("readxl")
library("readxl")

source("util.R")

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
desc.sodas.days <- describe.by.day(cafe.data, "Sodas")
View(desc.sodas.days)

# descriptive statistics for total coffee sales || day-dependent
desc.coffees.days <- describe.by.day(cafe.data, "Coffees")
View(desc.coffees.days)

# descriptive statistics for total wraps sales|| day-dependent
desc.wraps.sold.days <- describe.by.day(cafe.data, "Wraps Sold")
View(desc.wraps.sold.days)

# descriptive statistics for total bread sandwich sales|| day-dependent
desc.sand.sold.days <- describe.by.day(cafe.data, "Bread Sand Sold")
View(desc.sand.sold.days)

# descriptive statistics for total muffin sales || day-dependent
desc.muffins.sold.days <- describe.by.day(cafe.data, "Muffins Sold")
View(desc.muffins.sold.days)

# descriptive statistics for total cookie sales || day-dependent
desc.cookies.sold.days <- describe.by.day(cafe.data, "Cookies Sold")
View(desc.cookies.sold.days)

# descriptive statistics for total fruit cup sales || day-dependent
desc.fruit.sold.days <- describe.by.day(cafe.data, "Fruit Cup Sold")
View(desc.fruit.sold.days)

# descriptive statistics for total chips sales || day-dependent
desc.chips.days <- describe.by.day(cafe.data, "Chips")
View(desc.chips.days)

# descriptive statistics for total juice sales || day-dependent
desc.juices.days <- describe.by.day(cafe.data, "Juices")
View(desc.juices.days)




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