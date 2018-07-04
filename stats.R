# set working directory to git repository
# change parameters if needed
home <- Sys.getenv("USERPROFILE")
repo <- "/repos/cafe-statistics"
setwd(paste(home, repo, sep=""))
getwd()

# make sure you have readxl installed and loaded
install.packages("readxl")
install.packages('xts')
library("readxl")
library(graphics)
library(xts)

source("util.R")

## LOADING
# load dataset with correct attribute types and NA value parsing
cafe.data <- read_excel("cafedata.xls", na = "na")
View(cafe.data)

## DESCRIPTIVE STATISTICS
# descriptive statistics for total coffee and soda sales || day-independent
soda.coffee.desc.stats <- describe.full(cafe.data, c("Sodas", "Coffees"))
View(soda.coffee.desc.stats)

# descriptive statistics for total soda sales || day-dependent
desc.sodas.days <- describe.by.day(cafe.data, "Sodas")
View(desc.sodas.days)
boxplot(cafe.data$Sodas ~ cafe.data$`Day of Week`,
        main="Soda sales by day", xlab="Days", ylab="Soda sales")

# descriptive statistics for total coffee sales || day-dependent
desc.coffees.days <- describe.by.day(cafe.data, "Coffees")
View(desc.coffees.days)
ord <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
boxplot(cafe.data$Coffees ~ cafe.data$`Day of Week`, 
        main="Coffee sales by day", xlab="Days", ylab="Coffee sales",
        ylim=c(0,60))

# statistic tests on coffes and sodas for days and temperature
# 1. Test indepencence between coffee and soda sales
coffees <- cafe.data$Coffees
coffee.norm <- shapiro.test(coffees)
coffee.norm
qqnorm(coffees) # p-value = 0.0945 -> null-hypothesis not rejected (could be normal distribution)
qqline(coffees)


sodas <- cafe.data$Sodas
sodas.norm <- shapiro.test(sodas)
sodas.norm      # p-value = 0.1431 -> null-hypothesis not rejected (could be normal distribution)
qqnorm(sodas)
qqline(sodas)

coffees.sodas.tbl <- table(coffees, sodas)
chisq.test(coffees.sodas.tbl)  # p-value = 0.1415 -> null-hypothesis not rejected (could be independent)
# bad results, see comment below for more details

# 2. Test independence between coffee sales and day of week
coffees.days.tbl <- table(cafe.data$`Day of Week`, coffees)
chisq.test(coffees.days.tbl)   # p-value = 0.7011 -> null-hypothesis not rejected (could be independent)

# check for normality of coffee sales in groups
coffee.norm.days <- normality.by.day(cafe.data, "Coffees")
any(coffee.norm.days[coffee.norm.days < 0.05])

coffee.aov <- aov(coffees ~ cafe.data$`Day of Week`)
summary(coffee.aov)
layout(matrix(1:4, 2, 2))
plot(coffee.aov)

TukeyHSD(coffee.aov)
# everything seems to be in order -> no significant differences in means

# 3. Test indepencence between soda sales and day of week
sodas.days.tbl <- table(cafe.data$`Day of Week`, sodas)
chisq.test(sodas.days.tbl)     # p-value = 0.5995 -> null-hypothesis not rejected (could be independent)
# bad results, see comment below for more details

# check for normality of soda sales in groups
soda.norm.days <- normality.by.day(cafe.data, "Sodas")
any(soda.norm.days[soda.norm.days < 0.05])

soda.aov <- aov(sodas ~ cafe.data$`Day of Week`)
summary(soda.aov)
layout(matrix(1:4, 2, 2))
plot(soda.aov)

TukeyHSD(soda.aov)
# significant differences in means in these pairs : (Thu, Fri), (Tue, Fri), (Tue, Mon) || p-value < 0.05
# possibly significant differences in these pairs: (Wed, Fri), (Thu, Mon), (Wed-Tue)
# see soda/day boxplot

# 4. Test independence between coffee sales and temperature
temps <- cafe.data$`Max Daily Temperature (F)`
temps.norm <- shapiro.test(temps)
temps.norm     # p-value = 0.0004153 -> null-hypothesis rejected (may not be normal distribution)
qqnorm(temps)
qqline(temps)

coffees.temp.tbl <- table(temps, coffees)
chisq.test(coffees.temp.tbl)   # p-value = 0.2746 -> null-hypothesis not rejected (could be independent)

# 5. Test independence between soda sales and temperature
sodas.temp.tbl <- table(temps, sodas)
chisq.test(sodas.temp.tbl)     # p-value = 0.4162 -> null-hypothesis not rejected (could be independent)

# COMMENT
# contingency tables contain low numbers because of large number of different values per attribute
# therefore, the test may not be accurate

# time-series for coffees and sodas -> point is to illustrate trends before regressions
# construct time-series objects
sodas.ts <- xts(sodas, cafe.data$Date)
coffees.ts <- xts(coffees, cafe.data$Date)
both.ts <- merge.xts(sodas.ts, coffees.ts)

temps.ts <- xts(temps, cafe.data$Date)
plot.xts(temps.ts, main="Temperature trend")

# plot both time-series for analysis
both.max <- c(max(sodas.ts, na.rm=TRUE), max(coffees.ts, na.rm=TRUE))
ygran <- pretty(c(0, max(both.max)), n=6)
plot.xts(both.ts, main="Coffee and Soda trends", ylim=c(ygran[1], ygran[length(ygran)]))
addLegend("topleft", on=1, 
          legend.names = c("Coffee", "Soda"), 
          lty=c(1, 1), lwd=c(2, 1),
          col=c("red", "black"))

# Statistics for other sales
food.desc.stats <- describe.full(cafe.data, c("Bread Sand Sold", "Wraps Sold", "Muffins Sold", "Cookies Sold"))
View(food.desc.stats)

# descriptive statistics for total bread sandwich sales|| day-dependent
desc.sand.sold.days <- describe.by.day(cafe.data, "Bread Sand Sold")
View(desc.sand.sold.days)
sands.ts <- xts(cafe.data$`Bread Sand Sold`)

# descriptive statistics for total wraps sales|| day-dependent
desc.wraps.sold.days <- describe.by.day(cafe.data, "Wraps Sold")
View(desc.wraps.sold.days)

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






# TODO: analyze further