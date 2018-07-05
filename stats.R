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

# but, we can test correlation
sodas.coffees.cor <- cor(na.omit(coffees), na.omit(sodas))
# correlation is -0.4021 -> negative correlation

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
# bad results, see comment below for more details

# but, we can test correlation
ind <- which(is.na(coffees))
temps[ind] <- NA
coffees.temp.cor <- cor(na.omit(coffees), na.omit(temps))
# correlation is -0.741302 -> negative correlation

# 5. Test independence between soda sales and temperature
sodas.temp.tbl <- table(temps, sodas)
chisq.test(sodas.temp.tbl)     # p-value = 0.4162 -> null-hypothesis not rejected (could be independent)
# bad results, see comment below for more details

# but, we can test correlation
ind <- which(is.na(sodas))
temps[ind] <- NA
sodas.temp.cor <- cor(na.omit(sodas), na.omit(temps))
# correlation is 0.5301824 -> positive correlation

# COMMENT FOR CHI-SQUARE
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

##########################################################
# Statistics for other sales
food.desc.stats <- describe.full(cafe.data, c("Bread Sand Sold", "Wraps Sold", "Muffins Sold", "Cookies Sold"))
View(food.desc.stats)

###
# descriptive statistics for total bread sandwich sales|| day-dependent
sandwiches <- cafe.data$`Bread Sand Sold`
desc.sand.sold.days <- describe.by.day(cafe.data, "Bread Sand Sold")
View(desc.sand.sold.days)
sands.ts <- xts(sandwiches, cafe.data$Date)
plot.xts(sands.ts, main="Bread Sandwich sales trend")
boxplot(sandwiches ~ cafe.data$`Day of Week`, 
        main="Sandwich sales by day", xlab="Days", ylab="Sandwich sales",
        ylim=c(0,10))

sandwiches.norm <- shapiro.test(sandwiches)
sandwiches.norm # p-value = 0.09755 -> null-hypothesis not rejected (barely)
qqnorm(sandwiches)
qqline(sandwiches)

# test for sandwich sales normality over days
sandwich.norm.days <- normality.by.day(cafe.data, "Bread Sand Sold")
any(sandwich.norm.days[sandwich.norm.days < 0.05])
# normality not satisfied

# going with kruskal-wallis as non-parametric test
cafe.data$`Day of Week` <- as.factor(cafe.data$`Day of Week`)
kruskal.test(sandwiches ~ cafe.data$`Day of Week`, data=cafe.data)
# p-value = 0.0002652 -> we can assume that there are differences between groups

sandwiches.temp.cor <- cor(na.omit(sandwiches), na.omit(temps))
# p-value = -0.09483227 -> insignificant correlation

######
# descriptive statistics for total wraps sales|| day-dependent
wraps <- cafe.data$`Wraps Sold`
desc.wraps.sold.days <- describe.by.day(cafe.data, "Wraps Sold")
View(desc.wraps.sold.days)
wraps.ts <- xts(wraps, cafe.data$Date)
plot.xts(wraps.ts, main="Wraps sales trend")
boxplot(wraps ~ cafe.data$`Day of Week`, 
        main="Wrap sales by day", xlab="Days", ylab="Wrap sales",
        ylim=c(0,30))

wraps.norm <- shapiro.test(wraps)
wraps.norm # p-value = 0.1444 -> null-hypothesis not rejected
qqnorm(wraps)
qqline(wraps)

# test for wrap sales normality over days
wraps.norm.days <- normality.by.day(cafe.data, "Wraps Sold")
any(wraps.norm.days[wraps.norm.days < 0.05])

# going with kruskal-wallis as non-parametric test
kruskal.test(wraps ~ cafe.data$`Day of Week`, data=cafe.data)
# p-value = 0.0001315 -> we can assume that there are differences between groups

wraps.temp.cor <- cor(na.omit(wraps), na.omit(temps))
# p-value = 0.1017703 -> insignificant correlation

#####
# descriptive statistics for total muffin sales || day-dependent
muffins <- cafe.data$`Muffins Sold`
desc.muffins.sold.days <- describe.by.day(cafe.data, "Muffins Sold")
View(desc.muffins.sold.days)
muffins.ts <- xts(muffins, cafe.data$Date)
plot.xts(muffins.ts, main="Muffin sales trend")
boxplot(muffins ~ cafe.data$`Day of Week`, 
        main="Muffin sales by day", xlab="Days", ylab="Muffin sales",
        ylim=c(0,15))

muffins.norm <- shapiro.test(muffins)
muffins.norm # p-value = 3.913e-06 -> null-hypothesis rejected
qqnorm(muffins)
qqline(muffins)

# test for muffin sales normality over days
muffins.norm.days <- normality.by.day(cafe.data, "Muffins Sold")
any(muffins.norm.days[muffins.norm.days < 0.05])

# going with kruskal-wallis as non-parametric test
kruskal.test(muffins ~ cafe.data$`Day of Week`, data=cafe.data)
# p-value = 0.2709 -> we can assume that there are no significant differences between groups

muffins.temp.cor <- cor(na.omit(muffins), na.omit(temps))
# p-value = -0.0903645 -> insignificant correlation

#####
# descriptive statistics for total cookie sales || day-dependent
cookies <- cafe.data$`Cookies Sold`
desc.cookies.sold.days <- describe.by.day(cafe.data, "Cookies Sold")
View(desc.cookies.sold.days)
cookies.ts <- xts(cookies, cafe.data$Date)
plot.xts(cookies.ts, main="Cookie sales trend")
boxplot(cookies ~ cafe.data$`Day of Week`, 
        main="Cookie sales by day", xlab="Days", ylab="Cookie sales",
        ylim=c(0,15))

cookies.norm <- shapiro.test(cookies)
cookies.norm # p-value = 0.1845 -> null-hypothesis not rejected
qqnorm(cookies)
qqline(cookies)

# test for cookie sales normality over days
cookies.norm.days <- normality.by.day(cafe.data, "Cookies Sold")
any(cookies.norm.days[cookies.norm.days < 0.05])

# use anova
cookies.aov <- aov(cookies ~ cafe.data$`Day of Week`)
summary(cookies.aov)
layout(matrix(1:4, 2, 2))
plot(cookies.aov)

TukeyHSD(cookies.aov)
# no significant differences

cookies.temp.cor <- cor(na.omit(cookies), na.omit(temps))
# p-value = 0.006054085 -> insignificant correlation


#####
# descriptive statistics for total fruit cup sales || day-dependent
fruits <- cafe.data$`Fruit Cup Sold`
desc.fruit.sold.days <- describe.by.day(cafe.data, "Fruit Cup Sold")
View(desc.fruit.sold.days)

fruits.ts <- xts(fruits, cafe.data$Date)
plot.xts(fruits.ts, main="Fruit sales trend")
boxplot(fruits ~ cafe.data$`Day of Week`, 
        main="Fruit sales by day", xlab="Days", ylab="Fruit sales",
        ylim=c(0,5))

fruits.norm <- shapiro.test(fruits)
fruits.norm # p-value = 0.001236 -> null-hypothesis rejected
qqnorm(fruits)
qqline(fruits)

# test for fruit sales normality over days
fruits.norm.days <- normality.by.day(cafe.data, "Fruit Cup Sold")
any(fruits.norm.days[fruits.norm.days < 0.05])

# going with kruskal-wallis as non-parametric test
kruskal.test(fruits ~ cafe.data$`Day of Week`, data=cafe.data)
# p-value = 0.2099 -> we can assume that there are no significant differences between groups

fruits.temp.cor <- cor(na.omit(fruits), na.omit(temps))
# p-value = 0.07449467 -> insignificant correlation

#####
# descriptive statistics for total chips sales || day-dependent
chips <- cafe.data$Chips
desc.chips.days <- describe.by.day(cafe.data, "Chips")
View(desc.chips.days)

chips.ts <- xts(chips, cafe.data$Date)
plot.xts(chips.ts, main="Chips sales trend")
boxplot(chips ~ cafe.data$`Day of Week`, 
        main="Chips sales by day", xlab="Days", ylab="Chips sales",
        ylim=c(0,25))

chips.norm <- shapiro.test(chips)
chips.norm # p-value = 0.04132 -> null-hypothesis rejected
qqnorm(chips)
qqline(chips)

# test for chips sales normality over days
chips.norm.days <- normality.by.day(cafe.data, "Chips")
any(chips.norm.days[chips.norm.days < 0.05])

# use anova
chips.aov <- aov(chips ~ cafe.data$`Day of Week`)
summary(chips.aov)
layout(matrix(1:4, 2, 2))
plot(chips.aov)

TukeyHSD(chips.aov)
# no significant differences between groups

chips.temp.cor <- cor(na.omit(chips), na.omit(temps))
# p-value = -0.1778598 -> insignificant correlation

#####
# descriptive statistics for total juice sales || day-dependent
juices <- cafe.data$Juices
desc.juices.days <- describe.by.day(cafe.data, "Juices")
View(desc.juices.days)

juices.ts <- xts(juices, cafe.data$Date)
plot.xts(juices.ts, main="Juices sales trend")
boxplot(juices ~ cafe.data$`Day of Week`, 
        main="Juice sales by day", xlab="Days", ylab="Juice sales",
        ylim=c(0,15))

juices.norm <- shapiro.test(juices)
juices.norm # p-value = 1.161e-05 -> null-hypothesis not rejected
qqnorm(juices)
qqline(juices)

# test for juice sales normality over days
juices.norm.days <- normality.by.day(cafe.data, "Juices")
any(juices.norm.days[juices.norm.days < 0.05])

# going with kruskal-wallis as non-parametric test
kruskal.test(juices ~ cafe.data$`Day of Week`, data=cafe.data)
# p-value = 0.1573 -> we can assume that there are no significant differences between groups

juices.temp.cor <- cor(na.omit(juices), na.omit(temps))
# p-value = -0.06330148 -> insignificant correlation

#####################################################
# REGRESSIONS

temps <- cafe.data$`Max Daily Temperature (F)`
t <- cafe.data$t

time.temp.cor <- cor(temps, t)
time.temp.cor # 0.7615248 -> high correlation

### SODAS
sodas <- cafe.data$Sodas

# sodas vs time regression
sodas.time.model <- lm(sodas ~ t)
summary(sodas.time.model)
sodas.time.model$coefficients
layout(matrix(1))
plot(t, sodas)
abline(sodas.time.model, col='red')
layout(matrix(1:4, 2, 2))
plot(sodas.time.model)

# sodas vs temps regression
sodas.temps.model <- lm(sodas ~ temps)
summary(sodas.temps.model)
sodas.temps.model$coefficients
layout(matrix(1))
plot(temps, sodas)
abline(sodas.temps.model, col='red')
layout(matrix(1:4, 2, 2))
plot(sodas.temps.model)

### COFFEES
coffees <- cafe.data$Coffees

# coffees vs time regression
coffees.time.model <- lm(coffees ~ t)
summary(coffees.time.model)
coffees.time.model$coefficients
layout(matrix(1))
plot(t, coffees)
abline(coffees.time.model, col='red')
layout(matrix(1:4, 2, 2))
plot(coffees.time.model)

# coffees vs temps regression
coffees.temps.model <- lm(coffees ~ temps)
summary(coffees.temps.model)
coffees.temps.model$coefficients
layout(matrix(1))
plot(temps, coffees)
abline(coffees.temps.model, col='red')
layout(matrix(1:4, 2, 2))
plot(coffees.temps.model)

### COFFEE AND SODA
both <- cafe.data$`Total Soda and Coffee`

# against t
both.time.model <- lm(both ~ t)
summary(both.time.model)
both.time.model$coefficients
layout(matrix(1))
plot(t, both)
abline(both.time.model, col='red')
layout(matrix(1:4, 2, 2))
plot(both.time.model)

# total coffee and soda sales vs temps
both.temps.model <- lm(both ~ temps)
summary(both.temps.model)
layout(matrix(1))
plot(temps, both)
abline(both.temps.model, col='red')
layout(matrix(1:4, 2, 2))
plot(both.temps.model)

### SALES
sales <- cafe.data$Sales

# sales against t
sales.time.model <- lm(sales ~ t)
summary(sales.time.model)
sales.time.model$coefficients
layout(matrix(1))
plot(t, sales)
abline(sales.time.model, col='red')
layout(matrix(1:4, 2, 2))
plot(sales.time.model)

# sales against temps
sales.temps.model <- lm(sales ~ temps)
summary(sales.temps.model)
sales.temps.model$coefficients
layout(matrix(1))
plot(temps, sales)
abline(sales.temps.model, col='red')
layout(matrix(1:4, 2, 2))
plot(sales.temps.model)

### WASTE
bread.waste <- cafe.data$`Bread Sand Waste`
wraps.waste <- cafe.data$`Wraps Waste`
muffins.waste <- cafe.data$`Muffins Waste`
cookies.waste <- cafe.data$`Cookies Waste`
fruits.waste <- cafe.data$`Fruit Cup Waste`
total.waste <- cafe.data$`Total Items Wasted`

# total waste regression
total.waste.time.model <- lm(total.waste ~ t)
summary(total.waste.time.model)
total.waste.time.model$coefficients
layout(matrix(1))
plot(t, total.waste)
abline(total.waste.time.model, col='red')
layout(matrix(1:4, 2, 2))
plot(total.waste.time.model)

# bread waste regression
bread.waste.time.model <- lm(bread.waste ~ t)
summary(bread.waste.time.model)
bread.waste.time.model$coefficients
layout(matrix(1))
plot(t, bread.waste)
abline(bread.waste.time.model, col='red')
layout(matrix(1:4, 2, 2))
plot(bread.waste.time.model)

# wraps waste regression
wraps.waste.time.model <- lm(wraps.waste ~ t)
summary(wraps.waste.time.model)
wraps.waste.time.model$coefficients
layout(matrix(1))
plot(t, wraps.waste)
abline(wraps.waste.time.model, col='red')
layout(matrix(1:4, 2, 2))
plot(wraps.waste.time.model)

# muffins waste regression
muffins.waste.time.model <- lm(muffins.waste ~ t)
summary(muffins.waste.time.model)
muffins.waste.time.model$coefficients
layout(matrix(1))
plot(t, muffins.waste)
abline(muffins.waste.time.model, col='red')
layout(matrix(1:4, 2, 2))
plot(muffins.waste.time.model)

# cookies waste regression
cookies.waste.time.model <- lm(cookies.waste ~ t)
summary(cookies.waste.time.model)
cookies.waste.time.model$coefficients
layout(matrix(1))
plot(t, cookies.waste)
abline(cookies.waste.time.model, col='red')
layout(matrix(1:4, 2, 2))
plot(cookies.waste.time.model)

# fruits waste regression
fruits.waste.time.model <- lm(fruits.waste ~ t)
summary(fruits.waste.time.model)
fruits.waste.time.model$coefficients
layout(matrix(1))
plot(t, fruits.waste)
abline(fruits.waste.time.model, col='red')
layout(matrix(1:4, 2, 2))
plot(fruits.waste.time.model)


### REGRESSION BY DAYS
days <- cafe.data$`Day Code`

days.mon <- days == 1
zeros <- rep(0, length(days))
zeros[days.mon] <- 1
monday <- zeros

days.tue <- days == 2
zeros <- rep(0, length(days))
zeros[days.tue] <- 1
tuesday <- zeros

days.wed <- days == 3
zeros <- rep(0, length(days))
zeros[days.wed] <- 1
wednesday <- zeros

days.thu <- days == 4
zeros <- rep(0, length(days))
zeros[days.thu] <- 1
thursday <- zeros

days.fri <- days == 5
zeros <- rep(0, length(days))
zeros[days.fri] <- 1
friday <- zeros

reg.dataset <- data.frame(sodas, coffees, sales, monday, tuesday, wednesday, thursday, friday, t)
names(reg.dataset) <- c('Sodas', 'Coffees', 'Sales', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Time')
View(reg.dataset)

sales.days.model <- lm(sales ~ t + tuesday + wednesday + thursday + friday, data=reg.dataset)
summary(sales.days.model)
sales.days.model$coefficients