# set working directory to git repository
# change parameters if needed
home <- Sys.getenv("USERPROFILE")
repo <- "/repos/cafe-statistics"
setwd(paste(home, repo, sep=""))

# make sure you have readxl installed and loaded
install.packages("readxl")
library("readxl")

# load dataset with correct attribute types and NA value parsing
cafe.data <- read_excel("cafedata.xls", na = "na")
View(cafe.data)

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


