# descriptive statistics for a column
describe <- function(dataset, column) {
  col.names <- c('Variable', 'N', 'NA', 'Mean', 'SE Mean', 'StDev', 'Minimum', 'Q1', 'Median', 'Q3', 'Maximum', 'Total')
  items <- dataset[[column]]
  n <- length(na.omit(items))
  na <- sum(is.na(items))
  mean <- mean(items, na.rm=TRUE)
  sd <- sd(items, na.rm=TRUE)
  semean <- (sd[1] / sqrt(length(items[!is.na(items)])))
  min <- min(items, na.rm=TRUE)
  max <- max(items, na.rm=TRUE)
  quantiles <- quantile(items, na.rm=TRUE)
  q1 <- quantiles[2]
  median <- quantiles[3]
  q3 <- quantiles[4]
  total <- sum(items, na.rm=TRUE)
  desc.stats <- data.frame(column, n, na, mean, sd, semean, min, q1, median, q3, max, total)
  names(desc.stats) <- col.names
  row.names(desc.stats) <- c()
  desc.stats
}

# descriptive statistics for specified columns
describe.full <- function(dataset, names) {
  desc.stats <- data.frame()
  for (name in names) {
    tmp.stats <- describe(dataset, name)
    desc.stats <- rbind(desc.stats, tmp.stats)
  }
  desc.stats
}

# descriptive statistics for column by days
describe.by.day <- function(dataset, column) {
  days.ordered <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri')
  items.n <- c(length(na.omit(dataset[dataset$`Day Code` == 1, ][[column]])),
               length(na.omit(dataset[dataset$`Day Code` == 2, ][[column]])),
               length(na.omit(dataset[dataset$`Day Code` == 3, ][[column]])),
               length(na.omit(dataset[dataset$`Day Code` == 4, ][[column]])),
               length(na.omit(dataset[dataset$`Day Code` == 5, ][[column]])))
  names(items.n) <- days.ordered
  items.n
  
  items.na <- c(sum(is.na(dataset[dataset$`Day Code` == 1, ][[column]])),
                sum(is.na(dataset[dataset$`Day Code` == 2, ][[column]])),
                sum(is.na(dataset[dataset$`Day Code` == 3, ][[column]])),
                sum(is.na(dataset[dataset$`Day Code` == 4, ][[column]])),
                sum(is.na(dataset[dataset$`Day Code` == 5, ][[column]])))
  names(items.na) <- days.ordered
  items.mean <- tapply(dataset[[column]], dataset$`Day Code`, mean, na.rm=TRUE)
  names(items.mean) <- days.ordered
  items.stdev <- tapply(dataset[[column]], dataset$`Day Code`, sd, na.rm=TRUE)
  names(items.stdev) <- days.ordered
  tmp.lengths <- c(length(na.omit(dataset[dataset$`Day Code` == 1, ][[column]])),
                   length(na.omit(dataset[dataset$`Day Code` == 2, ][[column]])),
                   length(na.omit(dataset[dataset$`Day Code` == 3, ][[column]])),
                   length(na.omit(dataset[dataset$`Day Code` == 4, ][[column]])),
                   length(na.omit(dataset[dataset$`Day Code` == 5, ][[column]])))
  items.semean <- items.stdev / sqrt(tmp.lengths)
  names(items.semean) <- days.ordered
  items.min <- tapply(dataset[[column]], dataset$`Day Code`, min, na.rm=TRUE)
  names(items.min) <- days.ordered
  items.quantiles <- tapply(dataset[[column]], dataset$`Day Code`, quantile, na.rm=TRUE)
  items.q1 <- c(items.quantiles$`1`[2],
                items.quantiles$`2`[2],
                items.quantiles$`3`[2],
                items.quantiles$`4`[2],
                items.quantiles$`5`[2])
  names(items.q1) <- days.ordered
  items.median <- c(items.quantiles$`1`[3],
                    items.quantiles$`2`[3],
                    items.quantiles$`3`[3],
                    items.quantiles$`4`[3],
                    items.quantiles$`5`[3])
  names(items.median) <- days.ordered
  items.q3 <- c(items.quantiles$`1`[4],
                items.quantiles$`2`[4],
                items.quantiles$`3`[4],
                items.quantiles$`4`[4],
                items.quantiles$`5`[4])
  names(items.q3) <- days.ordered
  items.max <- tapply(dataset[[column]], dataset$`Day Code`, max, na.rm=TRUE)
  names(items.max) <- days.ordered
  desc.items.days <- data.frame(days.ordered, items.n, items.na, items.mean, items.semean, items.stdev, 
                                items.min, items.q1, items.median, items.q3, items.max)
  day.col.names <- col.names <- c('Day Of Week', 'N', 'NA', 'Mean', 'SE Mean', 'StDev', 
                                  'Minimum', 'Q1', 'Median', 'Q3', 'Maximum')
  names(desc.items.days) <- day.col.names
  desc.items.days
}