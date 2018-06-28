# set working directory to git repository
# change parameters if needed
home <- Sys.getenv("USERPFOFILE")
repo <- "/repos/cafe-statistics"
setwd(paste(home, repo, sep=""))

# make sure you have readxl installed and loaded
install.packages("readxl")
library("readxl")

# load dataset with correct attribute types and NA value parsing
cafe.data <- read_excel("cafedata.xls", na = "na")

# descriptive statistics for total coffee and soda sales
desc.soda <- summary(cafe.data$Sodas)
desc.coffee <- summary(cafe.data$`Coffees`)