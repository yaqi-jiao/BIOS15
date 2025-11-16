# assess effects of maternal vs. larval host plant on one or more response variable
# Interpret the results and produce a nice plot to illustrate

# activate function
source("Analysis_and_Plot.R")

# read csv and data preparation
dat <-  read.csv("Data/butterflies.csv")
names(dat)  # check the name of columns

# create factors/predictors
factorA <- factor(dat$LarvalHost)
factorB <- factor(dat$MaternalHost)

responses <- c("DevelopmentTime", "AdultWeight", "GrowthRate")

# main logic
for (i in responses) {
  title <- paste0(i, "depend on Larva & Adult host")
  analysis_and_plot(i, dat, i, title)
}





