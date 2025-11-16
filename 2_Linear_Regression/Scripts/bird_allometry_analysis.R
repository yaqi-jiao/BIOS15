# fitting a linear regression to bird allometry data
# estimate the allometric slopes and intercepts for the brain-body scaling
# whether these are similar in males and females.

# load and preview data
birds <- read.csv("Data/bird_allometry.csv")
head(birds)

# extract data based on sex
male_allometry <-  birds[birds$Sex=="m",]
female_allometry <- birds[birds$Sex=="f",]

# add log column
male_allometry$log_body <- log(male_allometry$body_mass)
male_allometry$log_brain <- log(male_allometry$brain_mass)
female_allometry$log_body <- log(female_allometry$body_mass)
female_allometry$log_brain <- log(female_allometry$brain_mass)

# fit linear regression model to log-converted brain mass and body mass
mm <- lm(log_brain~log_body, data=male_allometry)
mf <- lm(log_brain~log_body, data=female_allometry)

# check whether residuals is normal distribution
# preview
hist(residuals(mm), main="Male Residuals", xlab="Residuals", las=1)
hist(residuals(mf), main="Female Residuals", xlab="Residuals", las=1)

# export
png("results/residuals_hist_male_brain_body_allometry.png", 
    width=800, height=600)
hist(residuals(mm), main="Male residuals", xlab="", las=1)
dev.off()
png("results/residuals_hist_female_brain_body_allometry.png", 
    width=800, height=600)
hist(residuals(mf), main="Female residuals", xlab="", las=1)
dev.off()

# summary model
male_summary <- capture.output(summary(mm))
writeLines(male_summary, "results/summary_male_model.txt")
female_summary <- capture.output(summary(mf))
writeLines(female_summary, "results/summary_female_model.txt")
# What kind of analysis could we do to directly estimate the difference in slope between males and females

# produce a scatter plot to illustrate result
# to compare the male and female group, two results are illustrate in the same figure
# create dataframe
# draw male data first
plot(df$log_body, df$log_brain,
     col="blue", pch=16, xlab="log(Body mass)", ylab="log(Brain mass)",
     main="Allometric scaling of brain vs body mass", data=male_allometry)
# add female data as points
points(df$log_body, df$log_brain,
       col="red", pch=17, data=female_allometry)
# add regression line
abline(lm(log_brain ~ log_body, data=male_allometry), col="blue", lwd=2)
abline(lm(log_brain ~ log_body, data=female_allometry), col="red", lwd=2)
# add legend
legend("topleft", legend=c("Male","Female"), col=c("blue","red"),
       pch=c(16,17), lwd=2)
png("results/scatter_brain_body_allometry.png", 
    width=800, height=600)

