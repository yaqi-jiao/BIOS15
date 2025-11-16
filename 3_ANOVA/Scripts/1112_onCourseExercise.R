set.seed(100)
# simulate data
# create 3 groups, each has 50 variables, with 3 different levels: low, medium, high
# Because anova is comparison of variables from different groups, the linear regression is no longer modeling numeric variable, it is modeling group variable, which is different
groups = as.factor(rep(c("Low", "Medium", "High"), each=50))
# create 3 normal distribution, with different mean, but same variance
x <-  c(rnorm(50, 10, 3), rnorm(50, 13, 3), rnorm(50, 14, 3))
# plot figure to summary the distribution of target data
library(ggplot2)
df <- data.frame(groups, x)
# draw the distribution to view the data
ggplot(df, aes(x = groups, y = x)) +
  geom_boxplot(width = 0.25, position = position_nudge(x = 0.2), fill="lightgray") +
  geom_jitter(position = position_nudge(x = -0.2), color="lightgrey", alpha=0.7) +
  annotate("point" ,x = 0.8, y = 14, color = "red", size = 3, shape = 18) +
  annotate("point", x = 1.8, y = 10, color = "red", size = 3, shape = 18) +
  annotate("point", x = 2.8, y = 13, color = "red", size = 3, shape = 18) +
  labs(x = "", y = "Body size (g)") +
  theme_minimal()

# fit linear model
m <-  lm(x~groups)
anova_m <- anova(m)

total_sum_sq <- sum(anova_m$`Sum Sq`)
Var_T <- total_sum_sq / (150 - 1)
summary(m)

# suppress the intercept of the model
newm = lm(x~groups-1)
summary(newm)