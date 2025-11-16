# input interested response 
# output:

# package preparation
library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)

# main logic
analysis_and_plot <- function(response, dat, y_label, title) {
  # generate linera regression model
  formu <- as.formula(paste(response, "~ LarvalHost * MaternalHost"))
  model <- lm(formu, data = dat)
  
  # save output file
  out_file <- paste0("./Result/", response, "_summary.txt")
  sink(out_file)
  on.exit(sink(), add = TRUE)
  
  cat("===== ANOVA =====\n")
  print(anova(model))
  
  cat("\n\n===== SUMMARY =====\n")
  print(summary(model))
     
  
  # generate datadframe
  anova_table <- as.data.frame(anova(model))
  coef_table <- as.data.frame(summary(model)$coefficients)
  
  # create plot data
  plot_data <- dat %>%
  group_by(LarvalHost, MaternalHost) %>%
  summarise(
    mean_time = mean((.data[[response]]), na.rm = TRUE),
    se_time = sd((.data[[response]])) / sqrt(n())
  )
  
  # plot figure
  p <- ggplot(plot_data, aes(x = LarvalHost,
                        y = mean_time,
                        color = MaternalHost,
                        group = MaternalHost)) +
    geom_point(size = 3) +
    geom_line(linewidth = 1) +
    geom_errorbar(aes(ymin = mean_time - se_time,
                      ymax = mean_time + se_time),
                  width = 0.15,
                  linewidth = 0.8) +
    labs(
      x = "Larval host plant",
      y = y_label,
      color = "Maternal host plant",
      title = title,
      subtitle = "Error bars = SE"
    ) +
    theme_bw(base_size = 16)
  
  # save
  filename <- paste0("./Figure/", response, "_plot.png")
  ggsave(filename, plot = p, width = 7, height = 5, dpi = 300)
  
  # return data
  return(list(anova_table = anova_table,
  coef_table = coef_table,
  plot = p))
}

# 
# # mini test
# analysis_and_plot("DevelopmentTime", dat, "DevelopmentTime", "DevelopmentTime")
