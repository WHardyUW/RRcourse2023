install.packages("readxl")
library(readxl)
data <- read_xlsx("metaanalysis_data.xlsx")

#  boys' and girls' toy preferences
mean_boys_effect <- mean(data$Mean_boys_play_male)
mean_girls_effect <- mean(data$Mean_girls_play_male)

combined_effect <- c(mean_boys_effect, mean_girls_effect)

library(ggplot2)

# Calculate the standard errors (SE) for each effect size (assuming equal sample sizes for simplicity)
n <- length(combined_effect)
se <- 1 / sqrt(n)

# Create a data frame for the funnel plot
funnel_data <- data.frame(Effect = combined_effect, SE = se)

# Create the funnel plot
ggplot(funnel_data, aes(x = Effect, y = 1/SE)) +
  geom_point(shape = 21, fill = "blue", color = "black") +
  geom_line() +
  labs(x = "Effect Size", y = "Standard Error") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_bw()


# Calculate the mean effect sizes for studies with male and female authors
mean_male_authors_effect <- mean(data$Mean_boys_play_male[data$`Male authors` > 0])
mean_female_authors_effect <- mean(data$Mean_boys_play_male[data$`Female authors` > 0])

# Compare the effect sizes between studies with male and female authors
effect_comparison <- t.test(data$Mean_boys_play_male[data$`Male authors` > 0],
                            data$Mean_boys_play_male[data$`Female authors` > 0])

# Print the results
cat("Mean effect size for boys' toy preferences:", mean_boys_effect, "\n")
cat("Mean effect size for girls' toy preferences:", mean_girls_effect, "\n")
cat("Mean effect size for studies with male authors:", mean_male_authors_effect, "\n")
cat("Mean effect size for studies with female authors:", mean_female_authors_effect, "\n")
cat("Comparison of effect sizes between studies with male and female authors:\n")
print(effect_comparison)

# "Does gender affect results?":
#The output of the t-test indicates that the p-value is 0.4063, which is greater than the conventional significance level of 0.05. Therefore, we do not have sufficient evidence to reject the null hypothesis that the true difference in mean effect sizes between studies with male authors and studies with female authors is equal to zero.
#Based on this analysis, we can conclude that author gender does not significantly affect the study in terms of the mean effect sizes for boys' toy preferences.
#The sample estimates indicate that the mean effect size for studies with male authors is 152.728, while the mean effect size for studies with female authors is 186.280. However, the lack of statistical significance suggests that this observed difference could be due to random variation rather than a systematic effect of author gender.
