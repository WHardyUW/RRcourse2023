# Load required packages
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

# Read in viewership data 
got_data <- read.csv("got_viewership.csv")

# Convert dates to R date format
got_data$Original.Air.Date <- as.Date(got_data$Original.Air.Date, format = "%d-%b-%Y")

# Create a line plot of viewership over time
ggplot(got_data, aes(x = got_data$Original.Air.Date, y = got_data$U.S..Viewers..Millions.)) +
  geom_line() +
  labs(x = "Original Air Date", y = "Viewership (Millions)", title = "Viewership of Game of Thrones over Time")


# Calculate the average viewership for each season
season_data <- got_data %>%
  group_by(Season) %>%
  summarize(avg_viewership = mean(U.S..Viewers..Millions.))

# Create a bar chart of average viewership by season
ggplot(season_data, aes(x = Season, y = avg_viewership)) +
  geom_col() +
  labs(x = "Season", y = "Average Viewership (Millions)", title = "Average Viewership of Game of Thrones by Season")

# Calculate the total viewership for each season
total_viewership <- got_data %>%
  group_by(Season) %>%
  summarize(total_viewership = sum(`U.S. Viewers (Millions)`))

# Create a bar chart of total viewership by season
ggplot(total_viewership, aes(x = Season, y = total_viewership)) +
  geom_col() +
  labs(x = "Season", y = "Total Viewership (Millions)", title = "Total Viewership of Game of Thrones by Season")
