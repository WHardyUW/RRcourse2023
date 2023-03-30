# Import required libraries
library(readxl)

# Read O*NET task data
task_data <- read.csv("Data\\onet_tasks.csv")

# Function to read ISCO sheets
read_isco_sheets <- function(file_path, num_sheets) {
  isco_sheets <- list()
  for (i in 1:num_sheets) {
    isco_sheets[[i]] <- read_excel(file_path, sheet = paste("ISCO", i, sep=""))
  }
  return(isco_sheets)
}

# Read Eurostat employment data
employment_file_path <- "Data\\Eurostat_employment_isco.xlsx"
number_of_sheets <- 9
isco_sheets <- read_isco_sheets(employment_file_path, number_of_sheets)

# Get all countries
countries <- colnames(isco_sheets[[1]])[3:length(colnames(isco_sheets[[1]]))]

# Function to calculate worker totals for each country
calc_worker_totals <- function(isco_sheets, countries) {
  totals <- list()
  for (country in countries) {
    total <- 0
    for (sheet in isco_sheets) {
      total <- total + sheet[[country]]
    }
    totals[[country]] <- total
  }
  return(totals)
}

# Calculate worker totals for all countries
totals <- calc_worker_totals(isco_sheets, countries)

# Add ISCO and worker totals to each sheet
for (i in 1:length(isco_sheets)) {
  isco_sheets[[i]]$ISCO <- i
  for (country in countries) {
    isco_sheets[[i]][[paste("total_", country, sep="")]] <- totals[[country]]
  }
}

# Combine ISCO sheets into one dataset
all_data <- do.call(rbind, isco_sheets)

# Calculate shares of each occupation among all workers in a period-country
for (country in countries) {
  all_data[[paste("share_", country, sep="")]] <- all_data[[country]] / all_data[[paste("total_", country, sep="")]]
}

# Inspect the resulting dataset
head(all_data)


# Now let's look at the task data. We want the first digit of the ISCO variable only
library(stringr)

task_data$isco08_1dig <- str_sub(task_data$isco08, 1, 1) %>% as.numeric()

# And we'll calculate the mean task values at a 1-digit level 
# (more on what these tasks are below)

aggdata <- aggregate(task_data, by=list(task_data$isco08_1dig), FUN=mean, na.rm=TRUE)
aggdata$isco08 <- NULL

# We'll be interested in tracking the intensity of Non-routine cognitive analytical tasks
# Using a framework reminiscent of the work by David Autor.

# These are the ones we're interested in:
# Non-routine cognitive analytical
nrca_tasks <- c("t_4A2a4", "t_4A2b2", "t_4A4a1")

# Calculate mean of non-routine cognitive analytical tasks
nrca_means <- sapply(nrca_tasks, function(task) {
  return(mean(aggdata[[task]], na.rm = TRUE))
})

# Create a new data frame with the mean values of non-routine cognitive analytical tasks
nrca_mean_df <- data.frame("Task" = nrca_tasks, "Mean" = nrca_means)

# Combine the data
library(dplyr)

library(dplyr)

combined <- left_join(all_data, aggdata, by = c("ISCO" = "isco08_1dig"))

# Perform the previous operations to update the combined dataframe
# ...

# Convert the "Group.1" column in the `combined` dataframe to `character`
combined <- combined %>%
  mutate(Group.1 = as.character(Group.1))

# Join the `combined` and `nrca_mean_df` dataframes
combined <- left_join(combined, nrca_mean_df, by = c("Group.1" = "Task"))

# Add non-routine cognitive analytical tasks mean to combined data
# combined <- left_join(combined, nrca_mean_df, by = "Task")
# combined <- left_join(combined, nrca_mean_df, by = c("Group.1" = "Task"))


# Traditionally, the first step is to standardise the task values using weights 
# defined by share of occupations in the labour force. This should be done separately
# for each country. Standardisation -> getting the mean to 0 and std. dev. to 1.
# Let's do this for each of the variables that interests us:

#install.packages("Hmisc")
library(Hmisc)

# countries <- unique(combined$Country)
task_items <- c("t_4A2a4", "t_4A2b2", "t_4A4a1")

library(Hmisc)

# countries <- unique(combined$Country)
task_items <- c("t_4A2a4", "t_4A2b2", "t_4A4a1")

for (country in countries) {
  for (task_item in task_items) {
    temp_mean <- wtd.mean(combined[[task_item]], combined[[paste0("share_", country)]])
    temp_sd <- wtd.var(combined[[task_item]], combined[[paste0("share_", country)]]) %>% sqrt()
    combined[[paste0("std_", country, "_", task_item)]] <- (combined[[task_item]] - temp_mean) / temp_sd
  }
}
calculate_agg_NRCA <- function(country) {
  
  combined_col_name <- paste0("multip_", country, "_NRCA")
  share_col_name <- paste0("share_", country)
  
  # Calculate the mean of the standardized task items for each country
  combined[[paste0("std_", country, "_NRCA")]] <- rowMeans(combined[paste0("std_", country, "_", task_items)])
  std_col_name <- paste0("std_", country, "_NRCA")
  
  combined[[combined_col_name]] <- combined[[std_col_name]] * combined[[share_col_name]]
  
  agg_data <- aggregate(combined[[combined_col_name]], by = list(combined$TIME), FUN = sum, na.rm = TRUE)
  
  return(agg_data)
}


# Calculate aggregated NRCA for a given country
calculate_agg_NRCA <- function(country) {
  country_col <- paste0("multip_", country, "_NRCA")
  agg_data <- aggregate(combined[, country_col], by = list(combined$TIME), FUN = sum, na.rm = TRUE)
  colnames(agg_data) <- c("Year", "NRCA")
  return(agg_data)
}

# Plot aggregated NRCA for a given data frame and title
plot_agg_NRCA <- function(data, title) {
  if (any(is.finite(data$NRCA))) {
    plot(data$NRCA, xaxt = "n", main = title, xlab = "Year", ylab = "NRCA")
    axis(1, at = seq(1, nrow(data), 3), labels = data$Year[seq(1, nrow(data), 3)])
  } else {
    cat("No finite NRCA data available for:", title, "\n")
  }
}

for (country in countries) {
  agg_data <- calculate_agg_NRCA(country)
  plot_agg_NRCA(agg_data, paste0(country, " NRCA"))
}

####
for (country in countries) {
  for (task_item in task_items) {
    temp_mean <- wtd.mean(combined[[task_item]], combined[[paste0("share_", country)]])
    temp_sd <- wtd.var(combined[[task_item]], combined[[paste0("share_", country)]]) %>% sqrt()
    combined[[paste0("std_", country, "_", task_item)]] <- (combined[[task_item]] - temp_mean) / temp_sd
  }
}


for (country in countries) {
  std_col_name <- paste0("std_", country, "_NRCA")
  share_col_name <- paste0("share_", country)
  combined_col_name <- paste0("multip_", country, "_NRCA")
  
  combined[[std_col_name]] <- rowMeans(combined[paste0("std_", country, "_", task_items)])
  combined[[combined_col_name]] <- combined[[std_col_name]] * combined[[share_col_name]]
}

for (country in countries) {
  agg_data <- calculate_agg_NRCA(country)
  plot_agg_NRCA(agg_data, paste0(country, " NRCA"))
}


