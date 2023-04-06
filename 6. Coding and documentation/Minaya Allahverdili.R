# Set the working directory
setwd("C:/Users/allah/RRcourse2023/6. Coding and documentation")


# Import task data from O*NET database, cross-walked to ISCO-08 occupation level
task_data <- read.csv("Data/onet_tasks.csv")

# Import employment data from Eurostat, quarterly information on number of workers by ISCO-08 occupation category
library(readxl)

sheets <- paste0("ISCO", 1:9)
all_data <- data.frame()

for(sheet in sheets){
  isco_data <- read_excel("Data/Eurostat_employment_isco.xlsx", sheet=sheet)
  isco_data$ISCO <- as.integer(str_sub(sheet, 5))
  all_data <- rbind(all_data, isco_data)
}

str (all_data)
# Convert employment columns to numeric
all_data[,2:10] <- sapply(all_data[,2:10], function(x) as.numeric(as.character(x)))

# Sum employment in each country
total_employment <- colSums(all_data[,2:10], na.rm = TRUE)


# Add total employment as a new column
all_data$total_employment <- rep(total_employment, each = nrow(all_data)/length(total_employment))


# Calculate share of each occupation among all workers in a period-country
all_data$share_Belgium <- all_data$Belgium / all_data$total_employment[1]
all_data$share_Spain <- all_data$Spain / all_data$total_employment[2]
all_data$share_Poland <- all_data$Poland / all_data$total_employment[3]




# Extract first digit of ISCO-08 occupation codes
library(stringr)
task_data$isco08_1dig <- as.integer(str_sub(task_data$isco08, 1, 1))
 
print(colnames(task_data))

# Aggregate task data at 1-digit ISCO-08 occupation level
aggdata <- aggregate(task_data[,1:25], by=list(task_data$isco08_1dig), FUN=mean, na.rm=TRUE)


# Remove ISCO-08 code variable from aggregated data
aggdata$Group.1 <- NULL

# Rename variables in aggregated data to more descriptive names
names(aggdata) <- c("isco08_1dig", "task_duration", "task_math", "task_reading", "task_writing", "task_management",
                    "task_interpersonal", "task_information", "task_systems", "task_miscellaneous")

# Write cleaned data to file
write.csv(all_data, file="Data/cleaned_employment_data.csv", row.names=FALSE)
write.csv(aggdata, file="Data/cleaned_task_data.csv", row.names=FALSE)

