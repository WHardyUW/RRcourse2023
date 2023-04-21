
# Importing the necessary libraries
library(readxl)
library(stringr)
library(dplyr)
library(Hmisc)

# Sets the path to the parent directory of RR classes (Defining the working directory)
setwd("C:/Users/Acer/Desktop/RR2023/6. Coding and documentation")


# Import data from the O*NET database, at ISCO-08 occupation level.
# The original data uses a version of SOC classification, but the data we load here
# are already cross-walked to ISCO-08 using: https://ibs.org.pl/en/resources/occupation-classifications-crosswalks-from-onet-soc-to-isco/

# The O*NET database contains information for occupations in the USA, including
# the tasks and activities typically associated with a specific occupation.

task_data <- read.csv("Data/onet_tasks.csv") # reading data from O*NET database

# isco08 variable is for occupation codes
# the t_* variables are specific tasks conducted on the job

glimpse(task_data) # taking a peek at the data types of the different variables


# These datasets include quarterly information on the number of workers in specific
# 1-digit ISCO occupation categories. (Check here for details: https://www.ilo.org/public/english/bureau/stat/isco/isco08/)

# reading employment data from Eurostat   

# Store sheet names in a vector
sheet_names <- paste0("ISCO", 1:9)

# Loop through sheet names and read in each sheet as a separate object with its respective variable name
for (sheet_name in sheet_names) {
  assign(paste0("isco", substr(sheet_name, 5, 5)), read_excel("Data/Eurostat_employment_isco.xlsx", sheet = sheet_name))
}

# We will focus on three countries, but perhaps we could clean this code to allow it
# to easily run for all the countries in the sample?

# This will calculate worker totals in each of the chosen countries.
total_Belgium <- isco1$Belgium + isco2$Belgium + isco3$Belgium + isco4$Belgium + isco5$Belgium + isco6$Belgium + isco7$Belgium + isco8$Belgium + isco9$Belgium
total_Spain <- isco1$Spain + isco2$Spain + isco3$Spain + isco4$Spain + isco5$Spain + isco6$Spain + isco7$Spain + isco8$Spain + isco9$Spain
total_Poland <- isco1$Poland + isco2$Poland + isco3$Poland + isco4$Poland + isco5$Poland + isco6$Poland + isco7$Poland + isco8$Poland + isco9$Poland

# Let's merge all these datasets. We'll need a column that stores the occupation categories:
for (i in 1:9) {
  assign(paste0("isco", i), transform(get(paste0("isco", i)), ISCO = i))
}

# and this gives us one large file with employment in all occupations.
all_data <- rbind(isco1, isco2, isco3, isco4, isco5, isco6, isco7, isco8, isco9)

# We have 9 occupations and the same time range for each, so we an add the totals by
# adding a vector that is 9 times the previously calculated totals

# create a data.frame with the repeated vectors as columns
totals_df <- data.frame(total_Belgium = rep(total_Belgium, times = 9),
                        total_Spain = rep(total_Spain, times = 9),
                        total_Poland = rep(total_Poland, times = 9))

# add the new columns to all_data
all_data <- cbind(all_data, totals_df)


# And this will give us shares of each occupation among all workers in a period-country
all_data[, c("share_Belgium", "share_Spain", "share_Poland")] <- 
  all_data[, c("Belgium", "Spain", "Poland")] / c(total_Belgium, total_Spain, total_Poland)


# Now let's look at the task data. We want the first digit of the ISCO variable only
task_data$isco08_1dig <- str_sub(task_data$isco08, 1, 1) %>% as.numeric()

# And we'll calculate the mean task values at a 1-digit level 
# (more on what these tasks are below)

aggdata <-aggregate(task_data, by=list(task_data$isco08_1dig),
                    FUN=mean, na.rm=TRUE)
aggdata$isco08 <- NULL

# We'll be interested in tracking the intensity of Non-routine cognitive analytical tasks
# Using a framework reminiscent of the work by David Autor.

#These are the ones we're interested in:
# Non-routine cognitive analytical
# 4.A.2.a.4 Analyzing Data or Information
# 4.A.2.b.2	Thinking Creatively
# 4.A.4.a.1	Interpreting the Meaning of Information for Others

#Let's combine the data.


combined <- left_join(all_data, aggdata, by = c("ISCO" = "isco08_1dig"))

# Traditionally, the first step is to standardise the task values using weights 
# defined by share of occupations in the labour force. This should be done separately
# for each country. Standardisation -> getting the mean to 0 and std. dev. to 1.
# Let's do this for each of the variables that interests us:

# list of task items and corresponding variable suffixes
tasks <- list("t_4A2a4" = "t_4A2a4", "t_4A2b2" = "t_4A2b2", "t_4A4a1" = "t_4A4a1")
suffixes <- list("Belgium" = "Belgium", "Poland" = "Poland", "Spain" = "Spain")

# loop through task items and countries
for (task in names(tasks)) {
  for (suffix in names(suffixes)) {
    # calculate mean and sd for current task item and country
    temp_mean <- wtd.mean(combined[[task]], combined[[paste0("share_", suffix)]])
    temp_sd <- wtd.var(combined[[task]], combined[[paste0("share_", suffix)]]) %>% sqrt()
    
    # generate variable name and calculate standardized value
    std_var <- paste0("std_", suffix, "_", task)
    combined[[std_var]] <- (combined[[task]] - temp_mean) / temp_sd
  }
}



# The next step is to calculate the `classic` task content intensity, i.e.
# how important is a particular general task content category in the workforce
# Here, we're looking at non-routine cognitive analytical tasks, as defined
# by David Autor and Darron Acemoglu:

countries <- c("Belgium", "Poland", "Spain")
tasks <- c("t_4A2a4", "t_4A2b2", "t_4A4a1")

for (country in countries) {
  std_cols <- paste0("std_", country, "_", tasks)
  combined[[paste0(country, "_NRCA")]] <- rowSums(combined[std_cols])
}


for (country in countries) {
  nrca_col <- paste0(country, "_NRCA")
  share_col <- paste0("share_", country)
  std_nrca_col <- paste0("std_", country, "_NRCA")
  
  temp_mean <- wtd.mean(combined[[nrca_col]], combined[[share_col]])
  temp_sd <- wtd.var(combined[[nrca_col]], combined[[share_col]]) %>% sqrt()
  
  combined[[std_nrca_col]] <- (combined[[nrca_col]] - temp_mean) / temp_sd
}

# Finally, to track the changes over time, we have to calculate a country-level mean
# Step 1: multiply the value by the share of such workers.
combined$multip_Spain_NRCA <- (combined$std_Spain_NRCA*combined$share_Spain)
combined$multip_Belgium_NRCA <- (combined$std_Belgium_NRCA*combined$share_Belgium)
combined$multip_Poland_NRCA <- (combined$std_Poland_NRCA*combined$share_Poland)

# Step 2: sum it up (it basically becomes another weighted mean)
agg_Spain <-aggregate(combined$multip_Spain_NRCA, by=list(combined$TIME),
                      FUN=sum, na.rm=TRUE)
agg_Belgium <-aggregate(combined$multip_Belgium_NRCA, by=list(combined$TIME),
                      FUN=sum, na.rm=TRUE)
agg_Poland <-aggregate(combined$multip_Poland_NRCA, by=list(combined$TIME),
                      FUN=sum, na.rm=TRUE)


# We can plot it now!
plot(agg_Poland$x, xaxt="n")
axis(1, at=seq(1, 40, 3), labels=agg_Poland$Group.1[seq(1, 40, 3)])

plot(agg_Spain$x, xaxt="n")
axis(1, at=seq(1, 40, 3), labels=agg_Spain$Group.1[seq(1, 40, 3)])

plot(agg_Belgium$x, xaxt="n")
axis(1, at=seq(1, 40, 3), labels=agg_Belgium$Group.1[seq(1, 40, 3)])


# If this code gets automated and cleaned properly,
#  you should be able to easily add other countries as well as other tasks.
# E.g.:

# Routine manual
# 4.A.3.a.3	Controlling Machines and Processes
# 4.C.2.d.1.i	Spend Time Making Repetitive Motions
# 4.C.3.d.3	Pace Determined by Speed of Equipment

