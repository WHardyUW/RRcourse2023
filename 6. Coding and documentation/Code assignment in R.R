# Sets the path to the parent directory of RR classes
setwd("D:\\Quantitative\\RR_lab\\RR_lab4_1\\6. Coding and documentation")

# Import data from the O*NET database, at ISCO-08 occupation level.
# The original data uses a version of SOC classification, but the data we load here
# are already cross-walked to ISCO-08 using: https://ibs.org.pl/en/resources/occupation-classifications-crosswalks-from-onet-soc-to-isco/

# The O*NET database contains information for occupations in the USA, including
# the tasks and activities typically associated with a specific occupation.

task_data = read.csv("Data\\onet_tasks.csv")
# isco08 variable is for occupation codes
# the t_* variables are specific tasks conducted on the job

# read employment data from Eurostat
# These datasets include quarterly information on the number of workers in specific
# 1-digit ISCO occupation categories. (Check here for details: https://www.ilo.org/public/english/bureau/stat/isco/isco08/)
library(readxl)     

list_of_sheet <- c("ISCO1", "ISCO2", "ISCO3", "ISCO4", "ISCO5", "ISCO6", "ISCO7", "ISCO8", "ISCO9")

for (i in 1:length(list_of_sheet)) {
  assign(paste0("isco", i), read_excel("Data\\Eurostat_employment_isco.xlsx", sheet = list_of_sheet[i]))
}

# Let's merge all these datasets. We'll need a column that stores the occupation categories:
for (i in 1:length(list_of_sheet)) {
  assign(paste0("isco", i), transform(get(paste0("isco", i)), ISCO = i))
}
isco1$ISCO <- 1
isco2$ISCO <- 2
isco3$ISCO <- 3
isco4$ISCO <- 4
isco5$ISCO <- 5
isco6$ISCO <- 6
isco7$ISCO <- 7
isco8$ISCO <- 8
isco9$ISCO <- 9

# Let's merge all these datasets. We'll need a column that stores the occupation categories:
# and this gives us one large file with employment in all occupations.
all_data <- rbind(isco1, isco2, isco3, isco4, isco5, isco6, isco7, isco8, isco9)

# We will focus on three countries, but perhaps we could clean this code to allow it
# to easily run for all the countries in the sample?

list_of_isco <- list(isco1, isco2, isco3, isco4, isco5, isco6, isco7, isco8, isco9)
#create country list
countries <- c("Belgium", "Spain", "Poland")

# Create an empty vector for each country's total
totals <- c()

for (country in countries) {
  total <- 0
  for (i in 1:length(list_of_sheet)) {
    isco <- get(paste0("isco", i))
    total <- total + isco[, country]
  }
  var <- unlist(total)
  var <- rep(var,9)
  totals[[country]] <- var
}

# adding a vector that is 9 times the previously calculated totals
for (country in countries) {

   all_data[[paste0("total_", country)]] <- unlist(totals[[country]])
}
# And this will give us shares of each occupation among all workers in a period-country
for (country in countries) {
all_data[[paste0("share_", country)]] <- all_data[[paste0(country)]]/all_data[[paste0("total_", country)]]
}

#remove the individual ones as we can access from all_data now: 
rm(isco1, isco2, isco3, isco4, isco5, isco6, isco7, isco8, isco9)

# Now let's look at the task data. We want the first digit of the ISCO variable only
library(stringr)

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
library(dplyr)

combined <- left_join(all_data, aggdata, by = c("ISCO" = "isco08_1dig"))

# Traditionally, the first step is to standardise the task values using weights 
# defined by share of occupations in the labour force. This should be done separately
# for each country. Standardisation -> getting the mean to 0 and std. dev. to 1.
# Let's do this for each of the variables that interests us:

#install.packages("Hmisc")
library(Hmisc)


# first task item # second task item # third task item
selected_column <- c("t_4A2a4", "t_4A2b2","t_4A4a1")

for (column in 1:length(selected_column)) {
  for (country in countries) {
    temp_mean <- wtd.mean(combined[selected_column[column]], combined[country])
    temp_sd <- wtd.var(combined[selected_column[column]], combined[country]) %>% sqrt()
    combined[paste0("std_",country,"_", selected_column[column])] = (combined[selected_column[column]]-temp_mean)/temp_sd
  }
}

# The next step is to calculate the `classic` task content intensity, i.e.
# how important is a particular general task content category in the workforce
# Here, we're looking at non-routine cognitive analytical tasks, as defined
# by David Autor and Darron Acemoglu:
temp <- 0
for (country in countries) {
  for (column in 1:length(selected_column)) {
    temp <- temp + combined[paste0("std_",country,"_", selected_column[column])]
  }
  combined[paste0(country,"_NRCA")] <- temp
}

# And we standardise NRCA in a similar way.
for (country in countries){
  temp_mean <- wtd.mean(combined[paste0(country, "_NRCA")], combined[country])
  temp_sd <- wtd.var(combined[paste0(country, "_NRCA")], combined[country]) %>% sqrt()
  combined[paste0("std_",country,"_NRCA")] = (combined[paste0(country, "_NRCA")]-temp_mean)/temp_sd
}

# Finally, to track the changes over time, we have to calculate a country-level mean
# Step 1: multiply the value by the share of such workers.
for (country in countries){
  combined[paste0("multip_",country,"_NRCA")] <- (combined[paste0("std_",country,"_NRCA")]*combined[paste0("share_",country)])
}

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

