
# Sets the path to the parent directory of RR classes
getwd()
#setwd("D://UW//2. Summer 22-23//5. Reproducible Research//RRcourse2023//6. Coding and documentation")

#   Import data from the O*NET database, at ISCO-08 occupation level.
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

#Load package
library(readxl)
library(dplyr)
library(purrr)
#Create a table with all the worksheets:

df_list <- map(set_names(excel_sheets("Data\\Eurostat_employment_isco.xlsx")),
               read_excel, path = "Data\\Eurostat_employment_isco.xlsx")

# Let's merge all these datasets. We'll need a column that stores the occupation categories:
# and this gives us one large file with employment in all occupations.

df_list$ISCO1$ISCO <- 1
df_list$ISCO2$ISCO <- 2
df_list$ISCO3$ISCO <- 3
df_list$ISCO4$ISCO <- 4
df_list$ISCO5$ISCO <- 5
df_list$ISCO6$ISCO <- 6
df_list$ISCO7$ISCO <- 7
df_list$ISCO8$ISCO <- 8
df_list$ISCO9$ISCO <- 9

all_data <- rbind(df_list$ISCO1, df_list$ISCO2, df_list$ISCO3, df_list$ISCO4, df_list$ISCO5, df_list$ISCO6, df_list$ISCO7, df_list$ISCO8, df_list$ISCO9)


# This will calculate worker totals in each of the chosen countries.

total_worker <- function(country) {
  a <- df_list$ISCO1[[country]]+ df_list$ISCO2[[country]] + df_list$ISCO3[[country]] + df_list$ISCO4[[country]] + df_list$ISCO5[[country]] + df_list$ISCO6[[country]] + df_list$ISCO7[[country]] + df_list$ISCO8[[country]] + df_list$ISCO9[[country]]
  return(a)
}
total_worker("Belgium")
# We have 9 occupations and the same time range for each, so we an add the totals by
# adding a vector that is 9 times the previously calculated totals

all_data_total <- function(country){
  b <- c(total_worker(country),total_worker(country),total_worker(country),total_worker(country),total_worker(country),total_worker(country),total_worker(country),total_worker(country),total_worker(country))
  return(b)
}
all_data_total("Belgium")

# And this will give us shares of each occupation among all workers in a period-country

all_data_share <- function(country){
  c = all_data[[country]]/all_data_total(country)
  return(c)
}
all_data_share("Belgium")
all_data$share_Belgium <- all_data_share("Belgium")


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

combined
# Traditionally, the first step is to standardise the task values using weights 
# defined by share of occupations in the labour force. This should be done separately
# for each country. Standardisation -> getting the mean to 0 and std. dev. to 1.
# Let's do this for each of the variables that interests us:

#install.packages("Hmisc")
library(Hmisc)

# first task item

temp_mean <- function(country){
  col_name <- paste0("share_", country)
  a <- wtd.mean(combined$t_4A2a4, combined[[col_name]])
  return(a)
}
temp_mean("Belgium")

temp_sd <- function(country){
  col_name <- paste0("share_", country)
  b = wtd.var(combined$t_4A2a4, combined[[col_name]]) %>% sqrt()
  return(b)
}

temp_sd("Belgium")

combined_std_t_4A2a4 <- function(country){
  c = (combined$t_4A2a4-temp_mean(country))/temp_sd(country)
  return(c)
}

combined_std_t_4A2a4("Belgium")

# second task item
temp_mean <- function(country){
  col_name <- paste0("share_", country)
  a <- wtd.mean(combined$t_4A2b2, combined[[col_name]])
  return(a)
}
temp_mean("Belgium")

temp_sd <- function(country){
  col_name <- paste0("share_", country)
  b = wtd.var(combined$t_4A2b2, combined[[col_name]]) %>% sqrt()
  return(b)
}

temp_sd("Belgium")

combined_std_t_4A2b2 <- function(country){
  c = (combined$t_4A2b2-temp_mean(country))/temp_sd(country)
  return(c)
}

combined_std_t_4A2b2("Belgium")

# third task item
temp_mean <- function(country){
  col_name <- paste0("share_", country)
  a <- wtd.mean(combined$t_4A4a1, combined[[col_name]])
  return(a)
}
temp_mean("Belgium")

temp_sd <- function(country){
  col_name <- paste0("share_", country)
  b = wtd.var(combined$t_4A4a1, combined[[col_name]]) %>% sqrt()
  return(b)
}

temp_sd("Belgium")

combined_std_t_4A4a1 <- function(country){
  c = (combined$t_4A4a1-temp_mean(country))/temp_sd(country)
  return(c)
}

combined_std_t_4A4a1("Belgium")

# The next step is to calculate the `classic` task content intensity, i.e.
# how important is a particular general task content category in the workforce
# Here, we're looking at non-routine cognitive analytical tasks, as defined
# by David Autor and Darron Acemoglu:

combined$Belgium_NRCA <- combined_std_t_4A2a4("Belgium") + combined_std_t_4A2b2("Belgium") + combined_std_t_4A4a1("Belgium") 


# And we standardize NRCA in a similar way. 

temp_mean <- wtd.mean(combined$Belgium_NRCA, combined$share_Belgium)
temp_sd <- wtd.var(combined$Belgium_NRCA, combined$share_Belgium) %>% sqrt()
combined$std_Belgium_NRCA = (combined$Belgium_NRCA-temp_mean)/temp_sd

# Finally, to track the changes over time, we have to calculate a country-level mean
# Step 1: multiply the value by the share of such workers.

combined$multip_Belgium_NRCA <- (combined$std_Belgium_NRCA*combined$share_Belgium)

# Step 2: sum it up (it basically becomes another weighted mean)

agg_Belgium <-aggregate(combined$multip_Belgium_NRCA, by=list(combined$TIME),
                        FUN=sum, na.rm=TRUE)

# We can plot it now!
plot(agg_Belgium$x, xaxt="n")
axis(1, at=seq(1, 40, 3), labels=agg_Belgium$Group.1[seq(1, 40, 3)])


# If this code gets automated and cleaned properly,
#  you should be able to easily add other countries as well as other tasks.
# E.g.:

# Routine manual
# 4.A.3.a.3	Controlling Machines and Processes
# 4.C.2.d.1.i	Spend Time Making Repetitive Motions
# 4.C.3.d.3	Pace Determined by Speed of Equipment

