# for loop to generate data.all
data.all=as_tibble()
for (i in 1:9){
  intermediate=read_excel("Data\\Eurostat_employment_isco.xlsx", sheet = paste0('ISCO',i))
  intermediate$occupation=i
  assign(paste0('isconew',i),intermediate)
  data.all=rbind(data.all,intermediate)
}

# for loop to generate all country_total and country_share
for (country in country.name){
  intermediate=isconew1[country]+isconew2[country]+isconew3[country]+isconew4[country]+
            isconew5[country]+isconew6[country]+isconew7[country]+isconew8[country]+isconew9[country]
  total=rbind(intermediate,intermediate,intermediate,intermediate,intermediate,intermediate,intermediate,intermediate,intermediate)
  data.all[paste0(country,'_total')]=total
  data.all[paste0(country,'_share')]=data.all[country]/total
}

View(data.all)


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

combined <- left_join(data.all, aggdata, by = c("occupation" = "isco08_1dig"))
View(combined)
# Traditionally, the first step is to standardise the task values using weights 
# defined by share of occupations in the labour force. This should be done separately
# for each country. Standardisation -> getting the mean to 0 and std. dev. to 1.
# Let's do this for each of the variables that interests us:

#install.packages("Hmisc")
library(Hmisc)

temp_mean <- wtd.mean(combined$t_4A2a4, combined$share_Belgium)
temp_sd <- wtd.var(combined$t_4A2a4, combined$share_Belgium) %>% sqrt()
combined$std_Belgium_t_4A2a4 = (combined$t_4A2a4-temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$t_4A2a4, combined$share_Poland)
temp_sd <- wtd.var(combined$t_4A2a4, combined$share_Poland) %>% sqrt()
combined$std_Poland_t_4A2a4 = (combined$t_4A2a4-temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$t_4A2a4, combined$share_Spain)
temp_sd <- wtd.var(combined$t_4A2a4, combined$share_Spain) %>% sqrt()
combined$std_Spain_t_4A2a4 = (combined$t_4A2a4-temp_mean)/temp_sd

# second task item
temp_mean <- wtd.mean(combined$t_4A2b2, combined$share_Belgium)
temp_sd <- wtd.var(combined$t_4A2b2, combined$share_Belgium) %>% sqrt()
combined$std_Belgium_t_4A2b2 = (combined$t_4A2b2-temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$t_4A2b2, combined$share_Poland)
temp_sd <- wtd.var(combined$t_4A2b2, combined$share_Poland) %>% sqrt()
combined$std_Poland_t_4A2b2 = (combined$t_4A2b2-temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$t_4A2b2, combined$share_Spain)
temp_sd <- wtd.var(combined$t_4A2b2, combined$share_Spain) %>% sqrt()
combined$std_Spain_t_4A2b2 = (combined$t_4A2b2-temp_mean)/temp_sd

# third task item
temp_mean <- wtd.mean(combined$t_4A4a1 , combined$share_Belgium)
temp_sd <- wtd.var(combined$t_4A4a1 , combined$share_Belgium) %>% sqrt()
combined$std_Belgium_t_4A4a1  = (combined$t_4A4a1 -temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$t_4A4a1 , combined$share_Poland)
temp_sd <- wtd.var(combined$t_4A4a1 , combined$share_Poland) %>% sqrt()
combined$std_Poland_t_4A4a1  = (combined$t_4A4a1 -temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$t_4A4a1 , combined$share_Spain)
temp_sd <- wtd.var(combined$t_4A4a1 , combined$share_Spain) %>% sqrt()
combined$std_Spain_t_4A4a1  = (combined$t_4A4a1 -temp_mean)/temp_sd

# The next step is to calculate the `classic` task content intensity, i.e.
# how important is a particular general task content category in the workforce
# Here, we're looking at non-routine cognitive analytical tasks, as defined
# by David Autor and Darron Acemoglu:

combined$Belgium_NRCA <- combined$std_Belgium_t_4A2a4 + combined$std_Belgium_t_4A2b2 + combined$std_Belgium_t_4A4a1 
combined$Poland_NRCA <- combined$std_Poland_t_4A2a4 + combined$std_Poland_t_4A2b2 + combined$std_Poland_t_4A4a1 
combined$Spain_NRCA <- combined$std_Spain_t_4A2a4 + combined$std_Spain_t_4A2b2 + combined$std_Spain_t_4A4a1 

# And we standardise NRCA in a similar way.
temp_mean <- wtd.mean(combined$Belgium_NRCA, combined$share_Belgium)
temp_sd <- wtd.var(combined$Belgium_NRCA, combined$share_Belgium) %>% sqrt()
combined$std_Belgium_NRCA = (combined$Belgium_NRCA-temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$Poland_NRCA, combined$share_Poland)
temp_sd <- wtd.var(combined$Poland_NRCA, combined$share_Poland) %>% sqrt()
combined$std_Poland_NRCA = (combined$Poland_NRCA-temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$Spain_NRCA, combined$share_Spain)
temp_sd <- wtd.var(combined$Spain_NRCA, combined$share_Spain) %>% sqrt()
combined$std_Spain_NRCA = (combined$Spain_NRCA-temp_mean)/temp_sd

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

