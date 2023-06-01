data <- readxl::read_excel('.\\data\\metaanalysis_data.xlsx')
library(meta)
library(metafor)

m <- metagen(TE=data$Mean_boys_play_male,
             seTE=data$SD_boys_play_male,
             data=data,
             # studlab=paste(Author),
             comb.fixed = TRUE,
             comb.random = FALSE)

library(dplyr)

m %>% forest(sortvar=data$Mean_boys_play_male)
