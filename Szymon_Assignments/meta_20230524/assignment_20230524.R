library(meta)
library(dplyr)

data <- readxl::read_xlsx("../../13. Metaanalysis/data/metaanalysis_data.xlsx")

m.raw <- metacont(n.e=N_boys,
                  mean.e=Mean_boys_play_male,
                  sd.e=SD_boys_play_male,
                  n.c=N_girls,
                  mean.c=Mean_girls_play_male,
                  sd.c=SD_girls_play_male,
                  data=data,
                  studlab=paste(Study),
                  comb.fixed = TRUE,
                  comb.random = TRUE,
)
m.raw

m.raw %>% forest(sortvar=TE)