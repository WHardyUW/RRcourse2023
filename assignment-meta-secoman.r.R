library(meta)
library(dplyr)

data <- readxl::read_xlsx("data\\metaanalysis_data.xlsx")

meta_boys = metacont(n.e = N_boys,
                     mean.e = Mean_boys_play_male,
                     sd.e = SD_boys_play_male,
                     n.c = N-boys,
                     mean.c = Mean_boys_play_female,
                     sd.c = SD_boys_play_female,
                     data=data,
                     comb.fixed  = TRUE,
                     comb.random = TRUE)
meta_girls = metacont(n.e = N_girls,
                      mean.e = Mean_girls_play_male,
                      sd.e = SD_girls_play_male,
                      n.c = N-boys,
                      mean.c = Mean_girls_play_female,
                      sd.c = SD_girls_play_female,
                      data=data,
                      comb.fixed  = TRUE,
                      comb.random = TRUE)


meta_boys %>% forest (sortvar=TE)
meta_boys %>% funnel ()
meta_boys %>% metareg (- 'country')