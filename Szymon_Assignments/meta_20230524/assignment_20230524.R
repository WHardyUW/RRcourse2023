library(meta)
library(dplyr)

data <- readxl::read_xlsx("../../13. Metaanalysis/data/metaanalysis_data.xlsx")

meta_boys <- metacont(n.e=N_boys,
                  mean.e=Mean_boys_play_male,
                  sd.e=SD_boys_play_male,
                  n.c=N_boys,
                  mean.c=Mean_boys_play_female,
                  sd.c=SD_boys_play_female,
                  data=data,
                  studlab=paste(Study),
                  comb.fixed = TRUE,
                  comb.random = TRUE,
)

meta_girls <- metacont(n.e=N_girls,
                      mean.e=Mean_girls_play_male,
                      sd.e=SD_girls_play_male,
                      n.c=N_girls,
                      mean.c=Mean_girls_play_female,
                      sd.c=SD_girls_play_female,
                      data=data,
                      studlab=paste(Study),
                      comb.fixed = TRUE,
                      comb.random = TRUE,
)

meta_boys %>% forest(sortvar=TE)

meta_girls %>% forest(sortvar=TE)


contour_levels <- c(0.90, 0.95, 0.99)
contour_colors <- c("darkblue", "blue", "lightblue")

meta_boys %>% funnel(contour = contour_levels, col.contour = contour_colors)
legend("topright", c("p < 0.10", "p < 0.05", "p < 0.01"), bty = "n", fill = contour_colors)

meta_girls %>% funnel(contour = contour_levels, col.contour = contour_colors)
legend("topright", c("p < 0.10", "p < 0.05", "p < 0.01"), bty = "n", fill = contour_colors)


meta_boys %>% metareg(~ `Parent present`)

