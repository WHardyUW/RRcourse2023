#### Metaanalysis assignment Maciej Zdanowicz

# import data
library(readxl)
metaanalysis_data <- read_excel("data/metaanalysis_data.xlsx")
View(metaanalysis_data)

# boys

# effects for boys for different kinds of toys
m.boys <- metacont(n.e=N_boys,
                  mean.e=Mean_boys_play_female,
                  sd.e=SD_boys_play_female,
                  n.c=N_boys,
                  mean.c=Mean_boys_play_male,
                  sd.c=SD_boys_play_male,
                  data=metaanalysis_data
)
m.boys


# plots

## forest
m.boys %>% forest(sortvar=TE)

## funnel
m.boys %>% funnel()

## funnel with colours
contour_levels <- c(0.90, 0.95, 0.99)
contour_colors <- c("darkblue", "blue", "lightblue")
funnel(m.boys, contour = contour_levels, col.contour = contour_colors)
legend("topright", c("p < 0.10", "p < 0.05", "p < 0.01"), bty = "n", fill = contour_colors)

# regression
m.boys %>% metareg(~ `Age (months)`)


# girls

# effects for girls for different kinds of toys
m.girls <- metacont(n.e=N_girls,
                   mean.e=Mean_girls_play_female,
                   sd.e=SD_girls_play_female,
                   n.c=N_girls,
                   mean.c=Mean_girls_play_male,
                   sd.c=SD_girls_play_male,
                   data=metaanalysis_data
)
m.girls

# plots

## forest
m.girls %>% forest(sortvar=TE)

## funnel
m.girls %>% funnel()

## funnel with colours
contour_levels <- c(0.90, 0.95, 0.99)
contour_colors <- c("darkblue", "blue", "lightblue")
funnel(m.girls, contour = contour_levels, col.contour = contour_colors)
legend("topright", c("p < 0.10", "p < 0.05", "p < 0.01"), bty = "n", fill = contour_colors)

m.girls %>% metareg(`mode of delivery` + `type of students`)

# regression
m.girls %>% metareg(~ `Age (months)`)

