data <- readxl::read_xlsx("metaanalysis_data.xlsx")

meta_boys <- metacont(n.e = N_boys,
                       mean.e = Mean_boys_play_male,
                       sd.e = SD_boys_play_male,
                       n.c = N_boys,
                       mean.c = Mean_boys_play_female,
                       sd.c = SD_boys_play_female,
                       data = data,
                       comb.fixed = TRUE,
                       comb.random = TRUE)

meta_girls <- metacont(n.e = N_girls,
                      mean.e = Mean_girls_play_male,
                      sd.e = SD_girls_play_male,
                      n.c = N_girls,
                      mean.c = Mean_girls_play_female,
                      sd.c = SD_girls_play_female,
                      data = data)

meta_boys %>% forest(sortvar = TE)
meta_girls %>% forest(sortvar = TE)

meta_boys %>% funnel()

contour_levels <- c(0.90, 0.95, 0.99)
contour_colors <- c("darkblue", "blue", "lightblue")
funnel(meta_boys, contour = contour_levels, col.contour = contour_colors)
legend("bottomright", c("p < 0.10", "p < 0.05", "p < 0.01"), bty = "n", fill = contour_colors)
