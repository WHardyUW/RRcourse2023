# Rendering
library(quarto)

for (i in 1:8) {

  quarto_render("Assignment.qmd", execute_params = list(
    season = i
  ), output_file = paste0("Season-", i, ".html"))

}
