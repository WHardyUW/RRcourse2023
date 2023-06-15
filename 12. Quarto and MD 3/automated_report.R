library(quarto)

seasons <- c(1:8)

for (season in seasons) {
  path <- paste0("../../Data/season_", season, ".csv")
  
  quarto_render("Assignment.qmd", execute_params = list(
    season = season,
    data = path
  ), output_file = paste0("report-season_", season, ".html"))
}