# Rendering
library(quarto)
library(stringr)

# create list for loop
listt = c("season 1", "season 2", "season 3", "season 4", "season 5", "season 6", "season 7", "season 8")

# loop over all seasons
for(season_ in listt){
  quarto_render("Assignment.qmd", execute_params = list(
    season = season_,
    data = paste0("Data/", "season_", str_split(season_, " ")[[1]][2], ".RData")
  ), output_file = paste0("Report-", "season_", str_split(season_, " ")[[1]][2], ".html"))
}
