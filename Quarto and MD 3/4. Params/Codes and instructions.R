# Rendering
library(quarto)
quarto_render("QMD_class_3_4.qmd", execute_params = list(
  year = 2017,
  region = "Asia",
  printcode = FALSE,
  data = "file.csv"
))

hh <- c()

for (i in 1:8) {
  data_array <- paste0("D:/Quantitative/RR_lab/RR_lab4_1/Data/season_", i, ".RData")
  quarto_render("Assignment.qmd", execute_params = list(
    season = i, data = data_array), output_file = paste0("Report-GOT_season_", i, ".html"))

  }

# for (i in 1:8) {
# 
# }

# Name change

# reg <- "Asia"
# y <- 2049

# quarto_render("QMD_class_3_4.qmd", execute_params = list(
#   season = y,
#   region = reg,
#   printcode = FALSE,
#   data = "file.csv"
# ), output_file = paste0("Report-", reg, "-", y, ".html"))


