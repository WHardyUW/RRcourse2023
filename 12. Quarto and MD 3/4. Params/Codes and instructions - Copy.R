# Rendering
library(quarto)

quarto_render("Assignment.qmd", execute_params = list(
  year = 2017,
  region = "Asia",
  printcode = FALSE,
  data = "file.csv"
))

# Name change

reg <- "Asia"
y <- 2049

for (y in 2017:2019){
  quarto_render("QMD_class_3_4.qmd", execute_params = list(
    year = y,
    region = reg,
    printcode = FALSE,
    data = "file.csv"
  ), output_file = paste0("Report-", reg, "-", y, ".html"))
}



for (s in 1:8){
  quarto_render("Assignment.qmd", execute_params = list(
   season = s 
  ), output_file = paste0("Report-", s, ".html"))
}

