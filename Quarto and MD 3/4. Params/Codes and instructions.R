# Rendering
library(quarto)

# quarto_render("QMD_class_3_4.qmd", execute_params = list(
#   year = 2017,
#   region = "Asia",
#   printcode = FALSE,
#   data = "file.csv"
# ))

ss <- c(1:8)
for (i in 1:8) {
  data_array[i] <- paste0("../Data/season_", i, ".RData")
}

for (i in 1:8) {
  quarto_render("Assignment.qmd", execute_params = list(
   season = ss[i],
   data = data_array[i],
   printcode = FALSE
   #data = "file.csv"
), output_file = paste0("Report-GOT_season_", ss[i], ".html"))
}

# Name change

# reg <- "Asia"
# y <- 2049

# quarto_render("QMD_class_3_4.qmd", execute_params = list(
#   season = y,
#   region = reg,
#   printcode = FALSE,
#   data = "file.csv"
# ), output_file = paste0("Report-", reg, "-", y, ".html"))


