
setwd ("C:\\Users\\as455398\\Desktop\\RRcourse2023-1\\Quarto and MD 3\\4. Params")

library(quarto)
quarto_render("QMD_class_3_4.qmd", execute_params = list(
  year = 2017,
  region = "Asia",
  printcode = FALSE,
  data = "file.csv"
))

regs <- c("Asia", "Europe")
years <- 2017:2019

for (i in 1:2){
  for (y in 2017:2019) {
    quarto_render("QMD_class_3_4.qmd", execute_params = list(
      year = y,
      region = regs[i],
      printcode = FALSE,
      data = "file.csv"
      ), output_file = paste0("Report-", regs[i], "-", y ".html"))
  }
}

reg <- "Asia"
y <- 2017


quarto_render("QMD_class_3_4.qmd", execute_params = list(
  year = y,
  region = reg,
  printcode = FALSE,
  data = "file.csv"
), output_file = paste0("Report-", reg, "-", y ".html""))







