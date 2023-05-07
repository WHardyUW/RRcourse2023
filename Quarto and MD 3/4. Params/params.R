setwd(/Users/yuqingwu/Desktop/RR\ Study/20230330RR/rep0330/RRcourse2023/Quarto\ and\ MD\ 3/4.\ Params)

library (quarto)
quarto_render("QMD_class_3_4.qmd",execute_params = list(
  year = 2017, 
  region = "Asia", 
  printcode = FALSE, 
  data = "file.csv" 
))

regs <- c("Asia","Europe")
y<- 2017:2019

for (i in 1:2){
  for (y in 2017:2019){
    quarto_render("QMD_class_3_4.qmd",execute_params = list(
      year = y, 
      region = reg[i], 
      printcode = FALSE, 
      data = "file.csv" 
    ), output_file = paste0("report-",reg[i],"-",y,"html"))
  }
}

quarto_render("QMD_class_3_4.qmd",execute_params = list(
  year = y, 
  region = reg, 
  printcode = FALSE, 
  data = "file.csv" 
), output_file = paste0("Report-",reg[i],"-",y,"html"))