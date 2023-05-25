library(quarto)
y <- 1

quarto_render("Assignment.qmd", execute_params = list(season = 1, test = 2), 
              output_file = paste0("Report-", "-", y, ".html"))


