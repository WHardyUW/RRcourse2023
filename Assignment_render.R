# code adapted from course materials

library(quarto)

setwd("C:/Users/mz411968/Desktop/RR_3/RRcourse2023/Quarto and MD 2")

# Converting from Rmd to Qmd
knitr::convert_chunk_header(input = "Assignment.Rmd", 
                            output = "Assignment.qmd")


# Rendering
quarto_render("Assignment.qmd", output_format = "docx")
quarto_render("Assignment.qmd", output_format = "html")

# render file
quarto::quarto_render(input = "Assignment.qmd", #expecting a dir to render
                      output_format = "html")
