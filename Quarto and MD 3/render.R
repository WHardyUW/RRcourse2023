# Load rmarkdown package
library(rmarkdown)

for(i in 1:8) {
  rmarkdown::render(
    "Assignment.qmd",
    params = list(season = i),
    output_file = paste0("GOT_Season_", i, "_Report.html")
  )
}