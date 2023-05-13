Sys.setenv(LANG = "en")
# Install and load the rvest package
# install.packages("rvest")
library(rvest)

# Function to scrape the season description from Wikipedia
scrape_season_description <- function(season_num) {
  # Create the Wikipedia URL for the corresponding season
  wikipedia_url <- paste0("https://en.wikipedia.org/wiki/Game_of_Thrones_(season_", season_num, ")")
  
  # Scrape the description from the Wikipedia page
  description <- read_html(wikipedia_url) %>%
    html_nodes("#mw-content-text .mw-parser-output > p") %>%
    html_text() %>%
    .[1]
  
  return(description)
}

# Define the path to your Quarto Markdown file
qmd_file_path <- "C:/Users/rafal/Downloads/Studia_II_rok/Drugi_semestr/Reproducible_research/GIT_PULL_RR_repo/Quarto and MD 3/assignment_class_3_v4.qmd"

# Define the output directory
output_dir <- "C:/Users/rafal/Downloads/Studia_II_rok/Drugi_semestr/Reproducible_research/GIT_PULL_RR_repo/output"

# Create the output directory if it doesn't exist
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Change the working directory to the output directory
setwd(output_dir)

# Loop over all seasons
for (i in 1:8) {
  # Define the output file name
  output_file <- paste0("Game_of_Thrones_Analysis_Season_", i, ".pdf")
  
  # Create a new environment for each iteration
  env <- new.env()
  
  # Assign the season number and description to the environment
  env$season_num <- i
  env$season_desc <- scrape_season_description(i)
  
  # Render the document with the current environment
  rmarkdown::render(qmd_file_path, 
                    output_format = "pdf_document", 
                    output_file = output_file, 
                    envir = env)
}
