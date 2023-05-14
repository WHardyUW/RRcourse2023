# Quarto #2
# YAML, rendering and parameters
# Reproducible Research 2023
# Wojciech Hardy
library(tidyverse)
library(dplyr)
library(quarto)

setwd("C:/Users/ChotC/Downloads/RRcourse2023-main/assignment_1205")

# Converting from Rmd to Qmd

## Step 1) 
knitr::convert_chunk_header(input = "RR2604_444135.Rmd", 
                            output = "RR2604_444135.qmd")

## Step 2)
readLines("RR2604_444135.qmd")[1:5]

readLines("RR2604_444135.qmd") %>%
  stringr::str_replace(
    pattern = "output: html_document", 
    replace = "format: html") %>%
  writeLines(con = "RR2604_444135.qmd")

readLines("RR2604_444135.qmd")[1:5]

# Launching a preview mode
sys::exec_wait("quarto preview RR2604_444135.qmd")

# To create a PDF file you need a TeX installation:
sys::exec_wait("quarto install tinytex")

# Rendering
library(quarto)

quarto_render("RR2604_444135.qmd", output_format = "docx")
