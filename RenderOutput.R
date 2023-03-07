#' This file can be used to create html reports from
#' each of the files in the repository and move them 
#' to the output directory.
#' 
#' Use the here package to locate each of the files 
#' using a relative path.
library(here)
library(rmarkdown)
render(here("CodedExamples", "two_step_approach.R"), output_dir = here("Output"))
render(here("CodedExamples", "Otters_SSF.R"), output_dir = here("Output"))
render(here("CodedExamples", "amt_demo_iSSF.R"), output_dir = here("Output"))
