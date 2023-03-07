# Check for 'remotes'
if (!requireNamespace("remotes", quietly = TRUE)){
  install.packages("remotes")
}

# Install 'amt' from CRAN
install.packages("amt", dependencies = TRUE)
# Note: you may want to include 'build_vignettes = TRUE' above

# Install R-INLA
# https://www.r-inla.org/download-install
install.packages("INLA", 
                 repos = c(getOption("repos"),
                           INLA="https://inla.r-inla-download.org/R/stable"), 
                 dependencies = TRUE)
# INLA suggests:
if (!requireNamespace("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
}
BiocManager::install(c("graph", "Rgraphviz"), dependencies = TRUE)

# Install (or update) all other packages
packs <- c("raster",
           "sf",
           "tidyverse",
           "lubridate",
           "maptools",
           "survival",
           "TwoStepCLogit",
           "glmmTMB",
           "tictoc",
           "conflicted",
           "broom")

install.packages(packs, dependencies = TRUE)
