# ISEC HSF Workshop
This repository contains data, code, and presentations associated with the ISEC workshop: Advances in quantifying space-use and habitat-selection of animals.

**Presenters**: John Fieberg, University of Minnesota, USA; Johannes Signer, University of Göttingen, Germany; Brian Smith, Utah State University, USA, Stefanie Muff, Norwegian University of Science and Technology, Norway.
 
**ABSTRACT**:
We will illustrate strategies for fitting habitat-selection models (resource-selection and step-selection functions, RSFs and SSFs, respectively) to data from multiple tagged individuals, highlighting three recent developments:
1. Integrated Step-Selection Functions (iSSFs), a simple framework for simultaneous modeling animal movement and habitat selection processes using conditional logistic regression (Avgar et al. 2016, Fieberg et al. 2021). These models allow one to relax the assumption that movement characteristics (i.e. step lengths and turn angles) are independent of habitat features.
2. The amt (animal movement tools) package in R, which provides tools for exploratory analysis of animal location data, functions for data development prior to fitting RSFs or SSFs, and a simple tidyverse workflow for seamless fitting of RSF and SSF models to data from individual animals (Signer et al. in 2019).
3. Methods for efficient estimation of mixed-effect RSFs and SSFs using INLA and the glmmTMB package (Muff et al. 2020).
 
We will include a mix of lectures and hands on applications (model fitting in R). A preliminary schedule and list of topics are given below:
1. Short introduction to Resource-Selection Functions and their connection to an Inhomogeneous Poisson Process (lecture).
2. Introduction to Step-Selection and Integrated Step-Selection Functions (lecture, coded example).
3. Methods for modeling data from multiple individuals (lecture, coded examples)
4. Simulating movements from fitted integrated step-selection functions (lecture, coded example).
5. Validating step-selection functions (lecture, coded example).

## Software

### R and RStudio

Please make sure you have [installed `R` version >4.2.0](https://cran.r-project.org/). We also recommend the latest version of [RStudio Desktop](https://www.rstudio.com/products/rstudio/download/). 

To build packages from source, you will need additional build tools; see details [here for Windows](https://cran.r-project.org/bin/windows/Rtools/) or [here for macOS](https://mac.r-project.org/tools/). 

*Note* that if you are upgrading to R 4.2 from a previous version on Windows, you will need RTools 4.2 as well. Your previous RTools installation will not be sufficient.

### R Packages
*The script* `packages.R` *in this repository can help you install and/or update the packages required for this workshop.*

We will be highlighting the use of the `amt` package for fitting habitat selection functions (HSFs) and integrated step selection functions (iSSFs). You need to install the latest version from GitHub (which will require the additional build tools we referenced above). Assuming you have installed the R package `remotes`, you can install `amt` from GitHub like this:

```
remotes::install_github("jmsigner/amt", dependencies = TRUE)
```

We will be using several other packages during the workshop. You can install (or update) all of them by running the code in `packages.R`. We strongly recommend that you upgrade to the most up-to-date versions of all these packages.
