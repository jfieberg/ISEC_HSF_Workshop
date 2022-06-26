# ISEC HSF Workshop
This repository contains data, code, and presentations associated with the ISEC workshop: Advances in quantifying space-use and habitat-selection of animals.

**Presenters**: John Fieberg, University of Minnesota, USA; Johannes Signer, University of Göttingen, Germany; Brian Smith, Utah State University, USA, Stefanie Muff, Norwegian University of Science and Technology, Norway.
 
**ABSTRACT**:
We will illustrate strategies for fitting habitat-selection models (resource-selection and step-selection functions, RSFs and SSFs, respectively) to data from multiple tagged individuals, highlighting three recent developments:
1. Integrated Step-Selection Functions (iSSFs), a simple framework for simultaneous modeling animal movement and habitat selection processes using conditional logistic regression (Avgar et al. 2016, Fieberg et al. 2021). These models allow one to relax the assumption that movement characteristics (i.e. step lengths and turn angles) are independent of habitat features.
2. The amt (animal movement tools) package in R, which provides tools for exploratory analysis of animal location data, functions for data development prior to fitting RSFs or SSFs, and a simple tidyverse workflow for seamless fitting of RSF and SSF models to data from individual animals (Signer et al. in 2019).
3. Methods for efficient estimation of mixed-effect RSFs and SSFs using INLA and the glmmTMB package (Muff et al. 2020).

## Schedule

We will include a mix of lectures and hands on applications (model fitting in R). A preliminary schedule with list of topics is provided below:

9:00-9:10 Introductory remarks, housekeeping (schedule, materials, etc)    
9:10-9:45 Introduction to resource-selection functions, species distribution models (John)  
9:45-10:15 Step-selection functions and integrated step-selection functions (John)   
10:15-10:45 Break/Q&A   
10:45-11:45 Introduction to the amt package, coding resource and step-selection functions (Brian)    
11:45-1:15 Lunch Break   
1:15-2:00 Modeling among-animal variability using 2-step methods and mixed-effects models (John)   
2:00-3:00 INLA and coded mixed effect examples (Steffi and Johannes)   
3:00-3:15 Break/Q&A   
3:15-3:45 Simulating from fitted SSFs (Johannes)   
3:45-4:15 Validating SSFs using uhc plots (John)   
4:15-5:00 Validating SSFs using amt (Brian)


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


## Communication

We have created a Slack Workspace for the workshop where you can post questions or communicate with other participants. You can join the Workspace using [this link](https://join.slack.com/t/isec2022works-e1k8729/shared_invite/zt-1bf9lo8rl-YUmdRKxMbiJqDrmxI_BAgA).
