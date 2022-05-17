# ISEC_HSF_Workshop
This repository contains data, code, and presentations associated with the ISEC workshop:   Advances in quantifying space-use and habitat-selection of animals.

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
