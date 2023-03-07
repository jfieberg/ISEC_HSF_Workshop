#' ##  Habitat selection of otters
#'  Coded example for an SSF analysis  
#'  
#'  Authors: S. Muff, J. Signer, J. Fieberg
#'
#'  This code replicates the analysis presented in Section 4.2 of Muff, Signer, Fieberg (2022). Accounting for individual‐specific variation in habitat‐selection studies: Efficient estimation of mixed‐effects models using Bayesian or frequentist computation, Journal of Animal Ecology, 89, p.80-92, https://doi.org/10.1111/1365-2656.13087
#'
#'  The code is adapted from our data and code repository: https://conservancy.umn.edu/handle/11299/204737
#'
#'  The otter data are taken from Weinberger et al (2016). Flexible habitat selection paves the way for a recovery of otter populations in the European Alps, Biological Conservation 199, p. 88-95, https://doi.org/10.1016/j.biocon.2016.04.017
#'
#' Load libraries and read in data
#+ warning=FALSE, message=FALSE
library(survival)
library(TwoStepCLogit)
library(INLA)
library(glmmTMB)
library(tictoc)
options(width=160)# setting output width in html report
dat <-  read.csv("https://raw.githubusercontent.com/jfieberg/ISEC_HSF_Workshop/main/Data/d_otter.csv")

head(dat)
str(dat)

########################################################################################
### Part 1: Data wrangling 
########################################################################################

#' NAT1, REST1 and STAU1 are the three factor levels of the factor variable "habitat type", encoded as dummy variables, where
#'
#' - NAT1: natural habitat (reference category)
#' - REST1: residual water
#' - STAU1: a reservoir

#' Further, the two continuous variables in the model are:
 
#' - Sohlbrei: the river width
#' - Breaks_Dis: step length
 
#' Finally, `Loc` is the binary response variable that indicates if a habitat point was used (1) or available (0).

#' Some data manipulation:

#' Add numerical variable for animals:
dat$ANIMAL_ID <- as.numeric(as.factor(dat$NA_ANIMAL))

#' Stratum ID is given as "NA_ID" in the data; 
#' It is easier to have sequential enumeration, so let's generate a new stratum-ID variable str_ID:
d.map <- data.frame(NA_ID=unique(dat$NA_ID),str_ID=1:length(unique(dat$NA_ID)))
dat$str_ID <- d.map[match(dat$NA_ID,d.map$NA_ID),"str_ID"]
dat <- dat[order(dat$str_ID),]

#' Scale and center the two continuous variables river width (Sohlenbrei) and step length (Breaks_Dis)
dat$Sohlenbrei <- scale(dat$Sohlenbrei)
dat$Breaks_Dis <- scale(dat$Breaks_Dis)

#' Getting to know the data better:
str(dat)
#' Note that str_ID is the stratum ID. In each stratum there is exactly one realized (Loc=1) and 9 available locations (Loc=0):
dat[1:30,]



########################################################################################
### Part 2: Fixed effects models
########################################################################################

##########################
### 2A) The "Cox trick"
##########################

#' Remember that the conditional logistic regression model is a special case of the Cox proportional hazard model. clogit() is a wrapper function for the respective formulation. 
 
#' You can check more info:
# ?clogit()

#' Fitting the model with habitat type (STAU1, REST1), river width and step length
r.clogit <- clogit(Loc ~ STAU1 + REST1 + Sohlenbrei + Breaks_Dis  
                   +   strata(str_ID), data=dat) 

summary(r.clogit)$coef

#' However, clogit() cannot handle random effects, that is, it is not possible to e.g. include animal-specific effetcs on habitat type or river width. This is where the "Poisson trick" from Muff et al. (2020) is coming into play.  


##########################
### 2B) The "Poisson trick"
##########################

## 2B.1) Bayesian approach: INLA

#' We have to set mean and precision for the priors of slope (beta) coefficients.
#' We choose a N(0,10^4) prior:
mean.beta <- 0
prec.beta <- 1e-4  # precision = 1/variance

#' The model formula for INLA, where we set the stratum-specific intercept variance to $10^6$ (or rather: the precision to $10^{-6}$), is given as follows (NOTE: We remove the intercept with -1!)
formula.fixed <-  Loc ~ -1 +  STAU1 + REST1 + Sohlenbrei +  Breaks_Dis +
  f(str_ID,model="iid",hyper=list(
    theta=list(
      initial=log(1e-6), # The prior is parameterized log(precision) = log(1/variance)
      fixed=T # Fix the initial value to be a "point prior"; 
      # if fixed=F, the variance would be estimated
      )
    )
  ) 

#' Then fit the Poisson model
r.inla.fixed <- inla(formula.fixed, family ="Poisson", data=dat,
                       control.fixed = list(
                         mean = mean.beta,
                         prec = list(default = prec.beta)
                       )
                )

#' The summary for the posterior distribution of the fixed effects:
r.inla.fixed$summary.fixed

#' Bonus! Thanks to the Bayesian approach, we can plot (marginal) posterior distributions. 
#' Example for River width and REST1:

plot(r.inla.fixed$marginals.fixed$Sohlenbrei,type="l",xlim=c(-1,0.7),lwd=2)
lines(r.inla.fixed$marginals.fixed$STAU1,col=2,lwd=2)
lines(r.inla.fixed$marginals.fixed$REST1,col=3,lwd=2)
legend("topleft",legend=c("River width", "Hab. Dam (STAU)", "Hab. Residual (REST)"),col=1:3,lwd=2)

#######################################################################

## 2B.2) Frequentist approach: glmmTMB
 
#' The approach in glmmTMB is to write down the model, but not fit it.

TMBStruc.fix = glmmTMB(Loc ~ -1 + STAU1 + REST1 + Sohlenbrei +
                         Breaks_Dis +  (1|str_ID), 
                       family=poisson, data=dat, doFit=FALSE) 

#' Then manually fix the (log of the) standard deviation of the first random term, which is the `(1|str_ID)` component  in the above model equation:
TMBStruc.fix$parameters$theta[1] = log(1e3) 

#' We need to tell glmmTMB not to change the variance by setting it to NA:
TMBStruc.fix$mapArg = list(theta=factor(c(NA)))

#' Then fit the model and look at the results:
glmm.TMB.fixed = glmmTMB:::fitTMB(TMBStruc.fix) 
summary(glmm.TMB.fixed)


#' Alternatively (maybe easier), there is a one-step way to carry out the regression with newer versions of glmmTMB:
glmm.TMB.fixed = glmmTMB(Loc ~ -1 + STAU1 + REST1 + Sohlenbrei +
                           Breaks_Dis +  (1|str_ID), 
                         family=poisson, data=dat, 
                         map=list(theta=factor(c(NA))),
                         start=list(theta=c(log(1e3))))

summary(glmm.TMB.fixed)


###' Bonus: Illustration why the intercept term should be removed. Rerun without "-1" in the formula, and compare the fixed effect estimates:

glmm.TMB.fixed.2 = glmmTMB(Loc ~  STAU1 + REST1 + Sohlenbrei +
                           Breaks_Dis +  (1|str_ID), 
                         family=poisson, data=dat, 
                         map=list(theta=factor(c(NA))),
                         start=list(theta=c(log(1e3))))

summary(glmm.TMB.fixed.2)


########################################################################################
### Part 3: Random effects models
########################################################################################

##########################
### 3A) Random effects SSFs using INLA
##########################
 
#' To fit the model with random slopes in INLA, we need to generate new (but identical) variables of individual ID (ID cannot be used multiple times in the model formula, that's a peculiarity of INLA):

dat$ANIMAL_ID1 <- dat$ANIMAL_ID
dat$ANIMAL_ID2 <- dat$ANIMAL_ID
dat$ANIMAL_ID3 <- dat$ANIMAL_ID

#' Set the model formula as for the fixed-effects model, but now add three random slope terms, namely for the two levels of the categorical variable (STAU1 and REST1) which are not the reference categories, and for river width (Sohlenbrei). The priors for precision of the three random slopes are PC(3,0.05), while the intercept variance is again fixed=T:

formula.random <- Loc ~  -1 + STAU1 + REST1 + Sohlenbrei +  
  Breaks_Dis +
  f(str_ID,model="iid",hyper=list(theta = list(initial=log(1e-6),fixed=T))) +
  f(ANIMAL_ID1,Sohlenbrei, model="iid",
    hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) + 
  f(ANIMAL_ID2,STAU1, model="iid",
    hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) + 
  f(ANIMAL_ID3,REST1, model="iid",
    hyper=list(theta=list(initial=log(1),fixed=F,prior="pc.prec",param=c(3,0.05)))) 

#' Fitting the model in inla
tic()
r.inla.random <- inla(formula.random, family ="Poisson", data=dat, 
                      control.fixed = list(
                        mean = mean.beta,
                        prec = list(default = prec.beta)
                      )
)
toc()

#' Remember the speed for later comparison with glmmTMB

#' The summary for the posterior distribution of the fixed effects:
r.inla.random$summary.fixed 

#' Since variances are parameterized and treated as precisions, the summary of the respective posterior distributions is given for the precisions:
r.inla.random$summary.hyperpar
 

#' Source R functions for calculating posterior means and medians of the precisions.
source("https://raw.githubusercontent.com/jfieberg/ISEC_HSF_Workshop/main/CodedExamples/inla_mmarginal.R")
source("https://raw.githubusercontent.com/jfieberg/ISEC_HSF_Workshop/main/CodedExamples/inla_emarginal.R")
inla_emarginal(r.inla.random)
inla_mmarginal(r.inla.random)

#' Again, we can look at the posterior marginals for some of the parameters:

plot(r.inla.random$marginals.fixed$Sohlenbrei,type="l",xlim=c(-1,0.7),lwd=2)
lines(r.inla.random$marginals.fixed$STAU1,col=2,lwd=2)
lines(r.inla.random$marginals.fixed$REST1,col=3,lwd=2)
legend("topleft",legend=c("River width", "Hab. Dam (STAU)", "Hab. Residual (REST)"),col=1:3,lwd=2)


#' Interesting to compare the posterior distributions from the fixed and random effects model:

par(mfrow=c(1,3))
plot(r.inla.fixed$marginals.fixed$Sohlenbrei,type="l",xlim=c(-1,0.7),lwd=2,main="River width")
lines(r.inla.random$marginals.fixed$Sohlenbrei,type="l",lwd=2,lty=2)
legend("topleft",legend=c("Fixed", "Random"),lwd=2,lty=1:2,cex=1.2)

plot(r.inla.fixed$marginals.fixed$STAU1,type="l",xlim=c(-1,0.7),lwd=2,col=2,main="Habitat Dam (STAU)")
lines(r.inla.random$marginals.fixed$STAU1,type="l",lwd=2,lty=2,col=2)

plot(r.inla.fixed$marginals.fixed$REST1,type="l",xlim=c(-1,0.7),lwd=2,col=3,main = "Habitat Residual (REST)")
lines(r.inla.random$marginals.fixed$REST1,type="l",lwd=2,lty=2,col=3)

#' The above plots confirm that 
#' - fixed-effects models underestimate uncertainty.
#' - the actual point estimates are biased.

##########################
### 3B) Random effects SSFs using glmmTMB
##########################

#' And finally, the same mixed effects model using glmmTMB(). 
#' As before, start to set up the model without fitting it. Independent random effects are added as  (0 + STAU1 | ANIMAL_ID), for animal-specific slopes of "STAU1", for example:

TMBStruc = glmmTMB(Loc ~ -1 + STAU1 + REST1 + Sohlenbrei +  
                     Breaks_Dis +  (1|str_ID) + 
                     (0 + STAU1 | ANIMAL_ID) + 
                     (0 + REST1 | ANIMAL_ID)   + 
                     (0 + Sohlenbrei | ANIMAL_ID), 
                   family=poisson, data=dat, doFit=FALSE) 

#' Now there is more than one variance parameter to be estimated:
TMBStruc$parameters$theta

#' Since (1|str_ID) is the first parameter in the formula, it is the one we have to fix.
#' Therefore, set the value of the (log of the) standard deviation of the first random effect (here (1|str_ID)) to the fixed value:
TMBStruc$parameters$theta[1] = log(1e3) 

#' Tell glmmTMB not to change the first standard deviation (NA), all other values are freely estimated (and are different from each other)
TMBStruc$mapArg = list(theta=factor(c(NA,1:3)))

#' Fit the model and look at the summary:
tic()
glmm.TMB.random <- glmmTMB:::fitTMB(TMBStruc)
toc()

#' Check the speed and compare to the speed of INLA.

summary(glmm.TMB.random)

#' 95\% CIs for fixed and random effects (standard deviations) are obtained via the confint() function:
confint(glmm.TMB.random)



#' Again, there is a one-step way to carry out the regression with newer versions of glmmTMB:

glmm.TMB.random = glmmTMB(Loc ~ -1 + STAU1 + REST1 + Sohlenbrei +  
                            Breaks_Dis +  (1|str_ID) + 
                            (0 + STAU1 | ANIMAL_ID) + 
                            (0 + REST1 | ANIMAL_ID)   + 
                            (0 + Sohlenbrei | ANIMAL_ID),
                          family=poisson, data=dat,
                          map=list(theta=factor(c(NA,1:3))),
                          start=list(theta=c(log(1e3),0,0,0))
)
 
######################################################
# A note for those using an older version of glmmTMB
######################################################


#' It it has been a problem in a previous version of glmmTMB that the confint() function only showed a table of the length equal to the number of parameters estimated. As the variance for str_ID was not estimated but fixed to 10^6, but was still listed, the last variance component (here the one for (0 + Sohlenbrei | ANIMAL_ID)) was not shown. If you face this problem, you can solve the issue by moving the component (1|str_ID) to the last position in the formula of the above code, and then use 
#'
#' `TMBStruc$parameters$theta[4] = log(1e3)` 
#' 
#' `TMBStruc$mapArg = list(theta=factor(c(1:3, NA)))`
