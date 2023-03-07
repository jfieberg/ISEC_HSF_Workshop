#' ##  Habitat selection of otters
#'  Coded example for an SSF analysis 
#'  Authors: S. Muff, J. Signer, J. Fieberg

#'  The otter data are taken from Weinberger et al (2016). Flexible habitat selection paves the way for a recovery of otter populations in the European Alps, Biological Conservation 199, p. 88-95, https://doi.org/10.1016/j.biocon.2016.04.017

# Load libraries and read in data
#+warning=FALSE, message=FALSE
library(survival)
library(tidyverse)
library(glmmTMB)
library(broom)
library(tictoc)

dat <-  read.csv("https://raw.githubusercontent.com/jfieberg/ISEC_HSF_Workshop/main/Data/d_otter.csv")

#' ## Part 1: Data wrangling  

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

#' Scale and center the two continuous variables river width (Sohlenbrei) and step length (Breaks_Dis)
dat$Sohlenbrei <- scale(dat$Sohlenbrei)
dat$Breaks_Dis <- scale(dat$Breaks_Dis)

#' Add numerical variable for animals:
dat$ANIMAL_ID <- as.numeric(as.factor(dat$NA_ANIMAL))

#' Stratum ID is given as "NA_ID" in the data; 
#' It is easier to have sequential enumeration, so let's generate a new stratum-ID variable str_ID:
d.map <- data.frame(NA_ID=unique(dat$NA_ID),str_ID=1:length(unique(dat$NA_ID)))
dat$str_ID <- d.map[match(dat$NA_ID,d.map$NA_ID),"str_ID"]
dat <- dat[order(dat$str_ID),]

#' Getting to know the data better:
str(dat)


#' ## Part 4: Two-step approach 

#' We will use two approaches here: 

#' 1. Using a for loop.
#' 2. Using list columns.


#' For loops are more intuitive, but can become a bit tedious, especially when
#' more than two grouping factors should be considered.


#' The basic idea, however, is always the same. We want to fit for each animal
#' separately the same model (`Loc ~ STAU1 + REST1 + Sohlenbrei +  Breaks_Dis +
#' strata(str_ID)`) and then extract coefficients. 

# For loops are more intuitive, but can become a bit tedious, especially when
# more than two grouping factors should be considered.


# The basic idea, however, is always the same. We want to fit for each animal
# separatly the same model (`Loc ~ STAU1 + REST1 + Sohlenbrei +  Breaks_Dis +
# strata(str_ID)`) and then extract coefficients. 


#' ### Using a `for`-loop ----

#' We first want to know the name of all animals that are in the data set.
unique(dat$NA_ANIMAL)

 
# Lets fit the model to one animal (e.g., Alena)
head(dat)
m1 <- clogit(Loc ~ STAU1 + REST1 + Sohlenbrei +  Breaks_Dis + strata(str_ID), 
       data = dat[dat$NA_ANIMAL == "Alena", ])

#' To get a summary of all coefficients, we use the `tidy()` function from the
# package `broom`.
tidy(m1)

#' But we are interested in *all* animals, so we need to iterate over all animals
# and make sure we save the results.
res <- list()
for (i in unique(dat$NA_ANIMAL)) {
  m <- clogit(Loc ~ STAU1 + REST1 + Sohlenbrei +  Breaks_Dis + strata(str_ID), 
              data = dat[dat$NA_ANIMAL == i, ])
  res[[i]] <- tidy(m)
}

res

#' Next, we want to compare the coefficients for different animals. We have to bind the individual results together first.
bind_rows(res)

#' We might want to know the name of the animal too.
coefs0 <- bind_rows(res, .id = "name")

#' We will explore later, how to do summarize these results. But first, we will
#' explore the use of list columns to do the same.

#' ### Using list columns 

dat.n <- dat %>% nest(data = -c(NA_ANIMAL))
dat.n$NA_ANIMAL[4]
dat.n$data[[4]]

#' We could add as many grouping variables as we want to `-c(NA_ANIMAL)` to make
#' the model more complex.

#' Each element of the list `data` is a `data.frame` with the data for a given animal. 
dat.n$data[[4]]

#' This makes it easy to fit an individual model for each animal
dat.n <- dat.n %>% 
  mutate(
    ssf = map(data, ~ 
                clogit(Loc ~ STAU1 + REST1 + Sohlenbrei +  Breaks_Dis + strata(str_ID), 
                       data = .x) %>% tidy()))

dat.n <- dat.n %>% 
  mutate(
    ssf = lapply(data, function(.x) 
                clogit(Loc ~ STAU1 + REST1 + Sohlenbrei +  Breaks_Dis + strata(str_ID), 
                       data = .x) %>% tidy()))
dat.n
coefs1 <- dat.n %>% select(NA_ANIMAL, ssf) %>% unnest(cols = ssf)

 
# Ensure the two approaches lead to the same result (which they do). 
plot(coefs0$estimate, coefs1$estimate)
              
#' ##  Working with the results 

#' We could now work with the coefficients and for example plot males vs. females
#' or some other individual property. Here we will only calculate the mean and a
#' CI for the mean.

#' We use a bootstrap for the mean
coefs1
pop <- coefs1 %>% nest(dat = -term) %>% 
  mutate(
    mean = map_dbl(dat, ~ mean(.x$estimate)), 
    boot = map(dat, ~ replicate(1e4, mean(sample(.x$estimate, replace = TRUE)))), 
    lci = map_dbl(boot, ~ quantile(.x, probs = 0.025)),
    uci = map_dbl(boot, ~ quantile(.x, probs = 0.975)))

#' We can use this data to create a plot

coefs0 %>% 
  ggplot(aes(term, estimate)) + 
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(alpha = 0.3) +
  geom_pointrange(aes(ymin = lci, ymax = uci, y = mean), 
                  data = pop, alpha = 0.9, col = "red") +
  theme_light() 

#' ### Compare with glmmTMB() 

#' Now let us compare our results to what we got using glmmTMB

tic()
glmm.TMB.random = glmmTMB(Loc ~ -1 + STAU1 + REST1 + Sohlenbrei +  
                            Breaks_Dis + (1|str_ID) + 
                            (0 + STAU1 | ANIMAL_ID) + 
                            (0 + REST1 | ANIMAL_ID) + 
                            (0 + Breaks_Dis | ANIMAL_ID) + 
                            (0 + Sohlenbrei | ANIMAL_ID),
                          family=poisson, data = dat,
                          map = list(theta = factor(c(NA, 1:4))),
                          start = list(theta = c(log(1e3), 0, 0, 0, 0))
)
toc()

summary(glmm.TMB.random)

#' Combine results and plot
pop2 <- pop %>% select(term, mean, lci, uci) %>%
  rename(Estimate = mean, Term = term)
mixediSSF <- confint(glmm.TMB.random)
mixedcoef <- data.frame(Term = pop2$Term, 
                        Estimate = mixediSSF[1:4, 3],
                        lci = mixediSSF[1:4, 1],
                        uci = mixediSSF[1:4, 2])
pop2 <- rbind(pop2, mixedcoef)
pop2$method<-rep(c("Individual Fit", "Mixed iSSF"), each=4)

coefs0 <- coefs0 %>% rename(Term = term, Estimate=estimate)
ggplot(coefs0, aes(Term, Estimate)) + 
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_pointrange(aes(ymin = lci, ymax = uci, y = Estimate, col = method), 
                   data=pop2, alpha = 0.9, position = position_dodge2(width=0.2)) +
  theme_light() 

