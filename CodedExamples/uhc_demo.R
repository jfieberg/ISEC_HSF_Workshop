#################################################################X
#--------ISEC Workshop 5: Space-use and Habitat Selection--------X
#-----------------Fieberg, Signer, Smith, & Muff-----------------X
#--------------------------26 June 2022--------------------------X
#---------------------UHC Plots in 'amt' Demo--------------------X
#################################################################X

# We currently have an implementation of UHC plots in 'amt' for objects of class 
# 'fit_logit' and 'glm' (HSFs) and 'fit_clogit' (iSSFs). 

# The approach for both models is similar, making use of generic functions with 
# appropriate methods depending on the class.

# We do not have an implmentation for models fit with 'glmmTMB' or 'INLA'.

#For either an HSF or (i)SSF, the workflow is:
#   1. Prepare data for analysis
#   2. Split data into **training** and **testing** datasets.
#   3. Use **training** dataset to fit candidate model(s) [HSF or (i)SSF].
#   4. Prepare UHC plots by passing the fitted model and the **testing** data 
#       to the function `prep_uhc()`.
#   5. Create the UHC plots by calling `plot()` on the object returned in (4).

# We will illustrate the approach with example data from 'amt'.

# Load packages ----
library(amt)
library(raster)
library(sf)
library(dplyr)
library(ggplot2)

# Data ----
## Covariate rasters
data(uhc_hab)

# Affect habitat selection in HSF
plot(uhc_hab[[1:4]])

# Affect habitat selection in iSSF
plot(uhc_hab[[c(1:4, 6)]])

# Do not affect habitat selection in simulation
plot(uhc_hab[[c(5, 7)]])

## Location data
# Data simulated under the HSF model
data(uhc_hsf_locs)
# Data simulated under the iSSF model
data(uhc_issf_locs)

# HSF ----
# ... 1. & 2. prepare & split data ----
# Split into train (80%) and test (20%)
set.seed(1)
uhc_hsf_locs$train <- rbinom(n = nrow(uhc_hsf_locs),
                             size = 1, prob = 0.8)
train <- uhc_hsf_locs[uhc_hsf_locs$train == 1, ]
test <- uhc_hsf_locs[uhc_hsf_locs$train == 0, ]

# Available locations (entire raster extent)
avail_train <- st_bbox(uhc_hab) %>% 
  st_as_sfc() %>% 
  st_as_sf() %>% 
  random_points(n = nrow(train) * 50)

avail_test <- st_bbox(uhc_hab) %>% 
  st_as_sfc() %>% 
  st_as_sf() %>% 
  random_points(n = nrow(test) * 50)

# Combine with used
train_dat <- train %>%
  make_track(x, y, crs = 32612) %>%
  mutate(case_ = TRUE) %>%
  bind_rows(avail_train) %>%
  # Attach covariates
  extract_covariates(uhc_hab) %>%
  # Factor cover
  mutate(cover = factor(cover,
                        levels = 1:3,
                        labels = c("grassland", "forest", "wetland"))) %>%
  # Assign large weights to available
  mutate(weight = case_when(
    case_ ~ 1,
    !case_ ~ 5000
  ))

test_dat <- test %>%
  make_track(x, y, crs = 32612) %>%
  mutate(case_ = TRUE) %>%
  bind_rows(avail_test) %>%
  # Attach covariates
  extract_covariates(uhc_hab) %>%
  # Factor cover
  mutate(cover = factor(cover,
                        levels = 1:3,
                        labels = c("grassland", "forest", "wetland")))

# Note 'weight' column not created for test data
# (we assume all variables in test are candidate habitat variables)

# ... 3. fit model ----
# Here, we will fit two models. The "wrong" model has a different formulation
# than the model used to generate the data. The "right" model has the same
# formulation as the model used to generate the data.

# Wrong model
# Leave out quadratic term for 'temp' and leave out 'cover'
hsf_wrong <- glm(case_ ~ forage + temp + pred,
                 data = train_dat, family = binomial(), weights = weight)

# Right model
hsf_right <- glm(case_ ~ forage + temp + I(temp^2) + pred + cover,
                 data = train_dat, family = binomial(), weights = weight)

# ... 4. prepare UHC plots ----
# The function 'prep_uhc()' does most of the heavy lifting (*i.e.*, the 
# boostrapping).

# Prep under wrong model
system.time({ # takes < 30 sec
  uhc_hsf_wrong <- prep_uhc(object = hsf_wrong, test_dat = test_dat,
                            n_samp = 1000, verbose = TRUE)
})

# Prep under right model
system.time({ # takes < 30 sec
  uhc_hsf_right <- prep_uhc(object = hsf_right, test_dat = test_dat,
                            n_samp = 1000, verbose = TRUE)
  
})

# ... 5. plot ----
# The generic function 'plot()' will recognize the class of the prepped data
# and know what to do.

# Recall, a mismatch between the gray envelope and the black observed habitat
# is what indicates poor calibration.

# The red shows what is available, and a mismatch between used (black) and
# available (red) is what habitat selection looks like.

# The wrong model
plot(uhc_hsf_wrong)

# The right model
plot(uhc_hsf_right)

# iSSF ----
# ... 1. & 2. prepare & split data ----
# Format as steps
steps <- uhc_issf_locs %>%
  make_track(x, y, t, crs = 32612) %>%
  steps()

# Split into train (80%) and test (20%)
set.seed(1)
steps$train <- rbinom(n = nrow(steps),
                      size = 1, prob = 0.8)
train <- steps[steps$train == 1, ]
test <- steps[steps$train == 0, ]

# Generate available steps, attribute
train_dat <- train %>%
  random_steps(n_control = 15) %>%
  # Attach covariates
  extract_covariates(uhc_hab) %>%
  # Factor cover
  mutate(cover = factor(cover,
                        levels = 1:3,
                        labels = c("grassland", "forest", "wetland"))) %>%
  # Additional movement parameters
  mutate(log_sl_ = log(sl_),
         cos_ta_ = cos(ta_)) %>%
  # Drop 'train' column
  dplyr::select(-train) %>% 
  # Get rid of any NAs (sometimes available steps fall outside of raster)
  na.omit()

test_dat <- test %>%
  random_steps(n_control = 15) %>%
  # Attach covariates
  extract_covariates(uhc_hab) %>%
  # Factor cover
  mutate(cover = factor(cover,
                        levels = 1:3,
                        labels = c("grassland", "forest", "wetland"))) %>%
  # Additional movement parameters
  mutate(log_sl_ = log(sl_),
         cos_ta_ = cos(ta_)) %>%
  # Drop 'train' column
  dplyr::select(-train) %>% 
  # Get rid of any NAs (sometimes available steps fall outside of raster)
  na.omit()

# ... 3. fit model ----
issf_wrong <- fit_issf(train_dat, 
                       case_ ~ 
                         # Habitat
                         forage + 
                         # Movement
                         sl_ + log_sl_ + cos_ta_ +
                         # Strata
                         strata(step_id_), model = TRUE)

issf_right <- fit_issf(train_dat, 
                       case_ ~ 
                         # Habitat
                         forage + temp + I(temp^2) + pred + cover + dist_to_cent +
                         # Movement
                         sl_ + log_sl_ + cos_ta_ +
                         # Strata
                         strata(step_id_), model = TRUE)

# ... 4. prepare UHC plots ----
# Prep under wrong model
system.time({ # takes ~ 35 sec
  uhc_issf_wrong <- prep_uhc(object = issf_wrong, test_dat = test_dat,
                             n_samp = 100, verbose = TRUE)
})

# Prep under right model
system.time({ # takes ~ 35 sec
  uhc_issf_right <- prep_uhc(object = issf_right, test_dat = test_dat,
                             n_samp = 100, verbose = TRUE)
})

# ... 5. plot ----

plot(uhc_issf_wrong)

plot(uhc_issf_right)

# Working with 'uhc_data' objects ----
# Structure of `uhc_data` object
str(uhc_issf_right, 1)

# Coerce to data.frame
head(as.data.frame(uhc_issf_right), 10)

# This gives you the benefit of making custom plots, for example, with
# ggplot2
as.data.frame(uhc_issf_right) %>% 
  filter(var == "forage") %>% 
  mutate(dist_sort = factor(dist, levels = c("S", "U", "A"))) %>%
  ggplot(aes(x = x, y = y, color = dist_sort, linetype = dist_sort)) +
  geom_line() +
  scale_color_manual(name = "Distribution",
                     breaks = c("S", "U", "A"),
                     labels = c("Sampled", "Used", "Avail"),
                     values = c("gray70", "black", "red")) +
  scale_linetype_manual(name = "Distribution",
                        breaks = c("S", "U", "A"),
                        labels = c("Sampled", "Used", "Avail"),
                        values = c("solid", "solid", "dashed")
                        ) +
  xlab("Forage (g/m2)") +
  ylab("Density")

