#' ---
#' title: "Integrated Step-Selection Analyses Using amt"
#' output:
#'   html_document: 
#'     toc: true
#'     toc_float: true
#' ---
  
#' Author: Brian J. Smith <brian.smith@usu.edu> (with minor 
#' modifications by John Fieberg)
#'

#' ## Preamble
#' Load packages 
#+ warning=FALSE, message=FALSE
library(amt)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(lubridate)

#' Other packages required but not attached:
#'   - maptools

#' Load GPS data (Demo data included in 'amt')
gps <- uhc_issf_locs

#' Load habitat data (Demo data included in 'amt')
hab <- uhc_hab

#' Convert to a raster
hab <- rast(uhc_hab, type = "xyz", crs = "epsg:32612")

#' Convert "cover" layer to factor
levels(hab[[4]]) <- data.frame(id = 1:3,
                               cover = c("grass", "forest", "wetland"))

#' Quick look
plot(hab)

#' ## Basics of 'amt'

#' The basic building block in the package `amt` is a `track_*` object.
#' For movement data, this is a `track_xyt` object, but locations without
#' timestamps are also possible (`track_xy`).

#' We can format our data as a `track_*` object using `make_track()`.
trk <- make_track(gps, x, y, t, crs = 32612)

#' We can see the class of the resulting object
class(trk)

#' This is still a tibble and a data.frame, so we can manipulate it just
#' like we would manipulate any other tibble or data.frame in R. But we
#' get some added benefits from it being a `track_xyt` object -- e.g.,
#' we have default plotting methods:
plot(trk)
lines(trk)

#' Or, on top of our raster:
plot(hab[[1]])
points(trk)
lines(trk)

#' ## iSSFs in 'amt' 

#' ### Creating steps
#' 
#' While the most basic building block in 'amt' is a track, iSSA requires
#' that we convert from a point representation to a step representation.

#' We can do that with the function `steps()`.
stp <- gps %>% 
  make_track(x, y, t, crs = 32612) %>% 
  steps()
# View(stp)

#' Note that steps have their own S3 class:
class(stp)

#' Data cleaning is an important first step whenever you're working with
#' GPS data. Unfortunately, we did not allocate time today to dive into
#' the details of data cleaning. We have data cleaning functions in 
#' 'amt', and we will soon have a new vignette with a demonstrated cleaning
#' workflow.

#' Once your data are cleaned, you will often have gaps in an otherwise
#' regular trajectory. iSSA requires a constant step duration, so we cannot
#' create steps across gaps in our data. 

#' To simulate gaps, we will randomly remove ~5% of the locations:
set.seed(12345)
trk2 <- gps %>% 
  make_track(x, y, t, crs = 32612) %>% 
  mutate(rm = as.logical(rbinom(n = nrow(.), size = 1, prob = 0.05))) %>% 
  filter(!rm)

#' Check
nrow(trk2)/nrow(gps)
print(trk2, n = 10)

#' Notice we removed our 10th location -- we have a 2h gap from 
#' 2021-02-15 08:00 to 2021-02-15 10:00. 
#'
#' What happens if we make steps, now?
stp2 <- steps(trk2)
# View(stp2)

#' We now have some steps where dt = 2 hours. Our trajectory is irregular, 
#' and that will not work for iSSA. We can filter out those 2 hour steps
#' fairly easily in this simplified example, but a more useful way to
#' deal with this is to use the function `track_resample()`.
#'
#' `track_resample()` takes the track, the desired duration, and a tolerance
#' around that duration as arguments, then it divides a trajectory into
#' "bursts" with a constant step duration. Note, this function doesn't do
#' any interpolation.
trk3 <- track_resample(trk2, rate = hours(1), tolerance = minutes(5))
print(trk3, n = 12)

#' We have a new column, `burst_`, that identifies our bursts. We can see the
#' switch from burst 1 to burst 2 after our gap at 10:00. If we pass that to
#' `steps()`, we now get a warning:
steps(trk3)

#' The warning tells us to use `steps_by_burst()` to make use of the new 
#' column:
stp3 <- steps_by_burst(trk3)
# View(stp3)

#' We no longer have any steps with 2-h durations, and we are ready to
#' proceed with our iSSA.
#'

#' ### Movement parameters and random steps
#' 
#' The next thing we need to do is generate random available steps from
#' a tentative parametric distribution. We will use the gamma distribution
#' to generate step lengths and a von Mises distribution to generate turn
#' angles. After fitting our iSSA, we will update these tentative 
#' distributions to the true, selection-free movement distributions.

#' A good way to decide on tentative parameters is to fit them to observed
#' steps. We can do that in `amt` like this:

#' gamma distribution (fit to step lengths)
(sldist <- fit_distr(stp3$sl_, "gamma"))

#' von Mises distribution (fit to turn angles)
(tadist <- fit_distr(stp3$ta_, "vonmises"))

#' We can then overlay these on the empirical step-length and turn angle
#' distributions to see how well they match.
ggplot(stp3, aes(x = sl_)) + 
  geom_histogram(aes(y = ..density..), binwidth = 2) +
  stat_function(fun =function(x)dgamma(x, shape = sldist$params$shape, 
                                        scale = sldist$params$scale), 
                 color = "red", lwd=1.5)

ggplot(stp3, aes(x = ta_)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.2) +
  stat_function(fun =function(x)circular::dvonmises(x, mu = 0, kappa = tadist$params$kappa), 
                color = "red", lwd=1.5)

#' We see that the turn angle distribution appears unimodal with modes at both
#' 0 and +/- $\pi$. This is fairly common and may occur when animals use linear
#' features or when locations are measured with error.
#' 
#' We can use the function `random_steps()` to generate our random steps.
#' When we pass a `steps_xyt` object, the default is for it to fit the
#' gamma and von Mises distributions, just as we did above.
set.seed(20220626 + 1)
obs_avail <- random_steps(stp3, n_control = 20)

#' Random steps have their own S3 class, but they are also still of
#' class `steps_xyt`.
class(obs_avail)

#' Take a look:
#' View(obs_avail)

#' 
#' Each observed step gets an identifying number (`step_id_`), and all of 
#' the paired available steps get that same ID. These IDs form the strata
#' in our conditional logistic regression. The variable `case_` is TRUE
#' for the observed step and `FALSE` for the available steps. This will
#' be the response variable in our conditional logistic regression.
#'
#' Note that the tentative step length and turn angle distributions are
#' attached to the object as attributes.
attributes(obs_avail)

#' You can also access them with these convenience functions:
sl_distr(obs_avail)
ta_distr(obs_avail)

#' Since we used the gamma distribution as our tentative step-length
#' distribution, we will want to include step length and log(step length)
#' in our iSSF.  Doing so will:
#' 
#' - correct for bias that arises from ignoring habitat selection when 
#' estimating the movement parameters in the tenative distributions 
#' (Forester et al. 2009)
#' - allow us to obtain updated/improved estimates of movement parameters 
#' that adjust for habitat selection

#' 
#' Since we used the von Mises distribution as our tentative turn-angle
#' distribution, we need to include cos(turn angle) in our iSSF.
#'

#' ### Extracting Covariates
#' 
#' Now that we have observed and available steps, we need to attach our
#' environmental covariates to each one. We can use the functions
#' 'extract_covariates()' to attach the raster values to our steps.
covs <- extract_covariates(obs_avail, hab)

#' Format "cover" as factor
covs$cover <- factor(covs$cover,
                     labels = c("grassland", "forest", "wetland"))

#' Have a look:
print(covs, n = 3, width = 200)

#' One of the great strengths of iSSA is that it can handle temporal 
#' variation. Perhaps our organism selects habitat differently between
#' day and night, or perhaps it moves with different speeds during day
#' or night. We can account for this with interactions in our iSSF,
#' and 'amt' has a function that extracts the time of day,
#' given the coordinates and the date. I.e., it accounts for different
#' sunset times at different latitudes on different days of the year.
covs2 <- time_of_day(covs, where = "both")

#' The argument 'where = "both"' tells the function to extract the time
#' of day for *both* the start of the step and the end of the step.
print(covs2, n = 3, width = 200)

#' Now we're ready to fit a model!
#'

#' ### Fitting a model 
#' 
#' Let's begin with a simple iSSF. We'll model our movement-free habitat
#' selection kernel as a function of forage and predation risk, and we'll 
#' include all the movement parameters to update the selection-free movement 
#' kernel.

#' Note that the `amt` function `fit_issf()` is just a wrapper for
#' survival::clogit(). For much more information on the implementation,
#' see the help file.
#?survival::clogit

m1 <- fit_issf(covs2, 
               # Response
               case_ ~ 
                 # Habitat
                 forage + pred +
                 # Movement
                 sl_ + log(sl_) + cos(ta_) + 
                 # Stratum
                 strata(step_id_),
               # Need this later for model predictions
               model = TRUE)

#' Let's have a look at the structure of our object.
str(m1, 1)

#' Notice that it is a list with 4 elements at the top level.
#'   - $model: the actual fitted model
#'   - $sl_: the tentative step-length distribution
#'   - $ta_: the tentative turn-angle distribution
#'   - $more: (currently empty) a placeholder for additional information

#' ... model interpretation ----

#' Take a look at the model summary:
summary(m1)

#' Main take-aways from this summary are:
#' 
#' 1. our animal selects for forage (+ coef)
#' 2. our animal avoids predation risk (- coef)
#' 3. our tentative step-length distribution is very close to the estimated 
#'       selection-free movement kernel (none of the sl_ params are significant.)
#' 4. our tentative turn-angle distribution is different from the estimated
#'       selection-free movement kernel (cos(ta_) param is significant)

#' With this simple model structure, we have an 'amt' function that will
#' automatically update the movement distributions.
#'
#' ### Updating movement parameters
#' 
#' Tentative step-length distribution
(tent_sl <- sl_distr(m1))
#' Updated selection-free step-length distribution
(upd_sl <- update_sl_distr(m1,  beta_sl = "sl_", beta_log_sl = "log(sl_)"))

#' Compile the parameters into a data.frame for plotting with ggplot
tent_df <- data.frame(dist = "tent",
                      shp = tent_sl$params$shape,
                      scl = tent_sl$params$scale)
upd_df <- data.frame(dist = "upd",
                     shp = upd_sl$params$shape,
                     scl = upd_sl$params$scale)

(sl_df <- rbind(tent_df, upd_df))

#' Plot
expand.grid(sl = seq(1, 1500, length.out = 100),
            dist = c("tent", "upd")) %>% 
  left_join(sl_df) %>% 
  mutate(y = dgamma(sl, shape = shp, scale = scl)) %>% 
  ggplot(aes(x = sl, y = y, color = dist)) +
  geom_line() +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  theme_bw()

#' We can see there is barely a difference between our tentative and updated
#' step-length distributions. This is no surprise, given that the betas for
#' sl_ and log(sl_) were not significant.

#' Tentative turn-angle distribution
(tent_ta <- ta_distr(m1))
#' Updated selection-free turn-angle distribution
(upd_ta <- update_ta_distr(m1, beta_cos_ta = "cos(ta_)"))

#' Compile the parameters into a data.frame for plotting with ggplot
tent_df_ta <- data.frame(dist = "tent",
                         mu = tent_ta$params$mu,
                         k = tent_ta$params$kappa)
upd_df_ta <- data.frame(dist = "upd",
                        mu = upd_ta$params$mu,
                        k = upd_ta$params$kappa)

#' Sometimes, your kappa may be negative. The von Mises is not defined for
#' negative kappa. Negative kappa indicates that the turns are not actually
#' concentrated around 0 (as we typically assume), but rather are concentrated
#' around +/- pi. If this happens, we can simply multiply the estimated
#'  kappa by -1 and change mu to pi.

#upd_df_ta <- data.frame(dist = "upd",
#                        mu = pi,
#                        k = -1 * upd_ta$params$kappa)

(ta_df <- rbind(tent_df_ta, upd_df_ta))

#' Plot
expand.grid(ta = seq(-pi, pi, length.out = 100),
            dist = c("tent", "upd")) %>% 
  left_join(ta_df) %>% 
  # circular::dvonmises is not vectorized
  rowwise() %>% 
  mutate(y = circular::dvonmises(ta, mu = mu, kappa = k)) %>% 
  ggplot(aes(x = ta, y = y, color = dist)) +
  geom_line() +
  xlab("Turn Angle (radians)") +
  ylab("Probability Density") +
  scale_x_continuous(breaks = c(-pi, -pi/2, 0, pi/2, pi),
                     labels = expression(-pi, -pi/2, 0, pi/2, pi)) +
  coord_cartesian(ylim = c(0, 0.2)) +
  theme_bw()

#' As we expected, we see a difference here.
#'



#' ### Log relative selection strength
#' 
#' We can use (log-)RSS to visualize the habitat selection part of the model.
#' We can use 'amt::log_rss()' to compare 2 (or more) locations, with one of the
#' locations serving as a reference location.

#' Both x1 and x2 are defined using data.frames and require all of the
#' covariates in the fitted model. x1 can be any number of rows you wish,
#' but to avoid unintended issues with R's recycling rules, we limit
#' x2 to be exactly 1 row.
#'

#' #### Scenario 1:
#' How much more likely is our animal to step into a habitat with 
#' forage = 600 g/m^2 than a habitat with forage = 400 g/m^2, assuming
#' both locations are equally accessible (i.e., equally far away from the
#' current location)?
x1 <- data.frame(forage = 600, pred = 0, 
                 sl_ = 50, ta_ = 0)

x2 <- data.frame(forage = 400, pred = 0, 
                 sl_ = 50, ta_ = 0)

logRSS <- log_rss(m1, x1 = x1, x2 = x2, ci = "se")

#' RSS
exp(logRSS$df[, c("log_rss", "lwr", "upr")])

#' Our animal is ~ 1.5x more likely to step into a habitat with forage = 600
#' than forage = 400.
#'
#'
#' #### Scenario 2:
#' How much more likely is our animal to step into a habitat with
#' predator = 0 predators/100 km^2 than predator = 5 predators/100 km^2,
#' given both observations are equally accessible?
x1 <- data.frame(forage = 600, pred = 0, 
                 sl_ = 50, ta_ = 0)

x2 <- data.frame(forage = 600, pred = 5, 
                 sl_ = 50, ta_ = 0)

#' We can also ask for confidence intervals
logRSS <- log_rss(m1, x1 = x1, x2 = x2, ci = "se", ci_level = 0.95)

#' RSS
exp(logRSS$df$log_rss)
#' Confidence interval
exp(c(logRSS$df$lwr, logRSS$df$upr))

#' Our animal is about 7.3x more likely to step into a habitat with
#' predator = 0 than predator = 5. The 95% CI for that estimate is
#' ~ 5.9 -- 9.0.
#'

#' #### Figure
#' If we want to use this to make a figure, we can pass a sequence
#' of values to x1. Remember, x2 must always be 1 row. Let's 
#' visualize the RSS for forage vs. mean forage.
x1 <- data.frame(forage = seq(0, 800, length.out = 100), 
                 pred = 0, 
                 sl_ = 50, ta_ = 0)

x2 <- data.frame(forage = mean(values(hab$forage)), 
                 pred = 0, 
                 sl_ = 50, ta_ = 0)

logRSS <- log_rss(m1, x1, x2, ci = "se", ci_level = 0.95)

#' Default plot
plot(logRSS)

#' But if we want more control, we can use ggplot with the 'df' data.frame.
ggplot(logRSS$df, aes(x = forage_x1, y = exp(log_rss), 
                      ymin = exp(lwr), ymax = exp(upr))) +
  geom_ribbon(color = "black", fill = "gray80", linetype = "dashed") +
  geom_line() +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  xlab("Forage at x1") +
  ylab("RSS vs. Mean Forage") +
  theme_bw()

