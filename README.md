
<!-- README.md is generated from README.Rmd. Please edit that file -->

# harpPoint <img src='man/figures/harp_logo_dark.svg' align="right" width = "80" />

<!-- badges: start -->
<!-- badges: end -->

harpPoint provides functionality for the verification of meteorological
data at geographic points. Typically this would be the verification of
forecasts interpolated to the locations of weather stations. Functions
are provided for computing verification scores for both deterministic
and ensemble forecasts. In addition, confidence intervals for scores, or
the differences between scores for different forecast models can be
computed using bootstrapping.

## Installation

You can install harpPoint from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("harphub/harpPoint")
```

## Verification

harpPoint functions for verification are designed to work with data read
in using functions from [harpIO](https://harphub.github.io/harpIO). This
means `harp_df` data frames and `harp_lists`. These data must include a
column for observations against which the forecasts should be verified.

There are two main functions for verification:

[`det_verify()`](https://harphub.github.io/harpPoint/reference/det_verify.html) -
for deterministic forecasts;

[`ens_verify()`](https://harphub.github.io/harpPoint/reference/ens_verify.html) -
for ensemble forecasts.

Both of these functions will output a `harp_verif` list. This is a list
of data frames with scores separated out into summary scores and
threshold scores. Threshold scores are computed when thresholds are
provided to the functions and are computed for probabilities of
threshold exceedance.

## Deterministic scores

`det_verify()` computes scores with the following column names:

### *Summary scores*

- **bias** - The mean difference between forecasts and observations
- **rmse** - The root mean squared error
- **mae** - The mean of the absolute error
- **stde** - The standard deviation of the error
- **hexbin** - A heat map of paired hexagonal bins of forecasts and
  observations

### *Threshold scores*

- **cont_tab** - A contingency table of forecast hits, misses, false
  alarms and correct rejections
- **threat_score** - The ratio of hits to the sum of hits, misses and
  false alarms
- **hit_rate** - The ratio of hits to the sum of hits and misses
- **miss_rate** - The ratio of misses to the sum of hits and misses
- **false_alarm_rate** - The ratio of false alarms to the sum of false
  alarms and correct rejections
- **false_alarm_ratio** - The ratio of false alarms to the sum of false
  alarms and hits
- **heidke_skill_score** - The fraction of correct forecasts after
  eliminating those forecasts that would be correct purely due to random
  chance
- **pierce_skill_score** - 1 - miss rate - false alarm rate
- **kuiper_skill_score** - How well forecasts separates hits from false
  alarms
- **percent_correct** - The ratio of the sum of hits and correct
  rejections to the total number of cases
- **frequency_bias** - The ratio of the sum of hits and false alarms to
  the sum of hits and misses
- **equitable_threat_score** - How well the forecast measures hits
  accounting for hits due to pure chance
- **odds_ratio** - The ratio of the product of hits and correct
  rejections to the product of misses and false alarms
- **log_odds_ratio** - The sum of the logs of hits and correct
  rejections minus the sum of the logs of misses and false alarms.
- **odds_ratio_skill_score** - The ratio of the product of hits and
  correct rejections minus the product of misses and false alarms to the
  product of hits and correct rejections plus the product of misses and
  false alarms
- **extreme_dependency_score** - The ratio of the difference between the
  logs of observations climatology and hit rate to the sum of the logs
  of observations climatology and hit rate
- **symmetric_eds** - The symmetric extreme dependency score, which is
  the ratio of the difference between the logs of forecast climatology
  and hit rate to the sum of the logs of forecast climatology and hit
  rate
- **extreme_dependency_index** - The ratio of the difference between the
  logs of false alarm rate and hit rate to the sum of the logs of false
  alarm rate and hit rate
- **symmetric_edi** - The symmetric extreme dependency index, which is
  the ratio of the sum of the difference between the logs of false alarm
  rate and hit rate and the difference between the logs of the inverse
  hit rate and false alarm rate to the sum of the logs of hit rate,
  false alarm rate, inverse hit rate and inverse false alarm rate. Here
  the inverse is 1 - the value.

## Ensemble scores

`ens_verify()` computes scores with the following column names:

### *Summary scores*

- **mean_bias** - The mean difference between the ensemble mean of
  forecasts and observations
- **rmse** - The root mean squared error
- **stde** - The standard deviation of the error
- **spread** - The square root of the mean variance of the ensemble
  forecasts
- **hexbin** - A heat map of paired hexagonal bins of forecasts and
  observations
- **rank_histogram** - Observation counts ranked by ensemble member bins
- **crps**- The cumulative rank probability score - the difference
  between the cumulative distribution of the ensemble forecasts and the
  step function of the observations
- **crps_potential** - The crps that could be achieved with a perfectly
  reliable ensemble
- **crps_reliability** - Measures the ability of the ensemble to produce
  a cumulative distribution with desired statisical properties.
- **fair_crps** - The crps that would be achieved for either an ensemble
  with an infinite number of members, or for a number of members
  provided to the function

### *Threshold scores*

- **brier_score** - The mean of the squared error of the ensemble in
  probability space
- **fair_brier_score** - The Brier score that would be achieved for
  either an ensemble with an infinite number of members, or for a number
  of members provided to the function
- **brier_skill_score** - The Brier score compared to that of a
  reference probabilistic forecast (usually the observed climatology)
- **brier_score_reliability** - A measure of the ensemble’s ability to
  produce reliable (forecast probability = observed frequency) forecasts
- **brier_score_resolution** - A measure of the ensemble’s ability to
  discriminate between “on the day” uncertainty and climatological
  uncertainty
- **brier_score_uncertainty** - The inherent uncertainty of the events
- **reliability** - The frequency of observations for bins of forecast
  probability
- **roc** - The relative operating characteristic of the ensemble - the
  hit rates and false alarm rates for forecast probability bins
- **roc_area** - The area under a roc curve - summarises the ability of
  the ensemble to discriminate between events and non events
- **economic_value** - The relative improvement in economic value of the
  forecast compared to climatology for a range of cost / loss ratios

## Getting gridded data to points

For interpolation of gridded data to points see the [Interpolate
section](https://harphub.github.io/harpIO/articles/transformations.html#interpolate)
of the [Transforming model
data](https://harphub.github.io/harpIO/articles/transformations.html)
article on the [harpIO website](https://harphub.github.io/harpIO), or
the documentation for
[geo_points](https://harphub.github.io/harpCore/reference/geo_points.html).
