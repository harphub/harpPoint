# harpPoint v0.2.1

* HOTFIX released on 2nd February 2024

* Fixes a bug that caused errors in `check_obs_against_fcst()` for deterministic
forecasts

* Fixes bug that meant `lag_forecast()` and `shift_forecast()` did not accept 
the harp >= v0.2.0 column naming convention

# harpPoint v0.2.0

This is a major update. Most of the changes are internal, but there are some 
changes that may lead to problems with downstream scripts that are listed 
below along with some selected new features.

### Breaking changes

* Default `groupings` argument changed from `"leadtime"` to `"lead_time"` for 
consistency with changes in _{harpIO}_. Scripts that use verification functions
with `"leadtime"`, `"validdate"` and `"fcdate"` as values to `groupings` should 
be changed to `"lead_time"`, `"valid_dttm"` and `"fcst_dttm"` respectively. 

* Verification outputs now use the column name `fcst_model` instead of `mname` 
so that there is consistency throughout harp. 

* Attributes for verification outputs have changed to include all forecast dates,
all stations and all groupings used in the verification. Any scripts that make 
use of these attributes should be updated to reflect the new attributes.

* `scale_point_forecast()` and `scale_point_obs()` are deprecated. 
`scale_param()` should be used instead. 

* `gather_members()` and `spread_members()` are deprecated. `pivot_members()`
should be used instead. 

* `first_validdate()` and `last_validdate()` are deprecated. 
`unique_valid_dttm()` should be used instead.

* `pull_stations()` is deprecated. `unique_stations()` should be used instead.

* `bootstrap_score()`, `pooled_bootstrap_score()` and `bind_bootstrap_score()` 
are defunct. `bootstrap_verify()` and `bind_point_verif()` should be used 
instead. 


### Selected new features

* Verification functions have gained new progress bars and are generally less 
verbose in what they are doing, restricting messages to only progress with 
computing scores for different verification groups. 

* New verification score __hexbin__. This gives a data frame of what is 
essentially a heat map of forecast - observation value pairs. 

* New class, attributes and print method for verification function outputs. 

* `jitter_fcst()` now accepts vectorized functions so should be a lot faster. 


# harpPoint v0.0.9

* This is the version that is basically unchanged since late 2021 / early 2022. 

* It was officially tagged v0.0.9 in November 2023
