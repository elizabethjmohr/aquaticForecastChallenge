library(tidyverse)
library(streamMetabolizer)

# Note - this is not being used right now. Not sure if GPP, ER, and k600 explain
# much variation in daily mean DO relative to temperature.

# Fit stream metabolism model to POSE 102 data
model_specs <- mm_name(type='bayes', 
                       pool_K600='none', 
                       err_obs_iid=TRUE, 
                       err_proc_iid=TRUE, 
                       ) %>%
  specs(burnin_steps = 500, 
        saved_steps = 2000)

mm_102 <- metab(model_specs, data = DO_POSE_102)

paramEsts_102 <- get_params(mm_102) 

