# test
rm(list=ls())

library(tidyverse)
library(here)

size <- read.csv(here("data/crab_size_data_extracted.csv"))
raw <- read.csv(here("data/crab_feeding_trial_data_dec2018.csv"))
crab <- read.csv(here("data/crab_video_trial_datasheet.csv"))

data <- tbl_df(crab)

data %>%
  group_by(metadat_id) %>%
  summarize(sum_aggressive_posture = sum(aggressive_posture),
            sum_fights_initiated = sum(fights_initiated),
            sum_fights_responded = sum(fights_responded),
            sum_claw_strikes = sum(claw_strikes),
            sum_mussels_handled = sum(mussels_handled),
            sum_mussels_eaten = sum(mussels_eaten),
            sum_activity_subtotal = sum(activity_subtotal))
