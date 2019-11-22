# test
rm(list=ls())

library(tidyverse)
library(here)

size <- read.csv(here("data/crab_size_data_extracted.csv"))
raw <- read.csv(here("data/crab_feeding_trial_data_dec2018.csv"))
crab <- read.csv(here("data/crab_video_trial_datasheet.csv"))
time <- read.csv(here("data/condensed_crab_video_trial_datasheet.csv"))

data <- tbl_df(crab)
df2 <- tbl_df(time)

df1 <- data %>%
  group_by(metadat_id) %>%
  summarize(sum_aggressive_posture = sum(aggressive_posture),
            sum_fights_initiated = sum(fights_initiated),
            sum_fights_responded = sum(fights_responded),
            sum_claw_strikes = sum(claw_strikes),
            sum_mussels_handled = sum(mussels_handled),
            sum_mussels_eaten = sum(mussels_eaten),
            sum_activity_subtotal = sum(activity_subtotal))

# Integrating video_total into data
joined <- left_join(df1, df2)

# Creating activity_proportion & total_aggression columns  
data_complete <- joined %>%
  group_by(metadat_id) %>%
  mutate(activity_proportion=sum_activity_subtotal/video_total,
         total_aggression=sum_aggressive_posture+sum_fights_initiated+sum_fights_responded+sum_claw_strikes)

# Figures
## Per Crab
ggplot(data_complete, aes(x=crab_dens, y=activity_proportion))+geom_point()

ggplot(data_complete, aes(x=crab_dens, y=total_aggression))+geom_point()

ggplot(data_complete, aes(x=activity_proportion, y=total_aggression, color=crab_dens))+geom_point()

ggplot(data_complete, aes(x=activity_proportion, y=sum_mussels_eaten, color=crab_dens))+geom_point()

# Per Trial
df3 <- data %>%
  group_by(trial_id) %>%
  summarize(sum_aggressive_posture = sum(aggressive_posture),
            sum_fights_initiated = sum(fights_initiated),
            sum_fights_responded = sum(fights_responded),
            sum_claw_strikes = sum(claw_strikes),
            sum_mussels_handled = sum(mussels_handled),
            sum_mussels_eaten = sum(mussels_eaten),
            sum_activity_subtotal = sum(activity_subtotal))
joined_trial <- right_join(df3, df2)
data_complete_trial <- joined_trial %>%
  group_by(trial_id) %>%
  mutate(activity_proportion=sum_activity_subtotal/video_total,
         total_aggression=sum_aggressive_posture+sum_fights_initiated+sum_fights_responded+sum_claw_strikes)

## Figures
ggplot(data_complete_trial, aes(x=crab_dens, y=activity_proportion))+geom_point()

ggplot(data_complete_trial, aes(x=crab_dens, y=total_aggression))+geom_point()

ggplot(data_complete_trial, aes(x=activity_proportion, y=total_aggression, color=crab_dens))+geom_point()

ggplot(data_complete_trial, aes(x=activity_proportion, y=sum_mussels_eaten, color=crab_dens))+geom_point()
