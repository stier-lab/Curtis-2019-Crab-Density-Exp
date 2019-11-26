#1) Loading data
rm(list=ls())

library(tidyverse)
library(here)

size <- read.csv(here("data/crab_size_data_extracted.csv"))
raw <- read.csv(here("data/crab_feeding_trial_data_dec2018.csv"))
crab <- read.csv(here("data/crab_video_trial_datasheet.csv"))
time <- read.csv(here("data/condensed_crab_video_trial_datasheet.csv"))

data <- tbl_df(crab)
df2 <- tbl_df(time)

#2) Summarizing data so that there is 1 row per crab
df1 <- data %>%
  group_by(metadat_id) %>%
  summarize(sum_aggressive_posture = sum(aggressive_posture),
            sum_fights_initiated = sum(fights_initiated),
            sum_fights_responded = sum(fights_responded),
            sum_claw_strikes = sum(claw_strikes),
            sum_mussels_handled = sum(mussels_handled),
            sum_mussels_eaten = sum(mussels_eaten),
            sum_activity_subtotal = sum(activity_subtotal))

#3) Integrating Video Total into data
joined <- right_join(df1, df2)

#4) Creating Activity Proportion & Total Aggression columns  
data_complete <- joined %>%
  group_by(metadat_id) %>%
  mutate(activity_proportion=sum_activity_subtotal/video_total,
         total_aggression=sum_aggressive_posture+sum_fights_initiated+sum_fights_responded+sum_claw_strikes)

#5) Figures Per Crab
## Density vs. Activity Plot
ggplot(data_complete, aes(as.factor(crab_dens), activity_proportion))+geom_boxplot()+
  labs(title="Density vs. Activity per Crab", x="Crab Density", y="Activity Proportion")

## Density vs. Total Aggression Plot
ggplot(data_complete, aes(as.factor(crab_dens), total_aggression))+geom_boxplot()+
  labs(title="Density vs. Aggression per Crab", x="Crab Density", y="Total Aggression")

## Activity vs. Total Agression Plot
ggplot(data_complete, aes(x=activity_proportion, y=total_aggression, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Activity vs. Aggression per Crab", x="Activity Proportion", y="Total Aggression", color="Crab Density")

## Activity vs. Mussles Eaten Plot
ggplot(data_complete, aes(x=activity_proportion, y=sum_mussels_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Activity vs. Mussels Eaten per Crab", x="Activity Proportion", y="Mussels Eaten", color="Crab Density")

#6) Replicating steps 2-4 Per Trial
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

#7) Figures per Trial
## Density vs. Activity Plot
ggplot(data_complete_trial, aes(as.factor(crab_dens), activity_proportion))+geom_boxplot()+
  labs(title="Density vs. Activity per Trial", x="Crab Density", y="Activity Proportion")

## Density vs. Total Aggression Plot
ggplot(data_complete_trial, aes(as.factor(crab_dens), total_aggression))+geom_boxplot()+
  labs(title="Density vs. Aggression per Trial", x="Crab Density", y="Total Aggression")

## Activity vs. Total Agression Plot
ggplot(data_complete_trial, aes(x=activity_proportion, y=total_aggression, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Activity vs. Aggression per Trial", x="Activity Proportion", y="Total Aggression", color="Crab Density")

## Activity vs. Mussles Eaten Plot
ggplot(data_complete_trial, aes(x=activity_proportion, y=sum_mussels_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Activity vs. Mussels Eaten per Trial", x="Activity Proportion", y="Mussels Eaten", color="Crab Density")

