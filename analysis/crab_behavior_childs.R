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
         total_aggression=sum_aggressive_posture+sum_fights_initiated+sum_fights_responded+sum_claw_strikes,
         standardized_mussels_eaten=sum_mussels_eaten/video_total,
         standardized_mussels_handled=sum_mussels_handled/video_total,
         standardized_total_aggression=total_aggression/video_total)

## Activity Distribution per Crab - SEE EXAPMLE FOR SAVING PLOT - JC
example <- ggplot(data_complete, aes(x = reorder(metadat_id, activity_proportion), y = activity_proportion, color=as.factor(crab_dens)))+geom_col()+
  labs(title="Activity Distribution per Crab", x="Meta ID", y="Activity Proportion")

ggsave("example_for_jeff.png", plot = example, device ="png", path = here("figures"))
remove(example)

#5) Figures Per Crab
## Density vs. Activity Plot
ggplot(data_complete, aes(as.factor(crab_dens), activity_proportion))+geom_boxplot()+
  labs(title="Density vs. Activity per Crab", x="Crab Density", y="Activity Proportion")

## Density vs. Total Aggression Plot
ggplot(data_complete, aes(as.factor(crab_dens), total_aggression))+geom_boxplot()+
  labs(title="Density vs. Aggression per Crab", x="Crab Density", y="Total Aggression")            

## Total Aggression vs. Mussels Handled
ggplot(data_complete, aes(x=total_aggression, y=sum_mussels_handled, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Aggression vs. Mussels Handled per Crab", x="Total Agression", y="Mussels Handled", color="Crab Density")

## Activity vs. Total Agression Plot
ggplot(data_complete, aes(x=activity_proportion, y=total_aggression, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Activity vs. Aggression per Crab", x="Activity Proportion", y="Total Aggression", color="Crab Density")

## Activity vs. Standardized Total Agression Plot
ggplot(data_complete, aes(x=activity_proportion, y=standardized_total_aggression, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Activity vs. Standardized Aggression per Crab", x="Activity Proportion", y="Standardized Total Aggression", color="Crab Density")

## Activity vs. Mussles Eaten Plot
ggplot(data_complete, aes(x=activity_proportion, y=sum_mussels_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Activity vs. Mussels Eaten per Crab", x="Activity Proportion", y="Mussels Eaten", color="Crab Density")

## Activity vs. Standardized Mussles Eaten Plot
ggplot(data_complete, aes(x=activity_proportion, y=standardized_mussels_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Activity vs. Standardized Mussels Eaten per Crab", x="Activity Proportion", y="Standardized Mussels Eaten", color="Crab Density")

## Activity vs. Mussels Handled
ggplot(data_complete, aes(x=activity_proportion, y=sum_mussels_handled, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Activity vs. Mussels Handled per Crab", x="Activity Proportion", y="Mussels Handled", color="Crab Density")

## Activity vs. Standardized Mussels Handled
ggplot(data_complete, aes(x=activity_proportion, y=standardized_mussels_handled, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Activity vs. Standardized Mussels Handled per Crab", x="Activity Proportion", y="Standardized Mussels Handled", color="Crab Density")

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
         total_aggression=sum_aggressive_posture+sum_fights_initiated+sum_fights_responded+sum_claw_strikes,
         standardized_mussels_eaten=sum_mussels_eaten/video_total,
         standardized_mussels_handled=sum_mussels_handled/video_total,
         standardized_total_aggression=total_aggression/video_total,
         standardized_prey_eaten=prey_eaten/video_total)

#7) Figures per Trial
## Density vs. Activity Plot
ggplot(data_complete_trial, aes(as.factor(crab_dens), activity_proportion))+geom_boxplot()+
  labs(title="Density vs. Activity per Trial", x="Crab Density", y="Activity Proportion")

## Density vs. Total Aggression Plot
ggplot(data_complete_trial, aes(as.factor(crab_dens), total_aggression))+geom_boxplot()+
  labs(title="Density vs. Aggression per Trial", x="Crab Density", y="Total Aggression")

## Activity vs. Total Aggression Plot
ggplot(data_complete_trial, aes(x=activity_proportion, y=total_aggression, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Activity vs. Aggression per Trial", x="Activity Proportion", y="Total Aggression", color="Crab Density")

## Activity vs. Standardized Total Aggression Plot
ggplot(data_complete_trial, aes(x=activity_proportion, y=standardized_total_aggression, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Activity vs. Standardized Aggression per Trial", x="Activity Proportion", y="Standardized Total Aggression", color="Crab Density")

## Total Aggression vs. Mussels Eaten (during video trial)
ggplot(data_complete_trial, aes(x=total_aggression, y=sum_mussels_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Aggression vs. Mussels Eaten per Trial", x="Total Agression", y="Mussels Eaten", color="Crab Density")

## Total Aggression vs. Mussels Eaten (during entire trial)
ggplot(data_complete_trial, aes(x=total_aggression, y=prey_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Aggression vs. Mussels Eaten per Trial", x="Total Agression", y="Mussels Eaten", color="Crab Density")

## Total Aggression vs. Mussels Handled
ggplot(data_complete_trial, aes(x=total_aggression, y=sum_mussels_handled, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Aggression vs. Mussels Handled per Trial", x="Total Agression", y="Mussels Handled", color="Crab Density")

## Activity vs. Mussels Handled
ggplot(data_complete_trial, aes(x=activity_proportion, y=sum_mussels_handled, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Activity vs. Mussels Handled per Trial", x="Activity Proportion", y="Mussels Handled", color="Crab Density")

## Activity vs. Standardized Mussels Handled
ggplot(data_complete_trial, aes(x=activity_proportion, y=standardized_mussels_handled, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Activity vs. Standardized Mussels Handled per Trial", x="Activity Proportion", y="Standardized Mussels Handled", color="Crab Density")

## Activity vs. Mussles Eaten (during video trial) Plot
ggplot(data_complete_trial, aes(x=activity_proportion, y=sum_mussels_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Activity vs. Mussels Eaten per Trial", x="Activity Proportion", y="Mussels Eaten", color="Crab Density")

## Activity vs. Standardized Mussles Eaten (during video trial) Plot
ggplot(data_complete_trial, aes(x=activity_proportion, y=standardized_mussels_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Activity vs. Standardized Mussels Eaten per Trial", x="Activity Proportion", y="Standardized Mussels Eaten", color="Crab Density")

## Activity vs. Mussles Eaten (during entire trial) Plot 
ggplot(data_complete_trial, aes(x=activity_proportion, y=prey_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Activity vs. Mussels Eaten per Trial", x="Activity Proportion", y="Mussels Eaten", color="Crab Density")

## Activity vs. Standardized Mussles Eaten (during entire trial) Plot 
ggplot(data_complete_trial, aes(x=activity_proportion, y=standardized_prey_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Activity vs. Standardized Mussels Eaten per Trial", x="Activity Proportion", y="Mussels Eaten", color="Crab Density")

#7) Trial Average per Crab
df4 <- data_complete %>%
  group_by(crab_id) %>%
  summarize(avg_aggressive_posture = mean(sum_aggressive_posture),
            avg_fights_initiated = mean(sum_fights_initiated),
            avg_fights_responded = mean(sum_fights_responded),
            avg_claw_strikes = mean(sum_claw_strikes),
            avg_total_aggression = mean(total_aggression),
            avg_mussels_handled = mean(sum_mussels_handled),
            avg_mussels_eaten = mean(sum_mussels_eaten),
            avg_activity_proportion = mean(activity_proportion),
            avg_standardized_mussels_eaten = mean(standardized_mussels_eaten),
            avg_standardized_mussels_handled = mean(standardized_mussels_handled),
            avg_standardized_total_aggression = mean(standardized_total_aggression))
data_complete_average <- right_join(df4, df2)

## Density vs. Mean Activity Plot
ggplot(data_complete_average, aes(as.factor(crab_dens), avg_activity_proportion))+geom_boxplot()+
  labs(title="Density vs. Mean Activity per Crab", x="Crab Density", y="Mean Activity Proportion")

## Density vs. Mean Total Aggression Plot
ggplot(data_complete_average, aes(as.factor(crab_dens), avg_total_aggression))+geom_boxplot()+
  labs(title="Density vs. Mean Aggression per Crab", x="Crab Density", y="Mean Total Aggression")            

## Mean Total Aggression vs. Mussels Handled
ggplot(data_complete_average, aes(x=avg_total_aggression, y=avg_mussels_handled, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Mean Aggression vs. Mussels Handled per Crab", x="Mean Total Agression", y="Mean Mussels Handled", color="Crab Density")

## Mean Activity vs. Total Agression Plot
ggplot(data_complete_average, aes(x=avg_activity_proportion, y=avg_total_aggression, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Mean Activity vs. Aggression per Crab", x="Mean Activity Proportion", y="Mean Total Aggression", color="Crab Density")

## Mean Activity vs. Standardized Total Agression Plot
ggplot(data_complete_average, aes(x=avg_activity_proportion, y=avg_standardized_total_aggression, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Mean Activity vs. Standardized Aggression per Crab", x="Mean Activity Proportion", y="Mean Standardized Total Aggression", color="Crab Density")

## Mean Activity vs. Mussles Eaten Plot
ggplot(data_complete_average, aes(x=avg_activity_proportion, y=avg_mussels_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Mean Activity vs. Mussels Eaten per Crab", x="Mean Activity Proportion", y="Mean Mussels Eaten", color="Crab Density")

## Mean Activity vs. Standardized Mussles Eaten Plot
ggplot(data_complete_average, aes(x=avg_activity_proportion, y=avg_standardized_mussels_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Mean Activity vs. Standardized Mussels Eaten per Crab", x="Mean Activity Proportion", y="Mean Standardized Mussels Eaten", color="Crab Density")

## Mean Activity vs. Mussels Handled
ggplot(data_complete_average, aes(x=avg_activity_proportion, y=avg_mussels_handled, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Mean Activity vs. Mussels Handled per Crab", x="Mean Activity Proportion", y="Mean Mussels Handled", color="Crab Density")

## Mean Activity vs. Standardized Mussels Handled
ggplot(data_complete_average, aes(x=avg_activity_proportion, y=avg_standardized_mussels_handled, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Mean Activity vs. Standardized Mussels Handled per Crab", x="Mean Activity Proportion", y="Mean Standardized Mussels Handled", color="Crab Density")

