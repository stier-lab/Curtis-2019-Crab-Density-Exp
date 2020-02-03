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

## Activity Distribution per Crab
ggplot(data_complete, aes(x = reorder(metadat_id, activity_proportion), y = activity_proportion, color=as.factor(crab_dens)))+geom_col()+
  labs(title="Activity Distribution per Crab", x="Meta ID", y="Activity Proportion")

#5) Figures Per Crab
## Density vs. Activity Plot
fig_5.1 <- ggplot(data_complete, aes(as.factor(crab_dens), activity_proportion))+geom_boxplot()+
  labs(title="Density vs. Activity per Crab", x="Crab Density", y="Activity Proportion")

#ggsave("Density_vs_Activity_per_Crab.png", plot = fig_5.1, device ="png", path = here("figures"))
remove(fig_5.1)

## Density vs. Total Aggression Plot
fig_5.2 <- ggplot(data_complete, aes(as.factor(crab_dens), total_aggression))+geom_boxplot()+
  labs(title="Density vs. Aggression per Crab", x="Crab Density", y="Total Aggression")            

#ggsave("Density_vs_Aggression_per_Crab.png", plot = fig_5.2, device ="png", path = here("figures"))
remove(fig_5.2)

## Total Aggression vs. Mussels Handled
fig_5.3 <- ggplot(data_complete, aes(x=total_aggression, y=sum_mussels_handled, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Aggression vs. Mussels Handled per Crab", x="Total Agression", y="Mussels Handled", color="Crab Density")

#ggsave("Aggression_vs_Mussels_Handled_per_Crab.png", plot = fig_5.3, device ="png", path = here("figures"))
remove(fig_5.3)

## Activity vs. Total Agression Plot
fig_5.4 <- ggplot(data_complete, aes(x=activity_proportion, y=total_aggression, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Activity vs. Aggression per Crab", x="Activity Proportion", y="Total Aggression", color="Crab Density")

#ggsave("Activity_vs_Aggression_per_Crab.png", plot = fig_5.4, device ="png", path = here("figures"))
remove(fig_5.4)

## Activity vs. Standardized Total Agression Plot
fig_5.5 <- ggplot(data_complete, aes(x=activity_proportion, y=standardized_total_aggression, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Activity vs. Standardized Aggression per Crab", x="Activity Proportion", y="Standardized Total Aggression", color="Crab Density")

#ggsave("Activity_vs_Standardizd_Aggression_per_Crab.png", plot = fig_5.5, device ="png", path = here("figures"))
remove(fig_5.5)

## Activity vs. Mussles Eaten Plot
fig_5.6 <- ggplot(data_complete, aes(x=activity_proportion, y=sum_mussels_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Activity vs. Mussels Eaten per Crab", x="Activity Proportion", y="Mussels Eaten", color="Crab Density")

#ggsave("Activity_vs_Mussels_Eaten_per_Crab.png", plot = fig_5.6, device ="png", path = here("figures"))
remove(fig_5.6)

## Activity vs. Standardized Mussels Eaten Plot
fig_5.7 <- ggplot(data_complete, aes(x=activity_proportion, y=standardized_mussels_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Activity vs. Standardized Mussels Eaten per Crab", x="Activity Proportion", y="Standardized Mussels Eaten", color="Crab Density")

#ggsave("Activity_vs_Standardized_Mussels_Eaten_per_Crab.png", plot = fig_5.7, device ="png", path = here("figures"))
remove(fig_5.7)

## Activity vs. Mussels Handled
fig_5.8 <- ggplot(data_complete, aes(x=activity_proportion, y=sum_mussels_handled, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Activity vs. Mussels Handled per Crab", x="Activity Proportion", y="Mussels Handled", color="Crab Density")

#ggsave("Activity_vs_Mussels_Handled_per_Crab.png", plot = fig_5.8, device ="png", path = here("figures"))
remove(fig_5.8)

## Activity vs. Standardized Mussels Handled
fig_5.9 <- ggplot(data_complete, aes(x=activity_proportion, y=standardized_mussels_handled, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Activity vs. Standardized Mussels Handled per Crab", x="Activity Proportion", y="Standardized Mussels Handled", color="Crab Density")

#ggsave("Activity_vs_Standardized_Mussels_Handled_per_Crab.png", plot = fig_5.9, device ="png", path = here("figures"))
remove(fig_5.9)

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
fig_7.1 <- ggplot(data_complete_trial, aes(as.factor(crab_dens), activity_proportion))+geom_boxplot()+
  labs(title="Density vs. Activity per Trial", x="Crab Density", y="Activity Proportion")

#ggsave("Density_vs_Activity_per_Trial.png", plot = fig_7.1, device ="png", path = here("figures"))
remove(fig_7.1)

## Density vs. Total Aggression Plot
fig_7.2 <- ggplot(data_complete_trial, aes(as.factor(crab_dens), total_aggression))+geom_boxplot()+
  labs(title="Density vs. Aggression per Trial", x="Crab Density", y="Total Aggression")

#ggsave("Density_vs_Aggression_per_Trial.png", plot = fig_7.2, device ="png", path = here("figures"))
remove(fig_7.2)

## Activity vs. Total Aggression Plot
fig_7.3 <- ggplot(data_complete_trial, aes(x=activity_proportion, y=total_aggression, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Activity vs. Aggression per Trial", x="Activity Proportion", y="Total Aggression", color="Crab Density")

#ggsave("Activity_vs_Aggression_per_Trial.png", plot = fig_7.3, device ="png", path = here("figures"))
remove(fig_7.3)

## Activity vs. Standardized Total Aggression Plot
fig_7.4 <- ggplot(data_complete_trial, aes(x=activity_proportion, y=standardized_total_aggression, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Activity vs. Standardized Aggression per Trial", x="Activity Proportion", y="Standardized Total Aggression", color="Crab Density")

#ggsave("Activity_vs_Standardized_Aggression_per_Trial.png", plot = fig_7.4, device ="png", path = here("figures"))
remove(fig_7.4)

## Total Aggression vs. Mussels Eaten (during video trial)
fig_7.5 <- ggplot(data_complete_trial, aes(x=total_aggression, y=sum_mussels_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Aggression vs. Mussels Eaten per Trial", x="Total Agression", y="Mussels Eaten", color="Crab Density")

#ggsave("Aggression_vs_Mussels_Eaten(video)_per_Trial.png", plot = fig_7.5, device ="png", path = here("figures"))
remove(fig_7.5)

## Total Aggression vs. Mussels Eaten (during entire trial)
fig_7.6 <- ggplot(data_complete_trial, aes(x=total_aggression, y=prey_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Aggression vs. Mussels Eaten per Trial", x="Total Agression", y="Mussels Eaten", color="Crab Density")

#ggsave("Aggression_vs_Mussels_Eaten(trial)_per_Trial.png", plot = fig_7.6, device ="png", path = here("figures"))
remove(fig_7.6)

## Total Aggression vs. Mussels Handled
fig_7.7 <- ggplot(data_complete_trial, aes(x=total_aggression, y=sum_mussels_handled, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Aggression vs. Mussels Handled per Trial", x="Total Agression", y="Mussels Handled", color="Crab Density")

#ggsave("Aggression_vs_Mussels_Handled_per_Trial.png", plot = fig_7.7, device ="png", path = here("figures"))
remove(fig_7.7)

## Activity vs. Mussels Handled
fig_7.8 <- ggplot(data_complete_trial, aes(x=activity_proportion, y=sum_mussels_handled, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Activity vs. Mussels Handled per Trial", x="Activity Proportion", y="Mussels Handled", color="Crab Density")

#ggsave("Activity_vs_Mussels_Handled_per_Trial.png", plot = fig_7.8, device ="png", path = here("figures"))
remove(fig_7.8)

## Activity vs. Standardized Mussels Handled
fig_7.9 <- ggplot(data_complete_trial, aes(x=activity_proportion, y=standardized_mussels_handled, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Activity vs. Standardized Mussels Handled per Trial", x="Activity Proportion", y="Standardized Mussels Handled", color="Crab Density")

#ggsave("Activity_vs_Standardized_Mussels_Handled_per_Trial.png", plot = fig_7.9, device ="png", path = here("figures"))
remove(fig_7.9)

## Activity vs. Mussles Eaten (during video trial) Plot
fig_7.10 <- ggplot(data_complete_trial, aes(x=activity_proportion, y=sum_mussels_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Activity vs. Mussels Eaten per Trial", x="Activity Proportion", y="Mussels Eaten", color="Crab Density")

#ggsave("Activity_vs_Mussels_Eaten(video)_per_Trial.png", plot = fig_7.10, device ="png", path = here("figures"))
remove(fig_7.10)

## Activity vs. Standardized Mussles Eaten (during video trial) Plot
fig_7.11 <- ggplot(data_complete_trial, aes(x=activity_proportion, y=standardized_mussels_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Activity vs. Standardized Mussels Eaten per Trial", x="Activity Proportion", y="Standardized Mussels Eaten", color="Crab Density")

#ggsave("Activity_vs_Standardized_Mussels_Eaten(video)_per_Trial.png", plot = fig_7.11, device ="png", path = here("figures"))
remove(fig_7.11)

## Activity vs. Mussels Eaten (during entire trial) Plot 
fig_7.12 <- ggplot(data_complete_trial, aes(x=activity_proportion, y=prey_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Activity vs. Mussels Eaten per Trial", x="Activity Proportion", y="Mussels Eaten", color="Crab Density")

#ggsave("Activity_vs_Mussels_Eaten(trial)_per_Trial.png", plot = fig_7.12, device ="png", path = here("figures"))
remove(fig_7.12)

## Activity vs. Standardized Mussles Eaten (during entire trial) Plot 
fig_7.13 <- ggplot(data_complete_trial, aes(x=activity_proportion, y=standardized_prey_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Activity vs. Standardized Mussels Eaten per Trial", x="Activity Proportion", y="Mussels Eaten", color="Crab Density")

#ggsave("Activity_vs_Standardized_Mussels_Eaten(trial)_per_Trial.png", plot = fig_7.13, device ="png", path = here("figures"))
remove(fig_7.13)

#8) Creating Trial Average per Crab
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

#9) Figures per Mean Crab
## Density vs. Mean Activity Plot
fig_9.1 <- ggplot(data_complete_average, aes(as.factor(crab_dens), avg_activity_proportion))+geom_boxplot()+
  labs(title="Density vs. Mean Activity per Crab", x="Crab Density", y="Mean Activity Proportion")

#ggsave("Density_vs_Activity_per_Mean_Crab.png", plot = fig_9.1, device ="png", path = here("figures"))
remove(fig_9.1)

## Density vs. Mean Total Aggression Plot
fig_9.2 <- ggplot(data_complete_average, aes(as.factor(crab_dens), avg_total_aggression))+geom_boxplot()+
  labs(title="Density vs. Mean Aggression per Crab", x="Crab Density", y="Mean Total Aggression")            

#ggsave("Density_vs_Aggression_per_Mean_Crab.png", plot = fig_9.2, device ="png", path = here("figures"))
remove(fig_9.2)

## Mean Total Aggression vs. Mussels Handled
fig_9.3 <- ggplot(data_complete_average, aes(x=avg_total_aggression, y=avg_mussels_handled, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Mean Aggression vs. Mussels Handled per Crab", x="Mean Total Agression", y="Mean Mussels Handled", color="Crab Density")

#ggsave("Aggression_vs_Mussels_Handled_per_Mean_Crab.png", plot = fig_9.3, device ="png", path = here("figures"))
remove(fig_9.3)

## Mean Activity vs. Total Aggression Plot
fig_9.4 <- ggplot(data_complete_average, aes(x=avg_activity_proportion, y=avg_total_aggression, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Mean Activity vs. Aggression per Crab", x="Mean Activity Proportion", y="Mean Total Aggression", color="Crab Density")

#ggsave("Activity_vs_Aggression_per_Mean_Crab.png", plot = fig_9.4, device ="png", path = here("figures"))
remove(fig_9.4)

## Mean Activity vs. Standardized Total Aggression Plot
fig_9.5 <- ggplot(data_complete_average, aes(x=avg_activity_proportion, y=avg_standardized_total_aggression, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Mean Activity vs. Standardized Aggression per Crab", x="Mean Activity Proportion", y="Mean Standardized Total Aggression", color="Crab Density")

#ggsave("Activity_vs_Standardized_Aggression_per_Mean_Crab.png", plot = fig_9.5, device ="png", path = here("figures"))
remove(fig_9.5)

## Mean Activity vs. Mussels Eaten Plot
fig_9.6 <- ggplot(data_complete_average, aes(x=avg_activity_proportion, y=avg_mussels_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Mean Activity vs. Mussels Eaten per Crab", x="Mean Activity Proportion", y="Mean Mussels Eaten", color="Crab Density")

#ggsave("Activity_vs_Mussels_Eaten_per_Mean_Crab.png", plot = fig_9.6, device ="png", path = here("figures"))
remove(fig_9.6)

## Mean Activity vs. Standardized Mussels Eaten Plot
fig_9.7 <- ggplot(data_complete_average, aes(x=avg_activity_proportion, y=avg_standardized_mussels_eaten, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Mean Activity vs. Standardized Mussels Eaten per Crab", x="Mean Activity Proportion", y="Mean Standardized Mussels Eaten", color="Crab Density")

#ggsave("Activity_vs_Standardized_Mussels_Eaten_per_Mean_Crab.png", plot = fig_9.7, device ="png", path = here("figures"))
remove(fig_9.7)

## Mean Activity vs. Mussels Handled
fig_9.8 <- ggplot(data_complete_average, aes(x=avg_activity_proportion, y=avg_mussels_handled, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Mean Activity vs. Mussels Handled per Crab", x="Mean Activity Proportion", y="Mean Mussels Handled", color="Crab Density")

#ggsave("Activity_vs_Mussels_Handled_per_Mean_Crab.png", plot = fig_9.8, device ="png", path = here("figures"))
remove(fig_9.8)

## Mean Activity vs. Standardized Mussels Handled
fig_9.9 <- ggplot(data_complete_average, aes(x=avg_activity_proportion, y=avg_standardized_mussels_handled, color=as.factor(crab_dens)))+geom_point()+
  geom_smooth(se=F)+
  labs(title="Mean Activity vs. Standardized Mussels Handled per Crab", x="Mean Activity Proportion", y="Mean Standardized Mussels Handled", color="Crab Density")

#ggsave("Activity_vs_Standardized_Mussels_Handled_per_Mean_Crab.png", plot = fig_9.9, device ="png", path = here("figures"))
remove(fig_9.9)

