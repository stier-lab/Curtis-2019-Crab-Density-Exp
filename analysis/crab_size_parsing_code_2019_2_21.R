## Code for summarizing crab size per trial
#Author: J. Curtis
#Date: Feb 2, 2019

rm(list=ls())

library(tidyverse)

# Crab size extraction per trial

setwd("C:/Users/Joseph/Box Sync/Shared Stier Lab Folder/Projects/Rock Crab Research/Data")

size <- read.csv("crab_size_data_extracted.csv")
raw <- read.csv("crab_feeding_trial_data_dec2018.csv")


dat_sep <- separate_rows(raw, crab_id, sep = ",")
dat_sep$crab_id <- as.numeric(as.character(dat_sep$crab_id))
size$crab_id <- as.numeric(as.character(size$crab_id))


joined <- right_join(size, dat_sep)
joined <- group_by(joined, order)

summed <- summarise(joined, size_mean = mean(size), size_std = sd(size), size_min_max = max(size) - min (size))

final <- right_join(raw, summed)

test <- size_calc(raw,size)

check <- subset(trim_size, pred_dens == "1")
