# Plot crab Functional Response Curves with bootstrapped confidence bands
# Author: Joe Curtis, 4/11/2019

# Load libraries, import data

rm(list=ls())

library(tidyverse)
library(here)


boot_param_hv <- read.csv(here("analysis/results/bootstrap_results_hv_avg.csv"))

# Get quartiles for bootstrapped parameters

boot_param_hv_sort <- as.data.frame(apply(boot_param_hv, 2,sort,decreasing = F))
lower_ci_hv <- boot_param_hv_sort[2500,c(1:4)]
upper_ci_hv <- boot_param_hv_sort[7500,c(1:4)]
median_ci_hv <- summarise(boot_param_hv_sort, a = median(a), b = median(b), s = median(s), m = median(m))

x_range <- seq(0,20,20/100)

lower_line_hv_1p <- (lower_ci_hv$a * x_range)/((lower_ci_hv$b*x_range)+(1^lower_ci_hv$m))
median_line_hv_1p <- (median_ci_hv$a * x_range)/((median_ci_hv$b*x_range)+(1^median_ci_hv$m))
upper_line_hv_1p <- (upper_ci_hv$a * x_range)/((upper_ci_hv$b*x_range)+(1^upper_ci_hv$m))

hv_plot_1p <- data.frame(x = x_range, y = median_line_hv_1p, lower = lower_line_hv_1p, upper = upper_line_hv_1p)

ggplot(hv_plot_1p, aes(x,y)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower,ymax=upper,x=x,fill="band", alpha = 0.3))

boot_val_hv <- data.frame(matrix(ncol = nrow(boot_param_hv),nrow = length(x_range)))
for(i in seq(1,nrow(boot_param_hv),1)) {
  boot_val_hv[i] <- (boot_param_hv$a[i] * x_range)/((boot_param_hv$b[i]*x_range)+(1^boot_param_hv$m[i]))
}

boot_val_hv <- t(boot_val_hv)
boot_val_hv_sort <- as.data.frame(apply(boot_val_hv, 2,sort,decreasing = F))
boot_val_hv_lower <- as.data.frame(t(boot_val_hv_sort[500,]))
boot_val_hv_upper <- as.data.frame(t(boot_val_hv_sort[9500,]))
boot_val_hv_median <- as.data.frame(apply(boot_val_hv_sort, 2, median))
boot_val_hv_1p <- data.frame(x = x_range, y = boot_val_hv_median, lower = boot_val_hv_lower, upper = boot_val_hv_upper)
colnames(boot_val_hv_1p) <- c("x","y","upper","lower")

ggplot(boot_val_hv_1p, aes(x,y)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower,ymax=upper,x=x,fill="band", alpha = 0.3))

boot_val_hv_2 <- data.frame(matrix(ncol = nrow(boot_param_hv),nrow = length(x_range)))
for(i in seq(1,nrow(boot_param_hv),1)) {
  boot_val_hv_2[i] <- (boot_param_hv$a[i] * x_range)/((boot_param_hv$b[i]*x_range)+(2^boot_param_hv$m[i]))
}

boot_val_hv_2 <- t(boot_val_hv_2)
boot_val_hv_sort_2 <- as.data.frame(apply(boot_val_hv_2, 2,sort,decreasing = F))
boot_val_hv_lower_2 <- as.data.frame(t(boot_val_hv_sort_2[500,]))
boot_val_hv_upper_2 <- as.data.frame(t(boot_val_hv_sort_2[9500,]))
boot_val_hv_median_2 <- as.data.frame(apply(boot_val_hv_sort_2, 2, median))
boot_val_hv_2p <- data.frame(x = x_range, y = boot_val_hv_median_2, lower = boot_val_hv_lower_2, upper = boot_val_hv_upper_2)
colnames(boot_val_hv_2p) <- c("x","y","upper","lower")

ggplot(boot_val_hv_2p, aes(x,y)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower,ymax=upper,x=x,fill="band", alpha = 0.3))

boot_val_hv_3 <- data.frame(matrix(ncol = nrow(boot_param_hv),nrow = length(x_range)))
for(i in seq(1,nrow(boot_param_hv),1)) {
  boot_val_hv_3[i] <- (boot_param_hv$a[i] * x_range)/((boot_param_hv$b[i]*x_range)+(3^boot_param_hv$m[i]))
}

boot_val_hv_3 <- t(boot_val_hv_3)
boot_val_hv_sort_3 <- as.data.frame(apply(boot_val_hv_3, 2,sort,decreasing = F))
boot_val_hv_lower_3 <- as.data.frame(t(boot_val_hv_sort_3[500,]))
boot_val_hv_upper_3 <- as.data.frame(t(boot_val_hv_sort_3[9500,]))
boot_val_hv_median_3 <- as.data.frame(apply(boot_val_hv_sort_3, 2, median))
boot_val_hv_3p <- data.frame(x = x_range, y = boot_val_hv_median_3, lower = boot_val_hv_lower_3, upper = boot_val_hv_upper_3)
colnames(boot_val_hv_3p) <- c("x","y","upper","lower")


ggplot(boot_val_hv_3p, aes(x,y)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower,ymax=upper,x=x,fill="band", alpha = 0.3))
ggplot(boot_val_hv_3p, aes(x,y)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower,ymax=upper,x=x,fill="band", alpha = 0.3))