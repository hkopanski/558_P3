library(tidyverse)
library(maps)
library(forcats)


df_pulsar <- read_csv("HTRU_2.csv", col_names = FALSE)

names(df_pulsar) <- c("integ_mean","integ_sd","integ_exkur","integ_skew",
                      "DMSNR_mean","DMSNR_sd","DMSNR_exkur","DMSNR_skew","Class")

print(str(df_pulsar))

ggplot(df_pulsar, aes(x = integ_mean, y = DMSNR_mean, col = as.factor(Class))) +
  geom_point(size = 0.5)

ggplot(df_pulsar, aes(x = integ_sd, y = DMSNR_sd, col = as.factor(Class))) +
  geom_point(size = 0.5)

ggplot(df_pulsar, aes(x = integ_exkur, y = DMSNR_exkur, col = as.factor(Class))) +
  geom_point(size = 0.5)

ggplot(df_pulsar, aes(x = integ_skew, y = DMSNR_skew, col = as.factor(Class))) +
  geom_point(size = 0.5)


ggplot(df_pulsar, aes(x = DMSNR_mean, y = DMSNR_sd, col = as.factor(Class))) +
  geom_point(size = 0.5)

ggplot(df_pulsar, aes(x = integ_mean, y = integ_sd, col = as.factor(Class))) +
  geom_point(size = 0.5)
