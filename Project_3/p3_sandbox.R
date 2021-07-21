library(shiny)
library(tidyverse)
library(maps)
library(forcats)
library(MASS)
library(caret)
library(DT)

df_pulsar <- read_csv("./Data/HTRU_2.csv", col_names = FALSE)

names(df_pulsar) <- c("integ_mean","integ_sd","integ_exkur","integ_skew",
                      "DMSNR_mean","DMSNR_sd","DMSNR_exkur","DMSNR_skew","Class")

df_pulsar <- df_pulsar %>% 
  mutate(Class = ifelse(Class == 1, "Pulsar", "Non Pulsar"))

df_pulsar$Class <- as.factor(df_pulsar$Class)

var <- names(df_pulsar)

proper_names <- c("Integrated Mean", "Integrated Standard Deviation", 
                  "Integrated Kurtosis", "Intergrated Skew",
                  "DMSNR Mean", "DMSNR Standard Deviation", "DMSNR Kurtosis",
                  "DMSNR Skew")

var_sel1 <- "i_mean"
var_sel2 <- "d_skew"


var1 <- switch(var_sel1,
               i_mean = var[1],
               i_sd   = var[2],
               i_kurt = var[3],
               i_skew = var[4])

var2 <- switch(var_sel2,
               d_mean = var[5],
               d_sd   = var[6],
               d_kurt = var[7],
               d_skew = var[8])

p1 <- df_pulsar %>% rename(x = all_of(var1), y = all_of(var2)) %>%
  ggplot() + geom_point(aes(x = x, y = y, col = Class))

print(p1)

df_pulsar %>% head()