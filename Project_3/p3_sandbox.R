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

proper_names1 <- c("Integrated Mean", "Integrated Standard Deviation", 
                   "Integrated Kurtosis", "Intergrated Skew")
proper_names2 <- c("DMSNR Mean", "DMSNR Standard Deviation", "DMSNR Kurtosis",
                   "DMSNR Skew")

var_sel1 <- "integ_skew"
var_sel2 <- "DMSNR_skew"


var1 <- which(var == var_sel1)
var2 <- which(var == var_sel2)

p1 <- df_pulsar %>% rename(x = var1, y = var2) %>%
  ggplot() + geom_point(aes(x = x, y = y, col = Class))

print(p1)

df_pulsar %>% head()

df_pulsar %>% rename(x = var1, y = var2) %>%
  ggplot() + geom_point(aes(x = x, y = y, col = Class), size = 0.25) 


which(var == var_sel1)


df_pulsar %>% rename(x = var[var1], y = var[var2]) %>%
  ggplot() + geom_point(aes(x = x, y = y, col = Class), size = 0.25) + 
  labs(x = proper_names1[var1], 
       y = proper_names2[var2 - 4],
       title = paste(proper_names1[var1],
                     "vs" ,
                     proper_names2[var2 - 4]))