library(tidyverse)
library(maps)
library(forcats)
library(MASS)
library(caret)

df_pulsar <- read_csv("HTRU_2.csv", col_names = FALSE)

names(df_pulsar) <- c("integ_mean","integ_sd","integ_exkur","integ_skew",
                      "DMSNR_mean","DMSNR_sd","DMSNR_exkur","DMSNR_skew","Class")

df_pulsar <- df_pulsar %>% 
  mutate(Class = ifelse(Class == 1, "Pulsar", "Non Pulsar"))

df_pulsar$Class <- as.factor(df_pulsar$Class)

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

ggplot(df_pulsar, aes(x = I((integ_mean / integ_sd)^0.5), y = I((DMSNR_mean / DMSNR_sd)^0.5), 
                      col = as.factor(Class))) +
  geom_point(size = 0.5)

set.seed(1001)

######################Train Test Sets###############################
set.seed(1)
training <-sample(1:nrow(df_pulsar), size = nrow(df_pulsar) * 0.5)
testing <- dplyr::setdiff(1:nrow(df_pulsar), training)
pulsarTrain <- df_pulsar[training, ]
pulsarTest <- df_pulsar[testing, ]

#####################Logistic Regression############################
glm_fit <- glm(Class ~ ., data = pulsarTrain, family = "binomial")

glm_pred <- predict(glm_fit, newdata =pulsarTest, type = "response")

glm_pred <- ifelse(glm_pred > 0.5, 1, 0)

con_tab_log <- table(glm_pred, pulsarTest$Class)

misclass_rate <- 1 - sum(diag(con_tab_log)) / sum(con_tab_log)

colMeans(df_pulsar[,1:8])

#####Standardize the Data########################
df_pulsar2 <- df_pulsar %>% mutate_at(names(df_pulsar)[1:8], ~(scale(.) %>% as.vector))
#################################################


##################Colors for charts###################################
dense_colors <- c("#003f5c", "#2f4b7c", "#665191", "#a05195", 
                  "#d45087", "#f95d6a", "#ff7c43", "#ffa600")

dense_colors2 <- c("red","blue","green","yellow")
#####################################################################

ggplot(df_pulsar2) +
  geom_density(aes(x = integ_mean, fill = dense_colors[1]), alpha = .2) +
  geom_density(aes(x = integ_sd, fill = dense_colors[2]), alpha = .2) +
  geom_density(aes(x = integ_exkur, fill = dense_colors[3]), alpha = .2) +
  geom_density(aes(x = integ_skew, fill = dense_colors[4]), alpha = .2) +
  theme_gray() +
  theme(legend.position = c(0.9, 0.8)) +
  labs(x = "", y = "Density") +
  scale_fill_manual(guide = guide_legend(), name =  "Integrated \nReadings",  
                    labels = c("Mean", "Standard Deviation","Kurtosis", "Skew"),
                    values = dense_colors[1:4])

ggplot(df_pulsar2) +
  geom_density(aes(x = DMSNR_mean, fill = dense_colors[5]), alpha = .2) +
  geom_density(aes(x = DMSNR_sd, fill = dense_colors[6]), alpha = .2) +
  geom_density(aes(x = DMSNR_exkur, fill = dense_colors[7]), alpha = .2) +
  geom_density(aes(x = DMSNR_skew, fill = dense_colors[8]), alpha = .2) +
  #theme_grey() +
  theme(legend.position = c(0.9, 0.8)) +
  labs(x = "", y = "Density") +
  scale_fill_manual(guide = guide_legend(), name =  "DM-SNR \nReadings",  
                      labels = c("Mean", "Standard Deviation","Kurtosis", "Skew"), 
                      values = dense_colors[5:8])


#################Random Forest##############################################

trctrl <- trainControl(method = "repeatedcv", 
                       number = 5, 
                       repeats = 2)

rf_fit <- train(Class ~ ., data = pulsarTrain, method = 'rf',
                preProcess = c('center', 'scale'),
                tuneGrid = data.frame(mtry = 1:8))
rf_fit

set.seed(100)

rf_pred <- predict(rf_fit, newdata = pulsarTest)

rf_misclass <- sum(rf_pred != pulsarTest$Class)/nrow(pulsarTest)

sprintf("The rf misclassification rate is %0.3f" , rf_misclass)

pulsarTest3 <- pulsarTest %>% 
  mutate(rfPred = rf_pred, misclass = rfPred != Class)

head(pulsarTest3)

pulsarTest3 %>% ggplot(aes(x = integ_mean, y = DMSNR_mean, 
                           col = as.factor(misclass))) + 
  geom_point(shape = 20) + facet_grid( ~ Class)

knitr::kable(table(rf_pred, pulsarTest$Class))

##########################################################################################
ggplot(df_pulsar2) + geom_density(aes(x = integ_mean, fill = dense_colors[1]), alpha = .2)
ggplot(df_pulsar2) + geom_density(aes(x = integ_sd, fill = dense_colors[2]), alpha = .2)
ggplot(df_pulsar2) + geom_density(aes(x = integ_exkur, fill = dense_colors[3]), alpha = .2)
ggplot(df_pulsar2) + geom_density(aes(x = integ_skew, fill = dense_colors[4]), alpha = .2)
##########################################################################################

df_pulsar3 <- df_pulsar %>% gather(key = measure, 
                                   value = value, integ_mean:DMSNR_skew)
##########################################################################################
var <- names(df_pulsar)

proper_names <- c("Integrated Mean", "Integrated Standard Deviation", 
                  "Integrated Kurtosis", "Intergrated Skew",
                  "DMSNR Mean", "DMSNR Standard Deviation", "DMSNR Kurtosis",
                  "DMSNR Skew")
##########################################################################################
selection <- c(5,1)

df_pulsar %>% rename(x = var[selection[1]], y = var[selection[2]]) %>%
  ggplot() + geom_point(aes(x = x, y = y, col = Class)) + 
  labs(x = proper_names[selection[1]], 
       y = proper_names[selection[2]],
       title = paste(proper_names[selection[2]],
                     "vs" ,
                     proper_names[selection[1]]))

df_pulsar %>% print()
#########################################################################################
