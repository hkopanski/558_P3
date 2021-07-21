#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

df_pulsar2 <- df_pulsar %>% mutate_at(names(df_pulsar)[1:8], ~(scale(.) %>% as.vector))

var <- names(df_pulsar)

proper_names <- c("Integrated Mean", "Integrated Standard Deviation", 
                  "Integrated Kurtosis", "Intergrated Skew",
                  "DMSNR Mean", "DMSNR Standard Deviation", "DMSNR Kurtosis",
                  "DMSNR Skew")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    titlePanel("Pulsar Classification"),
    
    tabsetPanel(
        tabPanel("Exporatory Data Analysis",
                 sidebarLayout(
                     sidebarPanel(
        
                         radioButtons("plot_type",
                                      "Select the Plot Type",
                                      c("Scatter" = "A",
                                        "Density" = "B")),
                         br(),
                         
                         radioButtons("df_type",
                                      "Select Data Type",
                                      list("Raw" = "A",
                                           "Standardized" = "B")),
                         br(),
                         selectInput("var_sel1", "First Variable to Plot", list("Integrated Mean" = "integ_mean", 
                                                                                "Integrated Standard Deviation" = "integ_sd", 
                                                                                "Integrated Kurtosis" = "integ_exkur", 
                                                                                "Intergrated Skew" = "integ_skew"), 
                                     selected = "Integrated Mean"),
                         
                         selectInput("var_sel2", "Second Variable to Plot", list("DMSNR Mean" = "DMSNR_mean", 
                                                                                 "DMSNR Standard Deviation" = "DMSNR_sd", 
                                                                                 "DMSNR Kurtosis" = "DMSNR_exkur", 
                                                                                 "DMSNR Skew" = "DMSNR_skew"), 
                                     selected = "DMSNR Mean"),
                     ),
                     
                     mainPanel(
                         plotOutput("edaPlot"),
                         dataTableOutput("small_tab")
                     )
                 )
        ),
        tabPanel("Deep Dive",
                 sidebarLayout(
                     sidebarPanel(radioButtons("dd_type",
                                               "Select Unsupervised Learning Type",
                                               c("K Means" = "A",
                                                 "PCA" = "B")),
                                  br(),),
                     mainPanel(
                     )
                 )
        ),
        tabPanel("Modeling the Data",
                 sidebarLayout(
                     sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                     mainPanel(
                 )
             )
        )
))

)