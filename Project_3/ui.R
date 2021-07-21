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

var <- names(df_pulsar)

proper_names <- c("Integrated Mean", "Integrated Standard Deviation", 
                  "Integrated Kurtosis", "Intergrated Skew",
                  "DMSNR Mean", "DMSNR Standard Deviation", "DMSNR Kurtosis",
                  "DMSNR Skew")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    titlePanel("Pulsar Classification"),
    
    tabsetPanel(
        tabPanel("Exporatory Data Analysis", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
        
                         radioButtons("plot_type",
                                      "Select the Plot Type",
                                      c("Scatter" = "A",
                                        "Density" = "B")),
                         br(),
                         
                         radioButtons("df_type",
                                      "Select Data Type",
                                      c("Raw" = "A",
                                        "Standardized" = "B")),
                         br(),
                         selectInput("var_sel1", "First Variable to Plot", list("Integrated Mean" = "i_mean", 
                                                                                "Integrated Standard Deviation" = "i_sd", 
                                                                                "Integrated Kurtosis" = "i_kurt", 
                                                                                "Intergrated Skew" = "i_skew"), 
                                     selected = "Integrated Mean"),
                         
                         selectInput("var_sel2", "Second Variable to Plot", list("DMSNR Mean" = "d_mean", 
                                                                                 "DMSNR Standard Deviation" = "d_sd", 
                                                                                 "DMSNR Kurtosis" = "d_kurt", 
                                                                                 "DMSNR Skew" = "d_skew"), 
                                     selected = "DMSNR Mean")
                     ),
                     
                     mainPanel(
                         textOutput("edaText"),
                         h4("There should be a plot here"),
                         br(),
                         plotOutput("edaPlot"),
                     )
                 )
        ),
        tabPanel("Deep Dive", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                     mainPanel(fluidRow(
                         htmlOutput("Attacks")
                     )
                     )
                 )
        ),
        tabPanel("Modeling the Data", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                     mainPanel(fluidRow(
                         htmlOutput("Attacks")
                 )
                 )
             )
        )
))

)