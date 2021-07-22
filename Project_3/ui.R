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
library(matrixStats)
library(GGally)
library(shinycssloaders)
library(AMR)

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

options(spinner.color="#003f5c", spinner.color.background="#ffffff", spinner.size=2)

shinyUI(fluidPage(
    
    titlePanel("Pulsar Classification"),
    
    tabsetPanel(
        tabPanel("Exporatory Data Analysis",
                 sidebarLayout(
                     sidebarPanel(
        
                         h4("Select options below for exploratory analysis of pulsar data"),
                         br(),
                         radioButtons("plot_type",
                                      "Select the Plot Type",
                                      list("Scatter" = "A",
                                           "Density" = "B",
                                           "Pairs" = "C")),
                         br(),
                         h4("Select below to use standardized data"),
                         br(),
                         radioButtons("df_type",
                                      "Select Data Type",
                                      list("Raw" = "A",
                                           "Standardized" = "B")),
                         br(),
                         conditionalPanel(condition = "input.plot_type == 'A'",
                         br(),                  
                         h4("Choose Variables Below for Scatterplot"),
                         br(),
                         selectInput("var_sel1", "First Variable to Plot (X Axis)", 
                                                                                list("Integrated Mean" = "integ_mean", 
                                                                                "Integrated Standard Deviation" = "integ_sd", 
                                                                                "Integrated Kurtosis" = "integ_exkur", 
                                                                                "Intergrated Skew" = "integ_skew"), 
                                     selected = "Integrated Mean"),
                         
                         selectInput("var_sel2", "Second Variable to Plot (Y Axis)", 
                                                                                 list("DMSNR Mean" = "DMSNR_mean", 
                                                                                 "DMSNR Standard Deviation" = "DMSNR_sd", 
                                                                                 "DMSNR Kurtosis" = "DMSNR_exkur", 
                                                                                 "DMSNR Skew" = "DMSNR_skew"), 
                                     selected = "DMSNR Mean"),
                     )
                     
                     ),
                     
                     mainPanel(
                         conditionalPanel(condition = "input.plot_type == 'A'",
                            withSpinner(plotOutput("edaPlot"), type = 5)),
                         conditionalPanel(condition = "input.plot_type == 'B'",
                            h4("Density Plot for the 4 Integrated Measurements"),
                            withSpinner(plotOutput("denPlot1"), type = 5),
                            h4("Density Plot for the 4 DM-SNR Measurements"),
                            withSpinner(plotOutput("denPlot2"), type = 5)),
                         conditionalPanel(condition = "input.plot_type == 'C'",
                            h4("Pairs Plot for Pulsar Data"),
                            withSpinner(plotOutput("pairsPlot"),type = 5)),                  
                         tableOutput("information")
                     )
                 )
        ),
        tabPanel("Deep Dive",
                 sidebarLayout(
                     sidebarPanel(radioButtons("dd_type",
                                               "Select Unsupervised Learning Type",
                                               list("K Means" = "A",
                                                    "PCA" = "B")),
                                  conditionalPanel(condition = "input.dd_type == 'A'",
                                                   sliderInput("k_clust", "Number of Clusters (K)", min = 2, max = 8, value = 3),
                                                   selectInput("km_sel1", "First Variable to Plot (X Axis)", 
                                                               list("Integrated Mean" = "integ_mean", 
                                                                    "Integrated Standard Deviation" = "integ_sd", 
                                                                    "Integrated Kurtosis" = "integ_exkur", 
                                                                    "Intergrated Skew" = "integ_skew"), 
                                                               selected = "Integrated Mean"),
                                                   
                                                   selectInput("km_sel2", "Second Variable to Plot (Y Axis)", 
                                                               list("DMSNR Mean" = "DMSNR_mean", 
                                                                    "DMSNR Standard Deviation" = "DMSNR_sd", 
                                                                    "DMSNR Kurtosis" = "DMSNR_exkur", 
                                                                    "DMSNR Skew" = "DMSNR_skew"), 
                                                               selected = "DMSNR Mean")),
                                  conditionalPanel(condition = "input.dd_type == 'B'",
                                                   radioButtons("pca_plot_type", "Select a PCA Plot",
                                                                list("Biplot" = "A",
                                                                     "Screeplot" = "B",
                                                                     "Screeplot (Cumulative)" = "C")),
                                  )
                                  ),
                     mainPanel(
                         conditionalPanel(condition = "input.dd_type == 'A'",
                                          withSpinner(plotOutput("kmeans_plot"), type = 5)),
                         conditionalPanel(condition = "input.dd_type == 'B' & input.pca_plot_type == 'A'",
                                          withSpinner(plotOutput("PCA_biplot"), type = 5)),
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