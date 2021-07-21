#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Pulsar EDA"),

    # Sidebar with a slider input for number of bins
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
                            selected = "DMSNR Mean"),
                actionButton("create", "Create Plot")
            ),
            
            mainPanel(
                dataTableOutput("small_tab"),
                br(),
                plotOutput("edaPlot")
            )
    )
))
