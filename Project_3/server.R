#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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

shinyServer(function(input, output, session) {
    
    output$edaPlot <- renderPlot({
        
      var1 <- switch(input$var_sel1, 
                     i_mean = "integ_mean", 
                     i_sd = "integ_sd", 
                     i_kurt = "integ_exkur", 
                     i_skew = "integ_skew")
      var2 <- switch(input$var_sel2, 
                     d_mean = "DMSNR_mean", 
                     d_sd = "DMSNR_sd", 
                     d_kurt = "DMSNR_exkur", 
                     d_skew = "DMSNR_skew")
      
      
        df_pulsar %>% rename(x = var1, y = var2) %>%
             ggplot() + geom_point(aes(x = x, y = y, col = Class)) #+ 
            #labs(x = proper_names[selection[1]], 
                 #y = proper_names[selection[2]],
                 #title = paste(proper_names[selection[2]],
                               #"vs" ,
                               #proper_names[selection[1]]))
       
    })
    
    output$small_tab <- renderDataTable({
            df_pulsar %>%
            head() %>%
            datatable()
    })
    
    output$information <- renderTable({
            df_pulsar %>% head()
    })

})
