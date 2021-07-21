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
    
df_pulsar <- df_pulsar %>% mutate(Class = ifelse(Class == 1, "Pulsar", "Non Pulsar"))
    
df_pulsar$Class <- as.factor(df_pulsar$Class)

df_pulsar2 <- df_pulsar %>% mutate_at(names(df_pulsar)[1:8], ~(scale(.) %>% as.vector))

var <- names(df_pulsar)

proper_names1 <- c("Integrated Mean", "Integrated Standard Deviation", 
                  "Integrated Kurtosis", "Intergrated Skew")
proper_names2 <- c("DMSNR Mean", "DMSNR Standard Deviation", "DMSNR Kurtosis",
                   "DMSNR Skew")


shinyServer(function(input, output, session) {
    
  
  
  
  output$edaPlot <- renderPlot({
        
        if(input$df_type == "A") {df_data = df_pulsar} else {df_data = df_pulsar2 }  
    
        var1 <- which(var == input$var_sel1)
        var2 <- which(var == input$var_sel2)
      
        df_data %>% rename(x = var[var1], y = var[var2]) %>%
             ggplot() + geom_point(aes(x = x, y = y, col = Class), size = 0.25) + 
             labs(x = proper_names1[var1], 
                  y = proper_names2[var2 - 4],
                  title = paste(proper_names2[var2 - 4],
                                "vs" ,
                                proper_names1[var1]))
       
    })
    
    output$small_tab <- renderDataTable({
      
            if(input$df_type == "A"){df_data = df_pulsar} else {df_data = df_pulsar2 }
      
            df_data %>%
            head() %>%
            datatable()
    })
    
    output$information <- renderTable({
            df_pulsar %>% head()
    })

})
