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

dense_colors <- c("#003f5c", "#2f4b7c", "#665191", "#a05195", 
                  "#d45087", "#f95d6a", "#ff7c43", "#ffa600")

##################################################################################

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
  
  output$denPlot1 <- renderPlot({
    
    if(input$df_type == "A") {df_data = df_pulsar} else {df_data = df_pulsar2 }  
    
    ggplot(df_data) +
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
    
  })
  
  output$denPlot2 <- renderPlot({
    
    if(input$df_type == "A") {df_data = df_pulsar} else {df_data = df_pulsar2 }  
    
    ggplot(df_data) +
      geom_density(aes(x = DMSNR_mean, fill = dense_colors[5]), alpha = .2) +
      geom_density(aes(x = DMSNR_sd, fill = dense_colors[6]), alpha = .2) +
      geom_density(aes(x = DMSNR_exkur, fill = dense_colors[7]), alpha = .2) +
      geom_density(aes(x = DMSNR_skew, fill = dense_colors[8]), alpha = .2) +
      theme_grey() +
      theme(legend.position = c(0.9, 0.8)) +
      labs(x = "", y = "Density") +
      scale_fill_manual(guide = guide_legend(), name =  "DM-SNR \nReadings",  
                        labels = c("Mean", "Standard Deviation","Kurtosis", "Skew"), 
                        values = dense_colors[5:8])
    
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
