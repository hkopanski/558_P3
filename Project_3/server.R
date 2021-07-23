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
library(matrixStats)
library(GGally)
library(shinycssloaders)
library(AMR)


df_pulsar <- read_csv("./Data/HTRU_2.csv", col_names = FALSE)
    
names(df_pulsar) <- c("integ_mean","integ_sd","integ_exkur","integ_skew",
                          "DMSNR_mean","DMSNR_sd","DMSNR_exkur","DMSNR_skew","Class")
    
df_pulsar <- df_pulsar %>% mutate(Class = ifelse(Class == 1, "Pulsar", "Non Pulsar"))
    
df_pulsar$Class <- as.factor(df_pulsar$Class)

df_pulsar2 <- df_pulsar %>% mutate_at(names(df_pulsar)[1:8], ~(scale(.) %>% as.vector))

var <- names(df_pulsar)

proper_names <-  c("Integrated Mean", "Integrated Standard Deviation", 
                   "Integrated Kurtosis", "Intergrated Skew", "DMSNR Mean", 
                   "DMSNR Standard Deviation", "DMSNR Kurtosis", "DMSNR Skew")

proper_names1 <- c("Integrated Mean", "Integrated Standard Deviation", 
                  "Integrated Kurtosis", "Intergrated Skew")

proper_names2 <- c("DMSNR Mean", "DMSNR Standard Deviation", "DMSNR Kurtosis",
                   "DMSNR Skew")

dense_colors <- c("#003f5c", "#2f4b7c", "#665191", "#a05195", 
                  "#d45087", "#f95d6a", "#ff7c43", "#ffa600")

PCA_pulsar <- df_pulsar %>% dplyr::select("integ_mean":"DMSNR_skew") %>% 
  prcomp(., scale = TRUE)

df_pca_plot <- as_tibble(cbind(1:8, PCA_pulsar$sdev^2))

PCA_tab <- data.frame(PCA_pulsar$rotation, 
                      row.names = proper_names)

colnames(df_pca_plot) <- c("PCA", "variance")

##################################################################################

shinyServer(function(input, output, session) {
  df_data <- reactive({
                  switch(input$df_type,
                         "A" = df_pulsar,
                         "B" = df_pulsar2)
  }) 
  output$edaPlot <- renderPlot({
        
        var1 <- which(var == input$var_sel1)
        var2 <- which(var == input$var_sel2)
      
        df_data() %>% rename(x = var[var1], y = var[var2]) %>%
             ggplot() + geom_point(aes(x = x, y = y, col = Class), size = 0.25) + 
             labs(x = proper_names1[var1], 
                  y = proper_names2[var2 - 4],
                  title = paste(proper_names2[var2 - 4],
                                "vs" ,
                                proper_names1[var1])) +
             theme(legend.position = c(0.9, 0.9)) +
             scale_color_manual(values = c("Pulsar" = dense_colors[1],
                                        "Non Pulsar" = dense_colors[7]))
       
    })
  
  output$denPlot1 <- renderPlot({
    
    ggplot(df_data()) +
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
    
    ggplot(df_data()) +
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
  
  output$pairsPlot <- renderPlot({
    ggpairs(df_data(), mapping = ggplot2::aes(color = Class))
  })
    
  output$small_tab <- renderDataTable({
      
            if(input$df_type == "A"){df_data = df_pulsar} else {df_data = df_pulsar2 }
      
            df_data %>%
            head() %>%
            datatable()
    })
    
    output$information <- renderTable({
      a <- colMeans(df_data()[,1:8])
      b <- t(colQuantiles(as.matrix(df_data()[, 1:8])))
      c <- t(colIQRs(as.matrix(df_data()[, 1:8])))
      
      r_names <- c("Means", "Min", "25%", "Median", "75%", "Max" ,"IQ Range")
      
      summary_table <- data.frame(rbind(a, b, c))
      colnames(summary_table) <- c("Integrated Mean", "Integrated Standard Deviation", "Integrated Kurtosis", 
                                   "Intergrated Skew", "DMSNR Mean", "DMSNR Standard Deviation", "DMSNR Kurtosis", "DMSNR Skew")
      
      rownames(summary_table) <- r_names
      
      summary_table
    }, rownames = TRUE)
    
    output$kmeans_plot <- renderPlot ({
      set.seed(800)
      
      plot_var <- c(input$km_sel1, input$km_sel2, "Class")
      
      proper_names <- c("Integrated Mean", "Integrated Standard Deviation", 
                        "Integrated Kurtosis", "Intergrated Skew", "DMSNR Mean", 
                        "DMSNR Standard Deviation", "DMSNR Kurtosis", "DMSNR Skew")
      
      km_pulsar <- kmeans(df_pulsar[ , plot_var[1:2]], input$k_clust, nstart = 20)
      
      #plot(df_pulsar[ , plot_var[1:2]], col = (km_pulsar$cluster + 1))
      
      df_kplot <- as_tibble(cbind(df_pulsar[ , plot_var], km_pulsar$cluster))
      
      names(df_kplot)[4] <- "cluster"
      
      #df_kplot %>% print()
      
      df_kplot %>% rename(x = plot_var[1], y = plot_var[2]) %>% 
        ggplot(aes(x = x, y = y, col = as_factor(cluster), shape = Class)) + 
        geom_point() + 
        scale_color_manual(values = list("1" = dense_colors[1], 
                                         "2" = dense_colors[2],
                                         "3" = dense_colors[3],
                                         "4" = dense_colors[4],
                                         "5" = dense_colors[5],
                                         "6" = dense_colors[6],
                                         "7" = dense_colors[7],
                                         "8" = dense_colors[8]),
                           name = "Clusters") +
        scale_shape_manual(values = list("Pulsar" = 19, 
                                         "Non Pulsar" = 3)) +
        theme_minimal() +
        labs(x = proper_names[which(plot_var[1] == names(df_pulsar))], 
             y = proper_names[which(plot_var[2] == names(df_pulsar))],
             title = paste(proper_names[which(plot_var[2] == names(df_pulsar))], "vs",
                           proper_names[which(plot_var[1] == names(df_pulsar))], "using",
                           input$k_clust, "Clusters"))
    })
    
    
    output$PCA_biplot <- renderPlot({
      
      a_val <- length(numeric(input$PCA_pick1))
      b_val <- length(numeric(input$PCA_pick2))
      
      ggplot_pca(PCA_pulsar, 
                 choices = c(a_val, b_val), 
                 points_size = 1,
                 points_alpha = 0.15,
                 arrows = TRUE,
                 arrows_colour = dense_colors[5],
                 arrows_size = 2,
                 arrows_textsize = 6,
                 arrows_textangled = TRUE,
                 arrows_alpha = 0.75,
                 base_textsize = 12)
    })
    
    output$PCA_scree <- renderPlot ({
      
      df_pca_plot %>% ggplot(aes(x = PCA, y = variance/sum(variance))) + 
        geom_point(shape = 5, col = dense_colors[5]) + 
        geom_line(col = dense_colors[5]) + labs(x = "Principal Component", 
                                                y = "Proportion of Variance Explained",
                                                title = "Proportion of Variance") +
        geom_text(aes(x = PCA, 
                      y = variance/sum(variance), 
                      label = round(variance/sum(variance), 4),
                      hjust = -0.25,
                      vjust = -0.9)) + 
        scale_x_continuous(breaks = seq(0, 8, 1))
    
    })
    
    output$PCA_scree_cum <- renderPlot ({  
        
      df_pca_plot %>% ggplot(aes(x = PCA, y = cumsum(variance/sum(variance)))) + 
        geom_point(shape = 5, col = dense_colors[2]) + 
        geom_line(col = dense_colors[2]) + labs(x = "Principal Component", 
                                                y = "Cumulative Proportion of Variance Explained",
                                                title = "Cumulative Proportion Variance") +
        geom_text(aes(x = PCA, 
                      y = cumsum(variance/sum(variance)), 
                      label = round(cumsum(variance/sum(variance)), 4),
                      hjust = 1,
                      vjust = -2)) +
        ylim(NA, 1.025) + 
        scale_x_continuous(breaks = seq(0, 8, 1))
    })
    
    output$PCA_tab <- renderTable({
      PCA_tab
    }, rownames = TRUE)
    
    observeEvent(input$PCA_pick1, {
      selection_list = list("PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4,
                            "PC5" = 5, "PC6" = 6, "PC7" = 7, "PC8" = 8)
      
      new_selection_list = selection_list[-length(numeric(input$PCA_pick1))]
      
      updateSelectInput(session, "PCA_pick2", choices = new_selection_list)
    })

})
