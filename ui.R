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
library(shinythemes)
library(forcats)
library(caret)
library(DT)
library(matrixStats)
library(GGally)
library(shinycssloaders)
library(AMR)

df_pulsar <- read_csv("./Data/HTRU_2.csv", col_names = FALSE)

var <- c("integ_mean","integ_sd","integ_exkur","integ_skew",
         "DMSNR_mean","DMSNR_sd","DMSNR_exkur","DMSNR_skew",
         "Class")

names(df_pulsar) <- var

df_pulsar <- df_pulsar %>% 
    mutate(Class = ifelse(Class == 1, "Pulsar", "Non Pulsar"))

df_pulsar$Class <- as.factor(df_pulsar$Class)

df_pulsar2 <- df_pulsar %>% mutate_at(names(df_pulsar)[1:8], ~(scale(.) %>% as.vector))

var <- names(df_pulsar)

proper_names <- c("Integrated Mean", "Integrated Standard Deviation", 
                  "Integrated Kurtosis", "Intergrated Skew",
                  "DMSNR Mean", "DMSNR Standard Deviation", "DMSNR Kurtosis",
                  "DMSNR Skew")

names_list <- list("integ_mean","integ_sd","integ_exkur","integ_skew",
                   "DMSNR_mean","DMSNR_sd","DMSNR_exkur","DMSNR_skew",
                   "Class")

names(names_list) <- c(proper_names , "Class")

options(spinner.color="#D5220F", spinner.color.background="#ffffff", spinner.size=2)

shinyUI(fluidPage(
    theme = shinytheme("simplex"),
    titlePanel("Pulsar Classification"),
    
    tabsetPanel(
        tabPanel("About",
                mainPanel(
                    h3("Pulsar Classification"),
                    br(),
                    p("Pulsars are subset of neutron stars with the distinguishing feature of 
                      being highly magnetized and fast spinning. This characteristic causes 
                      them to appear as pulsating radio wave sources when observed (some do 
                      give off x-ray and even visible light pulses", 
                      tags$a(href = "https://astronomy.swin.edu.au/cosmos/p/pulsar", "(see SWIN Link)"), 
                      "). The pulse is a result of the pulsar’s radio beam sweeping past the earth 
                      for every rotation. Due to the very regular and precise nature of the 
                      pulses they give off, pulsars have become extremely important astronomical 
                      objects. They are used by physicists to conduct experiments on the 
                      most extreme areas of relativity and for making observations on the 
                      structure of our universe",tags$a(href = "https://www.nrao.edu/pr/2012/aaaspulsars/", "(see NRAO Link)"),".  
                      More can be read about pulsars and how they are being studied can be found 
                      in the links found in this write up and here 
                      ", tags$a(href = "https://chandra.si.edu/xray_sources/neutron_stars.html", "Chandra Link"),"."),
                    img(src = "pulsar.gif", height = '250px', width = '500px'),
                    p("Source of image/gif:", tags$a(href = "https://www.nasa.gov/sites/default/files/thumbnails/image/pulsar_magnetosphere_model_web.gif", "NASA Link"), " "),
                    p("The above gif displays why we see the 'pulse'. As the pulsar rotates, magnetic field (seen in blue) sweeps across the Earth. Each time this happens, 
                      a blip in radio signal is picked up."),
                    h3("Rendering of Pulsar"),
                    img(src = "magnetar.png", height = '250px', width = '500px', contentType = "image/png"),
                    h3(),
                    p("Source of image:", 
                    tags$a(href = "https://www.nasa.gov/sites/default/files/thumbnails/image/magnetar20200617-16.jpg", "NASA Link"),""),
                    h3(),
                    h3("Source Data"),
                    p("Data for about 18,000 neutron stars, some of which being pulsars, was 
                      obtained from the High Time Resolution Universe Survey and can be found 
                      here", tags$a(href = "https://archive.ics.uci.edu/ml/datasets/HTRU2", "UCI Link"),"."),
                    p("Each observation contains 9 variables (8 predictors and 1 classification). 
                      The 8 predictors are comprised of 4 statistical attributes (mean, standard 
                      deviation, excess kurtosis, and skew)  for the integrated radio profile 
                      and dispersion measure signal to noise ratio (DM-SNR). As a pulsar’s 
                      radio signal travels through space it interacts with interstellar media. 
                      This interaction creates a frequency based time delay, higher frequencies 
                      being delayed less than lower frequencies. The radio frequency can be 
                      integrated into a single profile of which is one of the predictors described 
                      in this app. To allow for the integration of the radio profile, the 
                      dispersion of each frequency has to be measured (essentially a factor 
                      that is applied to account for the time delay). The dispersion measure 
                      can be analyzed to produce more statistics on the neutron star in question. 
                      The information provided by both the integrated profile and the dispersion 
                      measure provides a sort of a finger print for the neutron star. A more in 
                      depth explanation of this can be found here", tags$a(href = "https://arxiv.org/abs/1603.05166", 
                                                                           "research paper"),"."),
                    h3("Application Purpose"),
                    p("The purpose of this app is to provide an interactive data analysis of neutron 
                      star data. The app allows you to view the raw data and get an understanding of 
                      the range of each of the predictors (see the “Data” tab). There is also the 
                      ability to graph the data in various scatter and density plots. Also, allowing 
                      you to view how the predictors relate to each other (see “Exploratory Data 
                      Analysis” tab). For a deeper dive into the data, the app has the ability to 
                      run a clustering and principal component analysis on the data (see “Deep Dive” tab). 
                      Finally, the user will be able to create 3 different prediction models and 
                      test them to see how well a pulsar can be identified using the provided data 
                      (see “Modeling the Data” tab). Instruction on how to use each tools are provided in the respective tabs.")
                    
                 )
        ),
        tabPanel("Data",
                 sidebarLayout(
                     sidebarPanel(
                         h3("Interactive Data Table"),
                         p("Table can be filtered on variable values by \n using the slider popup under the variable name"),
                         br(),
                         p("Select which variables to view in the table"),
                         checkboxGroupInput("var_options", "Select Variables:"," ")
                     ),
                 mainPanel(
                     dataTableOutput("filterable_data_table")
                     )
                 )
        ),
        tabPanel("Exporatory Data Analysis",
                 sidebarLayout(
                     sidebarPanel(
        
                         h4("Select options below for exploratory analysis of pulsar data"),
                         br(),
                         radioButtons("plot_type",
                                      "Select the Plot Type",
                                      list("Scatter" = "A",
                                           "Density" = "B",
                                           "Boxplots" = "D",
                                           "Pairs" = "C")
                                      ),
                         br(),
                         radioButtons("split_on_class",
                                      "Split chart on Class",
                                      list("Yes" = "yes",
                                           "No" = "no")
                                      ),
                         br(),
                         h4("Select below to use standardized data"),
                         br(),
                         radioButtons("df_type",
                                      "Select Data Type",
                                      list("Raw" = "raw_pulsar_data",
                                           "Standardized" = "standard_pulsar_data")
                                      ),
                         br(),
                         conditionalPanel(condition = "input.plot_type == 'A'",
                         br(),                  
                         h4("Choose Variables Below for Scatterplot"),
                         br(),
                         selectInput("var_sel1", "First Variable to Plot (X Axis)", 
                                     list("Integrated Mean" = "integ_mean", 
                                          "Integrated Standard Deviation" = "integ_sd", 
                                          "Integrated Kurtosis" = "integ_exkur", 
                                          "Integrated Skew" = "integ_skew",
                                          "DMSNR Mean" = "DMSNR_mean", 
                                          "DMSNR Standard Deviation" = "DMSNR_sd", 
                                          "DMSNR Kurtosis" = "DMSNR_exkur", 
                                          "DMSNR Skew" = "DMSNR_skew"), 
                                     selected = "integ_mean"),
                         
                         selectInput("var_sel2", "Second Variable to Plot (Y Axis)", 
                                     "", 
                                     selected = "DMSNR_mean")
                         ),
                         downloadButton("downloadEDA", "Download data used in this Section"),
                         p(),
                         conditionalPanel(condition = "input.plot_type == 'A'",
                                          downloadButton("download_plot1", "Download plot (Scatter)")
                                          ),
                         conditionalPanel(condition = "input.plot_type == 'B'",
                                          downloadButton("download_plot2", "Download Density Plot (Integrated)"),
                                          p(),
                                          downloadButton("download_plot3", "Download Density plot (DM-SNR)")
                                          ),
                         conditionalPanel(condition = "input.plot_type == 'C'",
                                          downloadButton("download_plot4", "Download Pairs Plot"),
                                          p("Click once and please wait, pairs plot download takes a moment to prepare")
                                          ),
                         conditionalPanel(condition = "input.plot_type == 'D'",
                                          downloadButton("download_plot5", "Download Boxplot"),
                                          p())
                     ),
                     
                     mainPanel(
                         conditionalPanel(condition = "input.plot_type == 'A'",
                            withSpinner(plotOutput("edaPlot", dblclick = "scplot_dblclick",
                                                   brush = brushOpts(id = "scplot_brush", 
                                                                     resetOnNew = TRUE)
                                                   ), 
                                        type = 5)
                            ),
                         conditionalPanel(condition = "input.plot_type == 'B'",
                            h4("Density Plot for the 4 Integrated Measurements"),
                            withSpinner(plotOutput("den_plot1_ui"), type = 5),
                            h4("Density Plot for the 4 DM-SNR Measurements"),
                            withSpinner(plotOutput("den_plot2_ui"), type = 5)
                            ),
                         conditionalPanel(condition = "input.plot_type == 'C'",
                            h4("Pairs Plot for Pulsar Data"),
                            withSpinner(plotOutput("pairs_plot_ui"),type = 5)
                            ),
                         conditionalPanel(condition = "input.plot_type == 'D'",
                            h4("Boxplot for Pulsar Data"),
                            withSpinner(plotOutput("boxplot_ui"),type = 5)
                            ),
                         withSpinner(tableOutput("information"), type = 5)
                     )
                 )
        ),
        tabPanel("Deep Dive",
                 sidebarLayout(
                     sidebarPanel(radioButtons("dd_type",
                                               "Select Unsupervised Learning Type",
                                               list("K Means" = "A",
                                                    "PCA" = "B")
                                               ),
                                  conditionalPanel(condition = "input.dd_type == 'A'",
                                                   sliderInput("k_clust", "Number of Clusters (K)", min = 2, max = 8, value = 3),
                                                   selectInput("km_sel1", "First Variable to Plot (X Axis)", 
                                                               list("Integrated Mean" = "integ_mean", 
                                                                    "Integrated Standard Deviation" = "integ_sd", 
                                                                    "Integrated Kurtosis" = "integ_exkur", 
                                                                    "Integrated Skew" = "integ_skew",
                                                                    "DMSNR Mean" = "DMSNR_mean", 
                                                                    "DMSNR Standard Deviation" = "DMSNR_sd", 
                                                                    "DMSNR Kurtosis" = "DMSNR_exkur", 
                                                                    "DMSNR Skew" = "DMSNR_skew"), 
                                                               selected = "Integrated Mean"),
                                                   
                                                   selectInput("km_sel2", "Second Variable to Plot (Y Axis)", 
                                                               "", 
                                                               selected = "DMSNR Mean")
                                                   ),
                                  conditionalPanel(condition = "input.dd_type == 'B'",
                                                   radioButtons("pca_plot_type", "Select a PCA Plot",
                                                                list("Biplot" = "A",
                                                                     "Screeplot" = "B",
                                                                     "Screeplot (Cumulative)" = "C")
                                                                ),
                                  ),
                                  br(),
                                  conditionalPanel(condition = "input.dd_type == 'B' & input.pca_plot_type == 'A'",
                                                   selectInput("PCA_pick1", "Choose a Principal Component",
                                                               list("PC1" = 1,
                                                                    "PC2" = 2,
                                                                    "PC3" = 3,
                                                                    "PC4" = 4,
                                                                    "PC5" = 5,
                                                                    "PC6" = 6,
                                                                    "PC7" = 7,
                                                                    "PC8" = 8)
                                                               ),
                                                   br(),
                                                   selectInput("PCA_pick2", "Choose a 2nd Principal Component",
                                                               ""),
                                  ),
                                  ),
                     mainPanel(
                         conditionalPanel(condition = "input.dd_type == 'A'",
                                          withSpinner(plotOutput("kmeans_plot", dblclick = "kplot_dblclick",
                                                                 brush = brushOpts(id = "kplot_brush", 
                                                                                   resetOnNew = TRUE)), 
                                                                 type = 5)
                                          ),
                         conditionalPanel(condition = "input.dd_type == 'B' & input.pca_plot_type == 'A'",
                                          withSpinner(plotOutput("PCA_biplot"), type = 5)
                                          ),
                         conditionalPanel(condition = "input.dd_type == 'B' & input.pca_plot_type == 'B'",
                                          withSpinner(plotOutput("PCA_scree"), type = 5)
                                          ),
                         conditionalPanel(condition = "input.dd_type == 'B' & input.pca_plot_type == 'C'",
                                          withSpinner(plotOutput("PCA_scree_cum"), type = 5)
                                          ),
                         conditionalPanel(condition = "input.dd_type == 'B'",
                                          withSpinner(tableOutput("PCA_tab"), type = 5)
                                          ),
                         verbatimTextOutput("PCA_text")
                     )
                 )
        ),
        tabPanel("Modeling the Data",
                 sidebarLayout(
                     sidebarPanel(
                         h3("Test Train Split and Data set Reduction"),
                         p("Due to the large size of the data set, you are able to reduce the data 
                           set size by adjusting the data reduction multipier. For example, 
                           entering 0.25 will result in a randomly sample dataset that is 25% the 
                           size of the original. This is so wait times can be reduced for analysis"),
                         checkboxInput("hide_split", "Hide Train Test Options"),
                         conditionalPanel(condition = "!input.hide_split",
                                          
                                          checkboxGroupInput("mod_var_opt", "Select Variables for Model Fitting:", 
                                                             names_list[1:8], selected = var[1:8]),
                                          numericInput("train_split", "Training Data Split", 
                                                       min = 0, max = 1, step = 0.05, value = 0.5),
                                          numericInput("seed_set", "Set Seed Value", 
                                                       min = 1, step = 1, value = 100),
                                                       numericInput("pop_redux", "Data Reduction Multiplier \n 
                                                                    (choosing 1 will use all available data)", 
                                                                    min = 0, max = 1, step = 0.05, value = 0.1)
                                          ),
                         
                         actionButton("model_prep", "Create Test and Train splits", class = "btn-success"),
                         p("Once this button has been pressed, verify the information displayed on the model fitting tab."),
                         br(),
                         h3("Cross Validation Arguments"),
                         p("Enter and verify the following information."),
                         checkboxInput("hide_cv", "Hide CV Options"),
                         conditionalPanel(condition = "!input.hide_cv",
                                          
                                          selectInput("method_opt", "Cross-Validation Methods", list("Repeat CV" = "repeatedcv",
                                                                                                     "CV" = "cv",
                                                                                                     "LOOCV" = "LOOCV")
                                                      ),
                                          numericInput("k_fold", "K folds for Cross Validation", 
                                                       min = 1, max = 10, value = 5, step = 1),
                                          numericInput("cv_repeats", "Number of Repeats", 
                                                       min = 1, max = 5, value = 2, step = 1)
                                          ),
                         actionButton("tc_update", "Update Train Control Parameters", class = "btn-success"),
                         br(),
                         h3("Model Arguments"),
                         checkboxInput("hide_mod_arg", "Hide Model Options"),
                         conditionalPanel(condition = "!input.hide_mod_arg",
                                          
                                          numericInput("max_k", "Maximum K value for KNN", 
                                                       min = 2, max = 25, value = 10, step = 1),
                                          numericInput("max_mtry", "Maximum Number of Variables for Random Forest", 
                                                       min = 1, max = 8, value = 3, step = 1),
                                          numericInput("n_trees", "Number of trees for Random Forest",
                                                       min = 100, max = 5000, value = 1000),
                                          p("Using a maximum of 8 variables may produce a bagging model")
                                          
                                          ),
                         actionButton("run_model", "Create Models", class = "btn-success"),
                         p("A message will appear once models have been trained and are ready for testing."),
                         br(),
                         p(" "),
                         radioButtons("model_sel", "Select Models to View in Prediction Tab", list("Logistic Regression" = "glm",
                                                                                              "KNN Analysis" = "knn",
                                                                                              "Ensemble Method" = "rf"),
                                            selected = c("glm")
                                      ),
                         p("Once the models have been trained they are ready for testing"),
                         actionButton("test_model", "Run Models on Test Set", class = "btn-success")
                         ),
                     mainPanel(
                         tabsetPanel(type = "tabs",
                                     tabPanel("Model Information",
                                              withMathJax(),
                                              h3("Methods used in this Application"),
                                              p("This application allows for 3 types of classification methods: Logistic, KNN, 
                                                and Random Forests. Below is a short summary of those three methods. All information 
                                                presented here was pulled from 'An Introduction to Statistical Learning' by James et al.",
                                                tags$a(href = "https://www.statlearning.com/", "This link"),"will take you to the book's 
                                                web page. There you will be able to find a more in depth explanation of the methods used in this 
                                                application as well as some additional ones."),
                                              h3("Logistic Regression"),
                                              p("Logistic regression is a generalized form of linear regression. The main difference being 
                                                rather than finding a regression value, the model will output a probability of whether an
                                                observation falls in a certain classification. In this case, the logistic regression is determining
                                                whether the probability of a particular neutron star is a pulsar or not. The default setting is 50%,
                                                but this can be adjusted manually if explicitly indicated."),
                                              p("The general form of the logistic regression equation are as follows:"),
                                              p(strong("Form 1")
                                                ),
                                              helpText("$$ln\\left(\\frac{p(x)}{1 - p(x)}\\right) = \\beta_0 + \\beta_1x_1 + \\beta_2x_2 + .......$$"),
                                              p(strong("Form 2")
                                                ),
                                              helpText("$$\\frac{p(x)}{1 - p(x)} = e^{\\beta_0 + \\beta_1x_1 + \\beta_2x_2 + .......}$$"),
                                              p(strong("Form 3")
                                                ),
                                              helpText("$$p(x) = \\frac{e^{\\beta_0 + \\beta_1x_1 + \\beta_2x_2 + .......}}{1 - e^{\\beta_0 + \\beta_1x_1 + \\beta_2x_2 + .......}}$$"),
                                              
                                              p("All three forms are derived from the same equation, but to focus on form 1, we can easily see how
                                                logistic and linear regression are related. Hence the name, generalized linear equation. The left side 
                                                of the equation is what is known as the log odds, or logit, function. This is what essentially 'converts' 
                                                a continuous term to a probability. In this case, without the logit function, we could have run a linear
                                                regression model an created a cutoff point for the response. This would lead to odd results such as anything
                                                over 100,000 is a pulsar. That value would not carry any real meaning and would be difficult to explain. 
                                                Where as, denoted the response as a probability is more intuitive."),
                                              p("A disadvantage of logistic regression is the data boundary is assumed to be linear. Logistic regression 
                                                does not do well in cases where data classifications are not linear."),
                                              h3("k Nearest Neighbors"),
                                              p("KNN is a  relatively easy concept to understand. Basically, classification is done by observing the k number 
                                                of neighbors near a point of interest and classifying that point as the majority of the neighbor classification.
                                                For example, an observation would be considered a pulsar if over 50% of it's neighbors are pulsars. This 
                                                'distance' is the euclidean distance and is calculated as such. The formula is as follows:"),
                                              p(strong("Euclidean Distance")),
                                              helpText("$$d^2 = d_{x_1}^2 + d_{x_2}^2 + d_{x_3}^2 + .......$$"), 
                                              helpText(span("In the case of pulsar data, each \\(d_x\\) is the distance between two predictor values
                                                            mentioned in the application such as Integrated Mean.", style = "color:gray")
                                                       ),
                                              p("The disadvantage of KNN is that it suffers from the curse of dimensionality. Meaning the more terms that are
                                                added the more data you would need. Adding more predictors to the model without more data causes the model to 
                                                become less accurate. Also, KNN can be computationally intense, especially when there are a lot of data points."),
                                              h3("Ensemble Methods (Random Forests)"),
                                              p("It is difficult to explain what a random forest is without first explaining decision trees and bagging. To 
                                                start of, decisions trees are flow chart type of way of classification. Meaning, when training a decision tree model
                                                you are looking for values in the data where the sample splits an creates the lowest amount of variance in the case
                                                of regression, and the lowest misclassification in the case of classification. This is done until a satisfactory 
                                                point such as no single group is more than 10. The problem with trees are that they are prone to high variance, where
                                                a small change in the data will create completely different tree. This is where bootstrap aggregation, or bagging, 
                                                comes in. Here, instead of using the entire training set, a sample with replacement is used instead (the set is treated
                                                as a population). Since the sampling is done with replacement, roughly two thirds of the data is used in the model training.
                                                The other third is used to test the model. This is done a large number of time, 500 or so. In each instance, an observation
                                                is given a classification (or a value in the case of regression). Once the model training is complete, each observation is
                                                given a classification based on majority vote (for example in 300 of the 500 trees, an observation was classified as a 
                                                pulsar). In the case of regression, the observation is assigned the average value of all the trees. This method reduces 
                                                model variance since the observation value/classification is based on an average rather than a single tree based model.
                                                Random forest takes this concept one step further, and varies the number of predictors. This allows for less dominant 
                                                predictors to contribute to the final model and provide an overall more accurate prediction. The disadvantage is they 
                                                do not have a single closed form formula, so they are essentially a black box algorithm."),
                                              p(strong("Please, go to the 'Model Fitting' tab to begin training and testing models."))
                                              ),
                                     tabPanel("Model Fitting", 
                                              
                                              p(strong("Procedure flow is: Test/Train Split --> CV Arguments --> Create Models --> Test Models")),
                                              p("If new test/train split is created, models must be recreated and retested. CV arguments 
                                                only need to be updated if they change"),
                                              p("Model information can be viewed prior to testing in 'Prediction on Test Data' tab. Test results 
                                                will be displayed under model information."),
                                              dataTableOutput("pulsar_redux"),
                                              verbatimTextOutput("train_rows"),
                                              verbatimTextOutput("test_rows"),
                                              verbatimTextOutput("trnctrl"),
                                              withSpinner(verbatimTextOutput("model_ready"), type = 5)
                                              
                                              ),
                                     
                                     tabPanel("Prediction on Test Data", 
                                              h3("Model Results"),
                                              conditionalPanel(condition = "input.model_sel == 'glm'",
                                                               withSpinner(verbatimTextOutput("log_fit_ui"), type = 5),
                                                               withSpinner(verbatimTextOutput("log_summary_ui"), type = 5),
                                                               verbatimTextOutput("log_MC_ui"),
                                                               verbatimTextOutput("log_CT_ui"),
                                                               dataTableOutput("df_log_pred_ui"),
                                                               plotOutput("plot_log_pred_ui")
                                                               ),
                                              conditionalPanel(condition = "input.model_sel == 'knn'",
                                                               withSpinner(verbatimTextOutput("knn_fit_ui"), type = 5),
                                                               withSpinner(verbatimTextOutput("knn_summary_ui"), type = 5),
                                                               verbatimTextOutput("knn_MC_ui"),
                                                               verbatimTextOutput("knn_CT_ui"),
                                                               dataTableOutput("df_KNN_pred_ui"),
                                                               plotOutput("plot_KNN_pred_ui")
                                                               ),
                                              conditionalPanel(condition = "input.model_sel == 'rf'",
                                                               withSpinner(verbatimTextOutput("rf_fit_ui"), type = 5),
                                                               withSpinner(verbatimTextOutput("rf_summary_ui"), type = 5),
                                                               plotOutput("rf_var_imp_plot"),
                                                               verbatimTextOutput("rf_MC_ui"),
                                                               verbatimTextOutput("rf_CT_ui"),
                                                               dataTableOutput("df_RF_pred_ui"),
                                                               plotOutput("plot_RF_pred_ui")
                                                               )
                                              ),
                                     tabPanel("Data Entry Prediction",
                                              h3("Enter a set of values to see what each model will predict"),
                                              p(strong("Note: Only predictors specified during model training will be used here.")
                                                ),
                                              fluidRow(
                                                  column(width = 4,
                                                         numericInput("i_mean", "Integrated Mean",
                                                                      min = -100, max = 200, value = 100),
                                                         numericInput("i_sd", "Integrated Standard Deviation",
                                                                      min = -100, max = 200, value = 100),
                                                         numericInput("i_exkur", "Integrated Excess Kurtosis",
                                                                      min = -100, max = 200, value = 100),
                                                         numericInput("i_skew", "Integrated Skew",
                                                                      min = -100, max = 200, value = 100),
                                                         
                                                  ),
                                                  column(width = 4,
                                                         numericInput("dmsnr_mean", "DMSNR Mean",
                                                                      min = -100, max = 200, value = 100),
                                                         numericInput("dmsnr_sd", "DMSNR Standard Deviation",
                                                                      min = -100, max = 200, value = 100),
                                                         numericInput("dmsnr_exkur", "DMSNR Excess Kurtosis",
                                                                      min = -100, max = 200, value = 100),
                                                         numericInput("dmsnr_skew", "DMSNR Skew",
                                                                      min = -100, max = 200, value = 100),
                                                  )
                                              ),
                                              
                                              fluidRow(
                                                  actionButton("single_test", "Click here to get prediction",  
                                                               class = "btn-success"),
                                                  verbatimTextOutput("single_pred_log_ui"),
                                                  verbatimTextOutput("single_pred_knn_ui"),
                                                  verbatimTextOutput("single_pred_rf_ui")
                                                  
                                              ),
                                             
                                              fluidRow(
                                                  h3("Reference Data"),
                                                  column(4,
                                                         p(strong("Total Data")
                                                           ),
                                                         tableOutput("sum_stats_total_ui")
                                                  ),
                                                  column(2,
                                                         p(strong("Pulsar Data")
                                                           ),
                                                         tableOutput("sum_stats_pulsar_ui")
                                                  ),
                                                  column(2,
                                                         p(strong("Non Pulsar Data")
                                                           ),
                                                         tableOutput("sum_stats_non_puls_ui")
                                                  )
                                                  
                                              )
                                              )
                         )
                 )
             )
        )
))

)