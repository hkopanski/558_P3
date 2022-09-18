# Pulsar Classification Project

Welcome to my pulsar classification app! I developed this app to allow users to view and interact with data compiled from High Time Resolution Universe Survey. The data contains derived signal information from roughly 18,000 neutron stars, some of them being pulsars. The app goes into a little more detail on what a pulsar is, but the main purpose of the app to create an interactive experience for a data oriented user. Within the app, the user will be able to scroll through the data, view it in various types of charts and summary tables, and apply both supervised and unsupervised learning techniques. The goal of the app is to show how existing data can be used to train and create classification models to be able to detect whether an object is a regular neutron star, or the more rare pulsar type. Thanks and I hope you enjoy using the app, because I definately enjoyed developing it!

This application was developed using R version 4.1.0 and R Studio version 1.4.1717.

To run this application you will need the following libraries:

```
library(shiny)             - version 1.6.0
library(tidyverse)         - version 1.3.1   
library(shinythemes)       - version 1.2.0  
library(forcats)           - version 0.5.1  
library(caret)             - version 6.0-88    
library(DT)                - version 0.18    
library(matrixStats)       - version 0.59.0  
library(GGally)            - version 2.1.2  
library(shinycssloaders)   - version 1.0.0  
library(AMR)               - version 1.7.1
```

Running the following code will check for and install the required libraries if necessary:

```
packages <- c("shiny", "tidyverse", "shinythemes", "forcats", 
              "caret", "DT", "matrixStats", "GGally", "shinycssloaders", 
              "AMR")


libraries <- function(p){
  
  missing_lib <- c()
  
  for(i in p){
    
    if(i %in% rownames(installed.packages()) == FALSE){
      
      missing_lib <- append(missing_lib, i)
      
    }
  }
  
  install.packages(missing_lib, dependencies = TRUE)
}

libraries(packages)
```

To run this application directly from github, run the following in an R session:
```
shiny::runGitHub('hkopanski/558_P3', ref = 'main')
```
