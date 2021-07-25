# 558_P3

This application was developed using R version 4.1.0 and R Studio version 1.4.1717.

To run this application you will need the following libraries:

```
library(shiny)             - version 1.6.0
library(tidyverse)         - version 1.3.1   
library(shinythemes)       - version 1.2.0  
library(forcats)           - version 0.5.1  
library(MASS)              - version 7.3-54    
library(caret)             - version 6.0-88    
library(DT)                - version 0.18    
library(matrixStats)       - version 0.59.0  
library(GGally)            - version 2.1.2  
library(shinycssloaders)   - version 1.0.0  
library(AMR)               - version 1.7.1
```

Running the following code will check for and install the required libraries if necessary:

```
packages <- c("shiny", "tidyverse", "shinythemes", "forcats", "MASS", 
              "caret", "DT", "matrixStats", "GGally", "shinycssloaders", 
              "AMR)

libraries <- function(packages){
  for(i in packages){
    if(!require(package, character.only = TRUE)){
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
}
```

To run this application directly from github, run the following in an R session:
```
shiny::runGitHub('hkopanski/558_P3', ref = 'main')
```
