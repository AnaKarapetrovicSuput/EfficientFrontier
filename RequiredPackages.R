#Install required packages prior to install skript
install.packages("tinytex")
tinytex::install_tinytex()
source("http://rnbeads.org/data/install.R")
install.packages("tidyquant")
install.packages("plotly")
install.packages("timetk")
install.packages("tidyr")
install.packages("dplyr")
install.packages("quantmod") # Bugfix for old yahoo finance URL from package "tidyquant"
install.packages("stringr")