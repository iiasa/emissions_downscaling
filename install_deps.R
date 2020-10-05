libs <- c( "ggplot2", "lubridate", "plyr", "dplyr", "stringr", "readxl", "zoo", "tidyr", "ncdf4", "sp", "geosphere" )

new.packages <- libs[!(libs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")