library(tidyverse)

cleaning_data_function <- function(data){
  cleanCode = data %>% select("REF_DATE", "Violations", "Statistics", "VALUE")
  return(cleanCode)
}

#cleaning_data_function(read.csv("3510017701_crimestats2000-2009.csv"))