library(tidyverse)

cleaning_data_function <- function(data){
  cleanCode = data %>% select("ï..REF_DATE", "Violations", "Statistics", "VALUE") %>%
    rename(Date = ï..REF_DATE, Value = VALUE)
  return(cleanCode)
}


write.csv(cleaning_data_function(read.csv("3510017701_crimestats2000-2009.csv")), "3510017701_crimestats2000-2009.csv", row.names = FALSE)
write.csv(cleaning_data_function(read.csv("3510017701_crimestats2010-2019.csv")), "3510017701_crimestats2010-2019.csv", row.names = FALSE)
write.csv(cleaning_data_function(read.csv("3510017701_crimestats2000-2019.csv")), "3510017701_crimestats2000-2019.csv", row.names = FALSE)
