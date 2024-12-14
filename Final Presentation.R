library(readr)
library(tidyverse)
prosecuting <- read_excel("data/election results/detail2024.xlsx", 
                          sheet = "24",skip=2) 
prosecuting <- prosecuting[-563, ]
colnames(prosecuting) <- c("Precinct","Registered_voters","Early_CP","Election_CP",
                           "Total_CP","Early_MP","Election_MP",
                           "Total_MP","Total" )

president <- read_excel("data/election results/detail2024.xlsx", 
                          sheet = "2",skip=2)
president <- president[-563, ]
colnames(president) <- c("Precinct","Registered_voters","Early_KH","Election_KH",
                           "Total_KH","Early_DT","Election_DT",
                           "Total_DT","Total" )
