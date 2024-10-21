


project - washigton_crime_report
date-- 
  
  
  ```{r}

#1) load the data set 

crime_data <- read.csv("C:/Users/DELL/Documents/GitHub/crime-analysis-washington-dc-2023/Crime_Incidents_in_2023.csv")



cluster <- read.csv('C:/Users/DELL/Documents/GitHub/crime-analysis-washington-dc-2023/Neighborhood_Clusters.csv')



library(dplyr)
library(ezids)
library(lubridate)
library(tigris)
library(tidycensus)
library(ggthemes)
library(ggplot2)
library(sf)
library(treemapify)
library(gridExtra)


head(cluster)
cluster <- cluster %>% 
  select(NAME, NBH_NAMES)




# 2) load the require library 

library(ggplot2)
library(dplyr)


library(lubridate) # fro thr data manupulation 

library(sf)
library(leaflet) 


```




```{r}








