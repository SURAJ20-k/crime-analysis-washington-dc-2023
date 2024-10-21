


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
# check the data structre 


# dataset(data) is a data frame 
# some time data set name is over written and we have to make sure that we have to 
# convert that data set  in to data frame  

crime_data <- as.data.frame(crime_data)



str(crime_data)

head(crime_data)

```
from the is.null operator , we got that there is no record in the OCTO_RECORD_ID column 

total 34215 records are missing from the records. 


```{r}
colnames(crime_data)

colSums(is.na(crime_data))

# Check for duplicate rows from the data set 
duplicates <- crime_data[duplicated(crime_data), ]
nrow(duplicates)

head(crime_data)




```
```{r}

sum(!is.na(crime_data$PSA))          # Check rows with PSA not missing
sum(!is.na(crime_data$CENSUS_TRACT)) # Check rows with CENSUS_TRACT not missing
sum(!is.na(crime_data$DISTRICT))     # Check rows with DISTRICT not missing
sum(is.na(crime_data$WARD))          # Check rows with WARD missing





```



```{r}


# we have to delete these null values , we had founded null from values ward , (census_tract) (geographical region ) , district,  psa (public safty areas )

#  we have the geographical data as well as we got the null vlaues from the geographical data so we have to 
#  delete it. it is appropriate for to delete and removing these small valmum from the data will not affect the 
# data set. 



crime_data1 <- crime_data[
  !is.na(crime_data$PSA) & 
    !is.na(crime_data$CENSUS_TRACT) & 
    !is.na(crime_data$DISTRICT) 
  , 
]

nrow(crime_data1)


# we are geeting the result numeric-0 so that means we don't have a null value in the data set. 
# now 
```
```{r}

# check duplicate values 
# Ensure crime_data1 is a data frame
crime_data1 <- as.data.frame(crime_data1)

duplicate_crime_data <- crime_data1[duplicated(crime_data1), ]
nrow(duplicate_crime_data)

head(crime_data1)
```
so we don't have duplicate vlaues in the data set also . 
 
 now our data set is net and clean . 
 we are ready for the data transformation .
 
 now we have to convert some rows of data in to some other formate beacuse of our need .
 
 we some time trand=sform the solumns as according to our use of that data .
```{r}

head(crime_data1)
crime_data1$REPORT_DAT <- as.Date(crime_data1$REPORT_DAT, formate="%m/%d/%Y")

# extract the year and month also for future plots

crime_data1$Year <- format(crime_data1$REPORT_DAT,"%Y")
crime_data1$Month <- format(crime_data1$REPORT_DAT,"%m")

head(crime_data1)

```
 
 we can see that , 2 new columns are added at the end of the all columns. now readjust columns, add these columns in the data frame after the REPOST_DAT. 
 
 rearrange the columns and move the columns YEAR and Month after the report year .
 
```{r}
crime_data1 <- crime_data1 %>%
    select(1:which(colnames(crime_data1) == "REPORT_DAT"), Year, Month, everything())

head(crime_data1)
str(crime_data1)
```






