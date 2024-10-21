


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


Now we have to normalize the data set 
 Beacuse 
 1) uniformality across the featurre- in the data set we have geo location data , date time data ,character data (shift ), numerical data(disctrict ) some pincode , so our data have SACLE  are drastically different , if we don;t normalize , some time anlalysis  will affect 
 
 2)  it will help in the improving  the speed in analysis 
 3) avoid the feature domination effect some feature are x, y co ordinate values are greater than the ward number , so to avioid these domination we have to noramzalise the data set . 
 
 
```{r}
# select the columns , on whcih we have to process the normalize the data. 

# libriry "dplyr" require for the normalize the data . 



# Select numerical columns that need to be normalized
# Exclude categorical columns like OFFENSE, SHIFT, METHOD, etc.
# Select only numeric columns that should be normalized
numeric_columns <- crime_data1 %>%
  select(X, Y, XBLOCK, YBLOCK, LATITUDE, LONGITUDE)  # Exclude WARD, DISTRICT, and PSA if they are categorical

# Apply Min-Max normalization for each numeric column
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Apply normalization to the numeric columns
normalized_numeric_columns <- as.data.frame(lapply(numeric_columns, normalize))

# Add back the non-numeric columns to the normalized dataset
crime_data2 <- bind_cols(crime_data1 %>% select(-X, -Y, -XBLOCK, -YBLOCK, -LATITUDE, -LONGITUDE), 
                          normalized_numeric_columns)



str(crime_data2)

# view the first 5 rows of the data set 
head(crime_data2)


```
 
 Now start the EDA- Exploratory Data Analysis
 
 summary stastics 
 
```{r}

summary(crime_data2)
```


summary provide the  stastics information for each varible in the dataset. '
```{r}

# Summarize total offenses by ward
offense_total_by_ward <- crime_data2 %>%
  group_by(WARD) %>%
  summarise(Total_Offenses = n())

# View the summarized data
print(offense_total_by_ward)

```


```{r}
offence_count <- crime_data2 %>%
  group_by(OFFENSE)%>%
  summarise(count=n())

print(offence_count)



```
```{r}
# Group by SHIFT and summarize the count of offenses
offence_by_shift <- crime_data2 %>%
  group_by(SHIFT) %>%
  summarise(Count = n(), .groups = 'drop')

# Check the structure of the resulting table
str(offence_by_shift)

# Print the summary table
print(offence_by_shift)


```




crime count by shift 

count by method 

```{r}
# Check if SHIFT is a factor or convert it to one if needed
crime_data2$SHIFT <- as.factor(crime_data2$SHIFT)

library(dplyr)

# Ensure SHIFT is a factor
crime_data2$SHIFT <- as.factor(crime_data2$SHIFT)

# Group by SHIFT and OFFENSE and summarize
offence_by_shift <- crime_data2 %>%
  group_by(SHIFT, OFFENSE) %>%
  summarise(Count = n(), .groups = 'drop')

# Check the structure of the resulting data frame
str(offence_by_shift)
head(offence_by_shift)

# Print the summary table
print(offence_by_shift)


```


```{r}
# Check the structure and contents of the `offence_by_shift`
str(offence_by_shift)
head(offence_by_shift)

# Print to see if the data frame has been populated correctly
print(offence_by_shift)


# Ensure SHIFT is a factor
crime_data2$SHIFT <- as.factor(crime_data2$SHIFT)

# Group by SHIFT and summarize the count of offenses
offence_by_shift <- crime_data2 %>%
  group_by(SHIFT, OFFENSE) %>%
  summarise(Count = n(), .groups = 'drop')

# Check the structure of the resulting table
str(offence_by_shift)

# Print the summary table
print(offence_by_shift)


```


```{r}
# Summarize the count of offenses by district
OFFENCE_BY_DISTRICT <- crime_data2 %>%
  group_by(DISTRICT) %>%
  summarise(count = n(), .groups = 'drop')  # 'drop' will ungroup after summarizing

# Print the summarized table
print(OFFENCE_BY_DISTRICT)

```

```{r}




```




```{r}






