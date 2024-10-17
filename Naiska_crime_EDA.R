library(dplyr)
library(ezids)
library(lubridate)
library(ggplot2)

# Loading the dataset
crime_data <- read.csv('Crime_Incidents_in_2023.csv')
xkabledplyhead(crime_data)
str(crime_data)
xkablesummary(crime_data)

# Data cleaning
colSums(is.na(crime_data))
sum(crime_data$BID == "" | crime_data$BID == " ")

crime_data_clean <- crime_data %>% 
  select(-OCTO_RECORD_ID, -BID) %>% 
  mutate(across(c(SHIFT, METHOD, OFFENSE, WARD, ANC, DISTRICT, NEIGHBORHOOD_CLUSTER, VOTING_PRECINCT, BID), as.factor)) %>% 
  mutate(across(c(REPORT_DAT, START_DATE, END_DATE), ~ ymd_hms(.x, tz = "America/New_York")))

crime_data_clean <- na.omit(crime_data_clean)

# Omitting the crime records that didn't happen in 2023
crime_data_clean <- crime_data_clean %>% 
  filter(year(START_DATE) >= 2023, year(END_DATE) >= 2023)

summary(crime_data_clean)

# EDA / Plotting 
# Crime d=Distribution by Months
crime_data_clean <- crime_data_clean %>% 
  mutate(Month = month(START_DATE, label=TRUE))

crime_by_month <- crime_data_clean %>% 
  
  

# Crime Distribution by Shift
ggplot(crime_data_clean) +
  geom_bar(aes(x=SHIFT),
           fill='steelblue') +
  labs(title = "Crime Distribution by Shift", x = "Shift", y = "Count")

# Plot the number of crimes by offense type
ggplot(crime_data_clean) +
  geom_bar(aes(x = OFFENSE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Crime Counts by Offense Type in 2023", x = "Offense Type", y = "Count")

# Crime counts by WARD 
ggplot(crime_data_clean, aes(x = WARD)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Crime Counts by Ward", x = "Ward", y = "Count")

# geographical analysis
ggplot(crime_data_clean, aes(x = LONGITUDE, y = LATITUDE)) +
  geom_point(alpha = 0.3) +
  labs(title = "Crime Incidents in Washington, DC", x = "Longitude", y = "Latitude")
