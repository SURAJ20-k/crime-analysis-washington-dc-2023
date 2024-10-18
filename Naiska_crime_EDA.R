library(dplyr)
library(ezids)
library(lubridate)
library(tigris)
library(tidycensus)
library(ggthemes)
library(ggplot2)
library(sf)
library(treemapify)

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
  mutate(across(c(SHIFT, METHOD, OFFENSE, WARD, ANC, DISTRICT, NEIGHBORHOOD_CLUSTER, VOTING_PRECINCT), as.factor)) %>% 
  mutate(across(c(REPORT_DAT, START_DATE, END_DATE), ~ ymd_hms(.x, tz = "America/New_York")))

crime_data_clean <- na.omit(crime_data_clean)

# Omitting the crime records that didn't happen in 2023
crime_data_clean <- crime_data_clean %>% 
  filter(year(START_DATE) >= 2023, year(END_DATE) >= 2023)

summary(crime_data_clean)

# EDA / Plotting 
# Crime d=Distribution by Months
crime_data_clean <- crime_data_clean %>%
  mutate(Month = factor(month(START_DATE, label = TRUE), levels = month.abb, ordered = TRUE))

crime_by_month <- crime_data_clean %>%
  group_by(Month) %>%
  summarise(Crime_Count = n()) %>%
  arrange(Month)

ggplot(crime_by_month, aes(x = Month, y = Crime_Count, group = 1)) +  # group = 1 for continuous line
  geom_line(color = 'blue', size = 1) +
  geom_point(color = 'red', size = 2) +
  labs(title = "Crime Counts by Month", x = "Month", y = "Number of Crimes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
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

crime_summary <- crime_data_clean %>% 
  group_by(OFFENSE) %>% 
  summarise(Crime_Count = n())

ggplot(crime_summary, aes(area=Crime_Count, fill=Crime_Count, label = paste(OFFENSE, Crime_Count, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", size = 10) +
  scale_fill_gradient(low = 'lightblue', high = 'darkblue') +
  labs(title = 'Crime by Offense Type', fill='Crijme_Count') +
  theme_minimal()

# Crime counts by WARD 
ggplot(crime_data_clean, aes(x = WARD)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Crime Counts by Ward", x = "Ward", y = "Count")

ggplot(crime_data_clean, aes(x = LONGITUDE, y = LATITUDE)) +
  geom_bin2d(bins = 30) +  # Use geom_density_2d() for contours instead of bins
  scale_fill_gradient(low = "lightyellow", high = "red") +
  labs(title = "Crime Density by Offense Type", x = "Longitude", y = "Latitude") +
  theme_minimal()


ggplot(crime_data_clean, aes(x = LONGITUDE, y = LATITUDE)) +
  geom_hex(bins = 30) +
  scale_fill_gradient(low = "lightyellow", high = "red") +
  labs(title = "Crime Density by Offense (Hexbin Map)", x = "Longitude", y = "Latitude") +
  theme_minimal()

# Drawing Maps
dc_tracts <- tracts(state = "dc")
dc_tracts %>% 
  ggplot() +
  geom_sf()

crime_data_clean$WARD <- as.character(crime_data_clean$WARD)
crime_data_clean$WARD <- paste("Ward", crime_data_clean$WARD)
  
dc_wards <- state_legislative_districts(state = "dc")

dc_wards %>% 
  ggplot() +
  geom_sf()  
  
ward_crime_counts <- crime_data_clean %>%
  group_by(WARD) %>%
  summarise(offense_count = n()) 
  

View(ward_crime_counts)

dc_ward_crime <- dc_wards %>% 
  left_join(ward_crime_counts, by = c("NAMELSAD" = "WARD"))

dc_data_map <- dc_ward_crime %>% 
  ggplot() +
  geom_sf(aes(fill = offense_count),
          color = "blue") +
  geom_sf_label(aes(label = paste0(NAMELSAD, 
                                   "\n",
                                   offense_count))) +
  scale_fill_distiller(palette = "Blues",
                       direction = 1) +
  labs(fill = "Crime Density") +
  theme_map() 
#  theme(legend.position = "none")
dc_data_map


