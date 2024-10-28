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
library(patchwork)
library(geosphere)

# Loading the dataset
crime_data <- read.csv('Crime_Incidents_in_2023.csv')
xkabledplyhead(crime_data)
str(crime_data)
xkablesummary(crime_data)

cluster <- read.csv('Neighborhood_Clusters.csv')
cluster <- cluster %>% 
  select(NAME, NBH_NAMES)

police_data <- read.csv('Police_Stations.csv')

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

crime_data_clean <- crime_data_clean %>% 
  left_join(cluster, by = c("NEIGHBORHOOD_CLUSTER" = "NAME"))

summary(crime_data_clean)

# EDA / Plotting 
# Crime Distribution by Months
crime_data_clean <- crime_data_clean %>%
  mutate(Month = factor(month(START_DATE, label = TRUE), levels = month.abb, ordered = TRUE))

crime_by_month <- crime_data_clean %>%
  group_by(Month) %>%
  summarise(Crime_Count = n()) %>%
  arrange(Month)

crime_by_month

ggplot(crime_by_month, aes(x = Month, y = Crime_Count, group = 1)) +  # group = 1 for continuous line
  geom_line(color = 'blue', size = 1) +
  geom_point(color = 'red', size = 2) +
  labs(title = "Crime Counts by Month", x = "Month", y = "Number of Crimes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(crime_data_clean, aes(x = Month, fill=SHIFT)) +
  geom_bar(position = 'dodge') +
  labs(title = "Crime Count by Month and Shift",
       x = "Month",
       y = "Crime Count") +
  scale_fill_manual(values = c("DAY" = "lightblue",    
                               "EVENING" = "orange",  
                               "MIDNIGHT" = "navy")) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.title = element_blank())
  
crime_oct <- crime_data_clean %>% 
  filter(Month == "Oct") %>% 
  mutate(day  = day(START_DATE)) %>% 
  group_by(day) %>% 
  summarise(Crime_Count = n())
  
ggplot(crime_oct, aes(x = day, y=Crime_Count)) +
  geom_line(color = 'blue', size=1) +
  geom_point(color='red', size=2) +
  labs(title = "Crime Distribution in October") +
  theme_minimal()

# Rock the Park DC WKNDR 2023
# Address: 1315 I Street NW.

crime_oct_22 <- crime_data_clean %>% 
  filter(Month == "Oct" & day(START_DATE) == 22) %>% 
  group_by(NEIGHBORHOOD_CLUSTER, NBH_NAMES) %>% 
  summarise(crime_count = n()) %>%              
  arrange(desc(crime_count))

xkabledplyhead(crime_oct_22)

crime_oct_22_add <- crime_data_clean %>% 
  filter(Month == "Oct" & day(START_DATE) == 22 & NEIGHBORHOOD_CLUSTER %in% c("Cluster 2", 
                                                                            "Cluster 3",
                                                                            "Cluster 18",
                                                                            "Cluster 6",
                                                                            "Cluster 1")) %>% 
  select(NEIGHBORHOOD_CLUSTER, BLOCK)

crime_oct_22_add
# Top 5 Clusters that had the highest crime on Oct 22nd is in NW area. Same as Rock the Park 2023
# Comparing warm and cold/chill months
crime_by_month <- crime_by_month %>%
  mutate(Weather = ifelse(Month %in% c("May", "Jun", "Jul", "Aug"), "Warm", "Not Warm"))

# Perform T-Test
t_test_result <- t.test(Crime_Count ~ Weather, data = crime_by_month)
print(t_test_result)

# Test on association between categorical variables, such as month (warmer vs. cooler months) and the time of day (shifts). 
# Perform Chi-Squared Test
# Null Hypothesis (H0): The null hypothesis states that there is no association between the month of the crime and the shift (time of day) in which it occurred. In other words, crime occurrences are independent of these variables.

table_month_shift <- table(crime_data_clean$Month, crime_data_clean$SHIFT)
chisq.test(table_month_shift)

# The p-value is extremely small (< 2.2e-16)
# Indicate a statistically significant association between the month of the crime and the time of day (shift) when it occurred.


# Crime counts by WARD 
ggplot(crime_data_clean, aes(x = WARD)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Crime Counts by Ward", x = "Ward", y = "Count")

#ggplot(crime_data_clean, aes(x = LONGITUDE, y = LATITUDE)) +
#  geom_bin2d(bins = 30) +  # Use geom_density_2d() for contours instead of bins
#  scale_fill_gradient(low = "lightyellow", high = "red") +
#  labs(title = "Crime Density by Offense Type", x = "Longitude", y = "Latitude") +
#  theme_minimal()


#ggplot(crime_data_clean, aes(x = LONGITUDE, y = LATITUDE)) +
#  geom_hex(bins = 30) +
#  scale_fill_gradient(low = "lightyellow", high = "red") +
#  labs(title = "Crime Density by Offense (Hexbin Map)", x = "Longitude", y = "Latitude") +
#  theme_minimal()

# Drawing Maps
#dc_tracts <- tracts(state = "dc")
#dc_tracts %>% 
#  ggplot() +
#  geom_sf()

crime_data_clean$WARD <- as.character(crime_data_clean$WARD)
crime_data_clean$WARD <- paste("Ward", crime_data_clean$WARD)

dc_wards <- state_legislative_districts(state = "dc")

#dc_wards %>% 
#  ggplot() +
#  geom_sf()  
ward_crime_counts <- crime_data_clean %>%
  group_by(WARD) %>%
  summarise(offense_count = n()) 

ward_crime_counts

dc_ward_crime <- dc_wards %>% 
  left_join(ward_crime_counts, by = c("NAMELSAD" = "WARD"))

ggplot(dc_ward_crime) +
  geom_sf(aes(fill = offense_count),
          color = "black") +
  geom_sf_label(aes(label = paste0(NAMELSAD, 
                                   "\n",
                                   offense_count))) +
  scale_fill_gradient(low = "lightyellow", high = "red") +
  labs(fill = "Crime Density") +
  theme_map() 

ggplot() +
  geom_hex(data = crime_data_clean, aes(x = LONGITUDE, y = LATITUDE), bins = 30) +  # Crime density
  scale_fill_gradient(low = "lightyellow", high = "red") +  # Fill gradient for crime density
  geom_sf(data = dc_wards, fill = NA, color = "black", size = 1.2) +
  labs(title = "Crime Density by Offense (Hexbin Map) with DC Wards", 
       x = "Longitude", 
       y = "Latitude") +
  theme_minimal() +
  theme(panel.grid = element_blank())


#ggplot() +
#  geom_hex(data = crime_data_clean, aes(x = LONGITUDE, y = LATITUDE), bins = 30) +  # Crime density
#  scale_fill_gradient(low = "lightyellow", high = "red") +  # Fill gradient for crime density
#  geom_sf(data = dc_tracts, fill = NA, color = "black", size = 1.2) +
#  labs(title = "Crime Density by Offense (Hexbin Map) with DC Wards", 
#       x = "Longitude", 
#       y = "Latitude") +
#  theme_minimal() +
#  theme(panel.grid = element_blank())

# Crime by cluster  
cluster_crime <- crime_data_clean %>% 
  group_by(NEIGHBORHOOD_CLUSTER) %>% 
  summarise(Crime_Count = n())

dc_clusters <- st_read("Neighborhood_Clusters.geojson") %>% 
  left_join(cluster_crime, by = c("NAME" = "NEIGHBORHOOD_CLUSTER")) %>% 
  mutate(cluster_number = gsub("\\D", "", NAME))

dc_clusters$Crime_Count[is.na(dc_clusters$Crime_Count)] <- 0

dc_clusters %>% 
  arrange(desc(Crime_Count)) %>% 
  select(NAME, NBH_NAMES, Crime_Count) %>% 
  head()

ggplot(data = dc_clusters) +
  geom_sf(aes(fill = Crime_Count), color = "black", size = 0.5) +
  geom_sf_text(aes(label = cluster_number), color = 'darkblue') +
  labs(title = "Neighborhood Clusters in Washington, DC",
       fill = "Total Crimes") +
  theme_minimal() +
  scale_fill_gradient(low = "lightyellow", high = "red")

ggplot() +
  geom_hex(data = crime_data_clean, aes(x = LONGITUDE, y = LATITUDE), bins = 30) +
  scale_fill_gradient(low = "lightyellow", high = "red") +  # Fill gradient for crime density
  geom_point(data = police_data, aes(x = LONGITUDE, y = LATITUDE), color = "blue", size = 1, stroke = 1, alpha = 0.7, shape = 4) +# Crime density
  geom_sf(data = dc_clusters, fill = NA, color = "black", size = 1.2) +
  labs(title = "Crime Density by Offense (Hexbin Map) with DC Wards", 
       x = "Longitude", 
       y = "Latitude") +
  theme_minimal() +
  theme(panel.grid = element_blank())


# Crime Distribution by Shift
ggplot(crime_data_clean) +
  geom_bar(aes(x=SHIFT),
           fill='steelblue') +
  labs(title = "Crime Distribution by Shift", x = "Shift", y = "Count")

crime_by_shift <- crime_data_clean %>% 
  group_by(SHIFT) %>% 
  summarise(Crime_Count = n())

crime_by_shift

ggplot(crime_data_clean, aes(x=SHIFT, fill=OFFENSE)) +
  geom_bar(position = 'dodge') +
  labs(title = 'Crime Count by Shift and Offense Type',
       x = 'Shift',
       y = 'Count') +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

# Crime spot police test

crime_coords <- as.matrix(crime_data_clean[, c("LONGITUDE", "LATITUDE")])
police_coords <- as.matrix(police_data[, c("LONGITUDE", "LATITUDE")])

# Calculate distance matrix between crime and police locations (in meters)
distance_matrix <- distm(crime_coords, police_coords, fun = distHaversine)

# Find nearest police station for each crime point
nearest_distance <- apply(distance_matrix, 1, min)

# Add distance to the crime dataset
crime_data_clean <- crime_data_clean %>%
  mutate(Nearest_Police_Distance = nearest_distance)

crime_counts <- crime_data_clean %>%
  group_by(NEIGHBORHOOD_CLUSTER) %>%
  summarise(Crime_Count = n(), .groups = 'drop')


average_distances <- crime_data_clean %>%
  group_by(NEIGHBORHOOD_CLUSTER) %>%
  summarise(Nearest_Police_Distance = mean(Nearest_Police_Distance, na.rm = TRUE), .groups = 'drop')  

crime_summary <- left_join(crime_counts, average_distances, by = "NEIGHBORHOOD_CLUSTER")


# Plot the number of crimes by offense method
gun_plot <- ggplot(subset(crime_data_clean, METHOD == "GUN"), 
       aes(x = OFFENSE)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cor_test <- cor.test(crime_summary$Crime_Count, crime_summary$Nearest_Police_Distance)
print(cor_test)


# Linear model
model <- lm(Crime_Count ~ Nearest_Police_Distance, data = crime_summary)

# Summarize the model
summary(model)

knife_plot <- ggplot(subset(crime_data_clean, METHOD == "KNIFE"), 
                   aes(x = OFFENSE)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

others_plot <- ggplot(subset(crime_data_clean, METHOD == "OTHERS"), 
                   aes(x = OFFENSE)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(gun_plot, knife_plot, others_plot, ncol = 3)

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



