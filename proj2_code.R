library(dplyr)
library(ezids)
library(lubridate)
library(tigris)
library(ggthemes)
library(ggplot2)
library(sf)
library(treemapify)
library(gridExtra)
library(patchwork)
library(geosphere)
library(readxl)
library(rpart)
library(rpart.plot)
library(caret)
library(corrplot)
library(rattle)
# ---------------------- Data -----------------------------------------
# Loading the crime dataset
crime_data <- read.csv('Crime_Incidents_in_2023.csv')
xkabledplyhead(crime_data, title = 'First 5 Rows of the Crime Dataset')
str(crime_data)
xkablesummary(crime_data, title = 'Summary Statistics of the Crime Dataset')

cluster <- read.csv('Neighborhood_Clusters.csv')
cluster <- cluster %>% 
  select(NAME, NBH_NAMES)
xkabledplyhead(cluster, title = 'Neighborhood Cluster Names')

police_data <- read.csv('Police_Stations.csv')
xkabledplyhead(police_data,title = 'DC Police Station Dataset')


night_data <- read_sf('Night_Club.geojson')
xkabledplyhead(night_data)
night_sf <- st_as_sf(night_data, coords = c("XCOORD", "YCOORD"), crs = 2248)
# Transform coordinates to WGS84 (EPSG:4326)
night_sf_transformed <- st_transform(night_sf, crs = 4326)
# Extract the transformed coordinates and add them back to the data frame
night_data$longitude <- st_coordinates(night_sf_transformed)[, 1]
night_data$latitude <- st_coordinates(night_sf_transformed)[, 2]


ward_df <- read.csv('warddata.csv')
ward_df <- ward_df %>% 
  rename(total_population = DP05_0001E) %>% 
  select(c('NAMELSAD', 'total_population'))
xkabledplyhead(ward_df, nrow(ward_df),title = 'DC Ward Population Dataset')

cluster_pop <- read_excel('Population_Cluster.xlsx')
View(cluster_pop)


# ----------------------------- Data Cleaning ------------
colSums(is.na(crime_data))
sum(crime_data$BID == "" | crime_data$BID == " ")
crime_data_clean <- crime_data %>% 
  select(-OCTO_RECORD_ID, -BID) %>% 
  mutate(WARD = paste("Ward", WARD)) %>% 
  mutate(across(c(SHIFT, METHOD, OFFENSE, WARD, ANC, DISTRICT, NEIGHBORHOOD_CLUSTER, VOTING_PRECINCT), as.factor)) %>% 
  mutate(across(c(REPORT_DAT, START_DATE, END_DATE), ~ ymd_hms(.x, tz = "America/New_York")))
crime_data_clean <- na.omit(crime_data_clean)

crime_data_clean <- crime_data_clean %>% 
  filter(year(START_DATE) >= 2023, year(END_DATE) >= 2023)

crime_data_clean <- crime_data_clean %>% 
  left_join(cluster, by = c("NEIGHBORHOOD_CLUSTER" = "NAME"))

# ---------------------- EDA ------------------------
crime_data_clean <- crime_data_clean %>%
  mutate(Month = factor(month(START_DATE, label = TRUE), levels = month.abb, ordered = TRUE))

crime_by_month <- crime_data_clean %>%
  group_by(Month) %>%
  summarise(Crime_Count = n()) %>%
  arrange(Month)

ggplot(crime_by_month, aes(x = Month, y = Crime_Count, group = 1)) +  # group = 1 for continuous line
  geom_line(color = 'blue', size = 1) +
  geom_point(color = 'red', size = 2) +
  labs(title = "Crime Distribution by Month", x = "Month", y = "Number of Crimes") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(crime_data_clean, aes(x = Month, fill=SHIFT)) +
  geom_bar(position = 'dodge') +
  labs(title = "Crime Distribution by Month and Shift",
       x = "Month",
       y = "Number of Crimes") +
  scale_fill_manual(values = c("DAY" = "lightblue",    
                               "EVENING" = "orange",  
                               "MIDNIGHT" = "navy")) +  
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

crime_oct <- crime_data_clean %>% 
  filter(Month == "Oct") %>% 
  mutate(day  = day(START_DATE)) %>% 
  group_by(day) %>% 
  summarise(Crime_Count = n())

ggplot(crime_oct, aes(x = day, y=Crime_Count)) +
  geom_line(color = 'blue', size=1) +
  geom_point(color='red', size=2) +
  labs(title = "Crime Distribution in October",
       x = "Day",
       y = 'Number of Crimes') +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 20)) 


# ------------------ Map --------------------
ward_crime_p <- ggplot(crime_data_clean, aes(x = WARD)) +
  geom_bar(fill = "steelblue", color = 'black') +
  labs(title = "Crime Distribution by Count VS Per Capita", x = "Ward", y = "Number of Crimes") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1))

ward_crime_counts <- crime_data_clean %>%
  group_by(WARD) %>%
  summarise(offense_count = n()) 

crime_per_capita <- ward_crime_counts %>%
  left_join(ward_df, by = c("WARD" = "NAMELSAD")) %>% 
  mutate(crime_rate_per_capita = offense_count / total_population)


ward_crime_pop_p <- ggplot(crime_per_capita, aes(x = WARD, y = crime_rate_per_capita)) +
  geom_bar(stat = "identity", fill = "darkgreen", color = 'black') +
  labs(title = ' ', x = "Ward", y = "Crime Rate Ward Pop.") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(ward_crime_p, ward_crime_pop_p, ncol = 2)


dc_wards <- state_legislative_districts(state = "dc")
dc_ward_crime <- dc_wards %>% 
  left_join(ward_crime_counts, by = c("NAMELSAD" = "WARD"))


ward_map <- ggplot(dc_ward_crime) +
  geom_sf(aes(fill = offense_count),
          color = "black") +
  geom_sf_label(aes(label = paste0(NAMELSAD, 
                                   "\n",
                                   offense_count))) +
  scale_fill_gradient(low = "lightyellow", high = "red") +
  labs(title = 'DC Crime Density Map with Count and Per Capita by Wards',
       fill = "Crime Density") +
  theme_map() +
  theme(plot.title = element_text(face = "bold", size = 15))

dc_ward_crime_pop <- dc_wards %>% 
  left_join(crime_per_capita, by = c("NAMELSAD" = "WARD"))

# Plotting the crime density map using crime per capita
ward_map_pop <- ggplot(dc_ward_crime_pop) +
  geom_sf(aes(fill = crime_rate_per_capita), color = "black") +  
  geom_sf_label(aes(label = paste0(NAMELSAD, "\n", 
                                   round(crime_rate_per_capita, 2)))) + 
  scale_fill_gradient(low = "lightyellow", high = "red") +
  labs(title = ' ',
       fill = "Crime Rate per Capita") +
  theme_map() +
  theme(plot.title = element_text(face = "bold", size = 15))

grid.arrange(ward_map, ward_map_pop, ncol = 2)


ggplot() +
  geom_hex(data = crime_data_clean, aes(x = LONGITUDE, y = LATITUDE), bins = 30) +  # Crime density
  scale_fill_gradient(low = "lightyellow", high = "red") +  
  geom_point(data = police_data, aes(x = LONGITUDE, y = LATITUDE), color = "blue", size = 1, stroke = 1, alpha = 0.7, shape = 4) +
  geom_sf(data = dc_wards, fill = NA, color = "black", size = 1.2) +
  labs(title = "Crime Density Map with Police Station Points", 
       x = "Longitude", 
       y = "Latitude",
       fill = 'Crime Density') +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 15),
        panel.grid = element_blank())










cluster_crime <- crime_data_clean %>% 
  group_by(NEIGHBORHOOD_CLUSTER) %>% 
  summarise(Crime_Count = n())

dc_clusters <- st_read("Neighborhood_Clusters.geojson") %>% 
  left_join(cluster_crime, by = c("NAME" = "NEIGHBORHOOD_CLUSTER")) %>% 
  mutate(cluster_number = gsub("\\D", "", NAME))


dc_clusters$Crime_Count[is.na(dc_clusters$Crime_Count)] <- 0

highest_clusters <- dc_clusters %>% 
  arrange(desc(Crime_Count)) %>% 
  select(NAME, NBH_NAMES, Crime_Count) %>% 
  rename(CLUSTER_NUMBER = NAME,
         NEIGHBORHOOD = NBH_NAMES, 
         CRIME_COUNT = Crime_Count)
xkabledplyhead(highest_clusters, nrow(highest_clusters), title = "Neighborhood Clusters with the Highest Crime Count")


ggplot(data = dc_clusters) +
  geom_sf(aes(fill = Crime_Count), color = "black", size = 0.5) +
  geom_sf_text(aes(label = cluster_number), color = 'darkblue') +
  scale_fill_gradient(low = "lightyellow", high = "red") +
  labs(title = "Crime Distribution by Neighborhood Clusters",
       x = 'Longitude',
       y = 'Latitude',
       fill = "Crime Density") +
  theme_map() +
  theme(plot.title = element_text(face = "bold", size = 15))

ggplot() +
  geom_hex(data = crime_data_clean, aes(x = LONGITUDE, y = LATITUDE), bins = 30) +
  scale_fill_gradient(low = "lightyellow", high = "red") +  # Fill gradient for crime density
  geom_point(data = police_data, aes(x = LONGITUDE, y = LATITUDE), color = "blue", size = 1, stroke = 1, alpha = 0.7, shape = 4) +# Crime density
  geom_sf(data = dc_clusters, fill = NA, color = "black", size = 1.2) +
  labs(title = "Crime Density Map with Police Station Points", 
       x = "Longitude", 
       y = "Latitude",
       fill = 'Crime Density') +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 15),
        panel.grid = element_blank())


night_ward_plot <- ggplot() +
  geom_hex(data = crime_data_clean, aes(x = LONGITUDE, y = LATITUDE), bins = 30) +
  scale_fill_gradient(low = "lightyellow", high = "red") +  # Fill gradient for crime density
  geom_point(data = night_data, aes(x = longitude, y = latitude), color = "blue", size = 1, stroke = 1, alpha = 0.7, shape = 4) +# Crime density
  geom_sf(data = dc_wards, fill = NA, color = "black", size = 1.2) +
  labs(title = "Crime Density Map with Night Life Facility Points", 
       x = "Longitude", 
       y = "Latitude",
       fill = 'Crime Density') +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 15),
        panel.grid = element_blank())
ggsave('night_ward_plot.jpg', night_ward_plot, height = 6, width = 6)

night_cluster_plot <- ggplot() +
  geom_hex(data = crime_data_clean, aes(x = LONGITUDE, y = LATITUDE), bins = 30) +
  scale_fill_gradient(low = "lightyellow", high = "red") +  # Fill gradient for crime density
  geom_point(data = night_data, aes(x = longitude, y = latitude), color = "blue", size = 1, stroke = 1, alpha = 0.7, shape = 4) +# Crime density
  geom_sf(data = dc_clusters, fill = NA, color = "black", size = 1.2) +
  labs(title = "Crime Density Map with Night Life Facility Points", 
       x = "Longitude", 
       y = "Latitude",
       fill = 'Crime Density') +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 15),
        panel.grid = element_blank())
ggsave('night_cluster_plot.jpg', night_cluster_plot, height = 6, width = 6)


crime_coords <- as.matrix(crime_data_clean[, c("LONGITUDE", "LATITUDE")])
night_data <- night_data %>%
  st_drop_geometry()
night_coords <- as.matrix(night_data[, c("longitude", "latitude")])

# Calculate distance matrix between crime and police locations (in meters)
distance_matrix <- distm(crime_coords, night_coords, fun = distHaversine)

# Find nearest police station for each crime point
nearest_distance <- apply(distance_matrix, 1, min)

# Add distance to the crime dataset
crime_data_clean <- crime_data_clean %>%
  mutate(Nearest_NightClub_Distance = nearest_distance)
View(crime_data_clean)


cluster_pop <- cluster_pop %>% 
  select(c(NC_Cluster, Total)) %>% 
  rename(Cluster_Population = Total)

ward_df <- ward_df %>% 
  rename(Ward_Population = total_population)

crime_data_clean <- left_join(crime_data_clean, ward_df, by = c("WARD" = "NAMELSAD"))
crime_data_clean <- left_join(crime_data_clean, cluster_pop, by = c("NEIGHBORHOOD_CLUSTER" = "NC_Cluster"))

xkabledplyhead(crime_data_clean)

crime_aggregated <- crime_data_clean %>%
  group_by(NEIGHBORHOOD_CLUSTER, Month, SHIFT) %>%
  summarize(
    Crime_Count = n(),  # Target variable
    Cluster_Population = first(Cluster_Population),
    Avg_NightClub_Distance = mean(Nearest_NightClub_Distance, na.rm = TRUE),
    .groups = 'drop')
xkabledplyhead(crime_aggregated)

crime_aggregated_ward <- crime_data_clean %>%
  group_by(WARD, Month, SHIFT) %>%
  summarize(
    Crime_Count = n(),  # Target variable
    Ward_Population = first(Ward_Population),
    Avg_NightClub_Distance = mean(Nearest_NightClub_Distance, na.rm = TRUE),
    .groups = 'drop')

View(crime_aggregated_ward)
nrow(crime_aggregated_ward)
ward_crime_dist <- ggplot(crime_aggregated_ward) +
  geom_histogram(aes(x=Crime_Count), fill = 'darkgreen', color = 'black') +
  labs(title = 'Crime distribution (Grouped by Ward, Month, and Shift)',
       x = 'Crime', y = 'Count') +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 20))
ggsave('ward_crime_dist.jpg', ward_crime_dist, height = 6, width = 10)

qqnorm(crime_aggregated_ward$Crime_Count, main = "Q-Q Plot of Crime Count")
qqline(crime_aggregated_ward$Crime_Count)


# -------------- Model -------------------
crime_aggregated_ward$WARD <- as.factor(crime_aggregated_ward$WARD)
crime_aggregated_ward$Month <- as.factor(crime_aggregated_ward$Month)
crime_aggregated_ward$SHIFT <- as.factor(crime_aggregated_ward$SHIFT)

library(rpart)
library(rpart.plot)
library(caret)
month_encoded <- model.matrix(~ Month - 1, data = crime_aggregated) %>% # '-1' removes the intercept
  as.data.frame() %>%
  select(-MonthJan)  # Drop 'MonthJanuary' to avoid multicollinearity

# Step 3: One-Hot Encoding for 'SHIFT' and Drop One Column (e.g., MIDNIGHT)
shift_encoded <- model.matrix(~ SHIFT - 1, data = crime_aggregated) %>%
  as.data.frame() %>%
  select(-SHIFTMIDNIGHT)  # Drop 'SHIFTMIDNIGHT'

# Step 4: Combine Encoded Data with Original Dataset
crime_aggregated_encoded <- crime_aggregated %>%
  select(-Month, -SHIFT) %>%    # Remove original categorical columns
  cbind(month_encoded, shift_encoded)

# View the resulting dataset
View(crime_aggregated_encoded)

set.seed(42)
train_index <- createDataPartition(crime_aggregated$Crime_Count, p = 0.8, list = FALSE)
train_data <- crime_aggregated[train_index, ]
test_data <- crime_aggregated[-train_index, ]

tree_model <- rpart(Crime_Count ~ NEIGHBORHOOD_CLUSTER + Cluster_Population + Avg_NightClub_Distance + Month +SHIFT, 
                    data = train_data, method = "anova")
feature_importance <- tree_model$variable.importance
print(feature_importance)
print(tree_model)
library(rpart.plot)
fancyRpartPlot(tree_model)
rpart.plot(tree_model, type = 2, extra = 101, under = TRUE, tweak = 1.2)

predictions <- predict(tree_model, test_data)
library(Metrics)
mae_val <- mae(test_data$Crime_Count, predictions)
rmse_val <- rmse(test_data$Crime_Count, predictions)
rsq <- 1 - sum((test_data$Crime_Count - predictions)^2) / sum((test_data$Crime_Count - mean(test_data$Crime_Count))^2)
print(paste("MAE:", mae_val, "RMSE:", rmse_val, "R-squared:", rsq))

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue", color = 'black') +
  coord_flip() +
  theme_minimal() +
  labs(title = "Feature Importance", x = "Features", y = "Importance")

print(tree_model$cptable)
plotcp(tree_model)

set.seed(42) # For reproducibility
train_index <- createDataPartition(crime_aggregated_ward$Crime_Count, p = 0.8, list = FALSE)
train_data <- crime_aggregated_ward[train_index, ]
test_data <- crime_aggregated_ward[-train_index, ]

tree_model <- rpart(Crime_Count ~ WARD + Ward_Population + Avg_NightClub_Distance +SHIFT, 
                    data = train_data, method = "anova")
feature_importance <- tree_model$variable.importance
print(feature_importance)
print(tree_model)
library(rpart.plot)
fancyRpartPlot(tree_model)
rpart.plot(tree_model, type = 2, extra = 101, under = TRUE, tweak = 1.2)

predictions <- predict(tree_model, test_data)
library(Metrics)
mae_val <- mae(test_data$Crime_Count, predictions)
rmse_val <- rmse(test_data$Crime_Count, predictions)
rsq <- 1 - sum((test_data$Crime_Count - predictions)^2) / sum((test_data$Crime_Count - mean(test_data$Crime_Count))^2)
print(paste("MAE:", mae_val, "RMSE:", rmse_val, "R-squared:", rsq))

importance_df <- data.frame(
  Feature = names(feature_importance),
  Importance = feature_importance)

ward_tree_feature <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue", color='black') +
  coord_flip() +
  theme_minimal() +
  labs(title = "Feature Importance", x = "Features", y = "Importance") +
  theme(plot.title = element_text(face = "bold", size = 20))
ggsave('ward_tree_feature.jpg', ward_tree_feature, height = 6, width = 10)  

pruned_tree <- prune(tree_model, cp = tree_model$cptable[which.min(tree_model$cptable[,"xerror"]), "CP"])
rpart.plot(pruned_tree)
feature_importance <- pruned_tree$variable.importance
print(feature_importance)
fancyRpartPlot(pruned_tree)
rpart.plot(pruned_tree, type = 2, extra = 101, under = TRUE, tweak = 1.2)

predictions_p <- predict(pruned_tree, test_data)
library(Metrics)
mae_val <- mae(test_data$Crime_Count, predictions_p)
rmse_val <- rmse(test_data$Crime_Count, predictions_p)
rsq <- 1 - sum((test_data$Crime_Count - predictions_p)^2) / sum((test_data$Crime_Count - mean(test_data$Crime_Count))^2)
print(paste("MAE:", mae_val, "RMSE:", rmse_val, "R-squared:", rsq))


library(corrplot)
num_vars <- crime_aggregated_ward %>% select_if(is.numeric)
cor_matrix <- cor(num_vars, use = "complete.obs")
corrplot(cor_matrix, method = "circle")

lm_model <- lm(Crime_Count ~ WARD + Ward_Population + Avg_NightClub_Distance + Month + SHIFT, data = crime_aggregated_ward)
summary(lm_model)



residuals <- test_data$Crime_Count - predictions
library(ggplot2)
ggplot(data = test_data, aes(x = predictions, y = residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot", x = "Predicted Crime Count", y = "Residuals")

ggplot(data = test_data, aes(x = test_data$Crime_Count, y = predictions)) +
  geom_point(color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted", x = "Actual Crime Count", y = "Predicted Crime Count")

pruned_tree <- prune(tree_model, cp = tree_model$cptable[9,"CP"])

# Visualize the pruned tree
rpart.plot(pruned_tree, type = 2, extra = 101, under = TRUE, tweak = 1.2)

predictions <- predict(pruned_tree, test_data)
library(Metrics)
mae_val <- mae(test_data$Crime_Count, predictions)
rmse_val <- rmse(test_data$Crime_Count, predictions)
rsq <- 1 - sum((test_data$Crime_Count - predictions)^2) / sum((test_data$Crime_Count - mean(test_data$Crime_Count))^2)
print(paste("MAE:", mae_val, "RMSE:", rmse_val, "R-squared:", rsq))

library(corrplot)
num_vars <- crime_aggregated %>% select_if(is.numeric)
cor_matrix <- cor(num_vars, use = "complete.obs")
corrplot(cor_matrix, method = "circle")

lm_model <- lm(Crime_Count ~ Cluster_Population + Avg_NightClub_Distance + Month + SHIFT, data = crime_aggregated)
summary(lm_model)

library(caret)
control <- trainControl(method = "cv", number = 10)
tuned_model <- train(Crime_Count ~ Cluster_Population + Avg_NightClub_Distance + Month + SHIFT, data = train_data, method = "rpart",
                     trControl = control, tuneLength = 10)
print(tuned_model)




print(tree_model$cptable)
plotcp(tree_model)
tree_model <- prune(tree_model, cp = tree_model$cptable[3,"CP"])
fancyRpartPlot(tree_model)
predictions <- predict(tree_model, test_data)

# Evaluate the performance of the model
actuals <- test_data$Crime_Count
rmse <- sqrt(mean((predictions - actuals)^2))
cat("RMSE:", rmse)

importance <- tree_model$variable.importance
print(importance)