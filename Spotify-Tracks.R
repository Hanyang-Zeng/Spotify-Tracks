library(dplyr)
library(tidyr) 
library(ggplot2) 

# Import data
data <- read.csv("dataset.csv", stringsAsFactors = FALSE)

# View data structures
str(data)

# Check for missing values
colSums(is.na(data))

# Data clean
## Remove duplicate values
data <- data %>% distinct()

# Check for and handle outliers (removing values other than 0 through 1)
data <- data %>% filter(energy >= 0 & energy <= 1)
data <- data %>% filter(danceability >= 0 & danceability <= 1)
data <- data %>% filter(duration_ms >= 0 & duration_ms <= 1)
data <- data %>% filter(tempo >= 0 & tempo <= 1)
data <- data %>% filter(valence >= 0 & valence <= 1)
data <- data %>% filter(key >= 0 & key <= 1)
data <- data %>% filter(track_genre >= 0 & track_genre <= 1)
data <- data %>% filter(popularity >= 0 & popularity <= 1)

## Feature engineering
## Standardized variables (tempo and duration_ms)
normalize <- function(x) (x - min(x)) / (max(x) - min(x))
data$tempo <- normalize(data$tempo)
data$duration_ms <- normalize(data$duration_ms)

# 4. Preservation of cleaned-up data
write.csv(data, "cleaned_dataset.csv", row.names = FALSE)

# Validate data cleansing effectiveness
summary(data)

# Calculate the average popularity of each music genre
genre_popularity <- data %>%
  group_by(track_genre) %>%
  summarise(avg_popularity = mean(popularity, na.rm = TRUE)) %>%
  arrange(desc(avg_popularity))

# View Results
print(genre_popularity)

# Plotting a bar graph of the popularity of music genres
ggplot(genre_popularity, aes(x = reorder(track_genre, -avg_popularity), y = avg_popularity)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  labs(title = "Average Popularity by Track Genre", x = "Track Genre", y = "Average Popularity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Most popular genres of music
most_popular_genre <- genre_popularity %>% slice(1)
print(most_popular_genre)

# Calculate average popularity and filter the top 10
top_genres <- genre_popularity %>% slice_max(avg_popularity, n = 10)

# Plotting a bar graph of the top 10 music genres
ggplot(top_genres, aes(x = reorder(track_genre, -avg_popularity), y = avg_popularity)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  labs(title = "Top 10 Most Popular Track Genres",
       x = "Track Genre", y = "Average Popularity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter data of type pop-film
pop_film_data <- data %>% filter(track_genre == "pop-film")

# Descriptive statistics
pop_film_stats <- pop_film_data %>%
  summarise(
    avg_danceability = mean(danceability, na.rm = TRUE),
    sd_danceability = sd(danceability, na.rm = TRUE),
    avg_duration_ms = mean(duration_ms, na.rm = TRUE),
    sd_duration_ms = sd(duration_ms, na.rm = TRUE),
    avg_tempo = mean(tempo, na.rm = TRUE),
    sd_tempo = sd(tempo, na.rm = TRUE),
    avg_valence = mean(valence, na.rm = TRUE),
    sd_valence = sd(valence, na.rm = TRUE),
    avg_key = mean(key, na.rm = TRUE),
    sd_key = sd(key, na.rm = TRUE),
    avg_energy = mean(energy, na.rm = TRUE),
    sd_energy = sd(energy, na.rm = TRUE)
  )
View(pop_film_stats)
print(pop_film_stats)

# Density map: energy
ggplot(pop_film_data, aes(x = energy)) +
  geom_density(fill = "lightblue", alpha = 0.6) +
  labs(title = "Density Plot of Energy for Pop-Film",
       x = "Energy", y = "Density")
# Density map：danceability
ggplot(pop_film_data, aes(x = danceability)) +
  geom_density(fill = "lightblue", alpha = 0.6) +
  labs(title = "Density Plot of Danceability for Pop-Film",
       x = "Danceability", y = "Density")
# Density map：duration_ms
ggplot(pop_film_data, aes(x = duration_ms)) +
  geom_density(fill = "lightblue", alpha = 0.6) +
  labs(title = "Density Plot of Duration_ms for Pop-Film",
       x = "Duration_ms", y = "Density")
# Density map：tempo
ggplot(pop_film_data, aes(x = tempo)) +
  geom_density(fill = "lightblue", alpha = 0.6) +
  labs(title = "Density Plot of Tempo for Pop-Film",
       x = "Tempo", y = "Density")
# Density map：valence
ggplot(pop_film_data, aes(x = valence)) +
  geom_density(fill = "lightblue", alpha = 0.6) +
  labs(title = "Density Plot of Valence for Pop-Film",
       x = "Valence", y = "Density")
# Density map：key
ggplot(pop_film_data, aes(x = key)) +
  geom_density(fill = "lightblue", alpha = 0.6) +
  labs(title = "Density Plot of Key for Pop-Film",
       x = "Key", y = "Density")

# Select the desired audio features
pop_film_features <- pop_film_data %>%
  select(danceability, duration_ms, tempo, valence, key, energy)

# Calculate the correlation matrix
cor_matrix <- cor(pop_film_features, use = "complete.obs")

# Heat maps
install.packages('reshape2')
library(reshape2)
ggplot(melt(cor_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Matrix of Audio Features for Pop-Film") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Selection of variables for regression analysis
regression_data <- data %>% 
  select(popularity, danceability, duration_ms, tempo, valence, key, energy)

# Check for multicollinearity (VIF)
install.packages('car')
library(car)
vif_model <- lm(popularity ~ danceability + duration_ms + tempo + valence + key + energy, 
                data = regression_data)
vif(vif_model)

# Multiple linear regression modeling
lm_model <- lm(popularity ~ danceability + duration_ms + tempo + valence + key + energy, 
               data = regression_data)

# View regression model results
summary(lm_model)
