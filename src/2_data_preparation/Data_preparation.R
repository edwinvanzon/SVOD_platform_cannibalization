# Merge datasets into one dataset
## Make sure TMDb has 1 observation per IMDb ID
TMDb_clean <- TMDb %>%
  distinct(imdb_id, .keep_all = TRUE)

titles <- releases %>%
  left_join(TMDb_clean, by = "imdb_id") %>%
  left_join(title_basics, by = c("imdb_id" = "tconst")) %>%
  left_join(title_ratings, by = c("imdb_id" = "tconst"))

titles <- titles %>% 
  rename(IMDb_rating = averageRating)

# Create weeks as release periods
titles <- titles %>%
  mutate(week = lubridate::floor_date(as.Date(release_day), "week")) %>%
  arrange(week) %>%
  mutate(week_id = as.numeric(factor(week))) %>%
  group_by(week_id) %>%
  mutate(n_releases = n()) %>%
  ungroup()

#Create genre variety
genres_long <- titles %>%
  separate_rows(genres, sep = ",")

genre_variety <- genres_long %>% 
  group_by(week_id) %>% 
  summarise(n_genres_week = n_distinct(genres))

titles <- titles %>% 
  left_join(genre_variety, by = 'week_id')


# Order weekly data
week_data <- titles %>%
  group_by(week_id) %>%
  summarise(n_releases = first(n_releases))

week_data <- titles %>%
  group_by(week_id, week) %>%
  summarise(
    n_releases = first(n_releases),
    total_views = sum(viewing_7days, na.rm = TRUE),
    original_ratio = mean(original, na.rm = TRUE),
    licensed_ratio = 1 - mean(original, na.rm = TRUE),
    n_genres_week = first(n_genres_week),
    avg_imdb_rating = mean(IMDb_rating, na.rm = TRUE),
    avg_tmdb_popularity = mean(tmdb_popularity, na.rm = TRUE)
  ) %>%
  mutate(
    avg_views_per_title = total_views/n_releases,
  ) %>% 
  ungroup()