# Merge datasets into one dataset
## Make sure TMDb has 1 observation per IMDb ID
TMDb_clean <- TMDb %>%
  distinct(imdb_id, .keep_all = TRUE)

titles <- releases %>%
  left_join(TMDb_clean, by = "imdb_id") %>%
  left_join(title_basics, by = c("imdb_id" = "tconst")) %>%
  left_join(title_ratings, by = c("imdb_id" = "tconst")) %>% 
  arrange(release_day)

titles <- titles %>% 
  rename(IMDb_rating = averageRating)

# Create # releases in 7 days after release
titles <- titles %>% 
  mutate(window_start = release_day,
         window_end = release_day + 6)

titles <- titles %>% 
  rowwise() %>% 
  mutate(n_releases_window = sum(
    releases$release_day >= window_start &
    releases$release_day <= window_end
  )) %>% ungroup()

# Create weeks as release periods
titles <- titles %>%
  mutate(week = lubridate::floor_date(as.Date(release_day), "week")) %>%
  arrange(week) %>%
  mutate(week_id = as.numeric(factor(week))) %>%
  group_by(week_id) %>%
  mutate(n_releases_week = n()) %>%
  ungroup()

#Create genre variety
titles <- titles %>% 
  mutate(obs_id = row_number())

genres_long <- titles %>%
  select(window_title_id = obs_id, release_day, genres) %>%
  separate_rows(genres, sep = ",")

windows <- titles %>%
  select(focal_obs_id = obs_id, window_start, window_end)

genres_window <- windows %>%
  left_join(genres_long, by = character()) %>%
  filter(release_day >= window_start & release_day <= window_end)

hhi_data <- genres_window %>%
  group_by(focal_obs_id, genres) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(focal_obs_id) %>%
  mutate(share = n / sum(n)) %>%
  summarise(hhi = sum(share^2), .groups = "drop") %>%
  mutate(variety = 1 - hhi)

titles <- titles %>% 
  left_join(hhi_data, by = c('obs_id' = 'focal_obs_id'))

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