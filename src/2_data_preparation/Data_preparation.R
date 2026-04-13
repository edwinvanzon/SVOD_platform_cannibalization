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

# Create # releases in 7 days prior to release
titles <- titles %>% 
  mutate(prev_window_start = release_day - 7,
         prev_window_end = release_day -1)

titles <- titles %>%
  rowwise() %>%
  mutate(n_releases_prev7 = sum(
    releases$release_day >= prev_window_start &
      releases$release_day <= prev_window_end
  )) %>%
  ungroup()

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

# Create seasonality variable
titles <- titles %>%
  mutate(year_month = factor(format(as.Date(release_day), "%Y-%m")))

# Create weeks as release periods
titles <- titles %>%
  mutate(week = lubridate::floor_date(as.Date(release_day), "week")) %>%
  arrange(week) %>%
  mutate(week_id = as.numeric(factor(week))) %>%
  group_by(week_id) %>%
  mutate(n_releases_week = n()) %>%
  ungroup()

# Order weekly data
week_data <- titles %>%
  distinct(week_id, week, n_releases_week)


# Restructure the titles dataset
titles <- titles %>% 
  select(
    imdb_id, service, original, release_day, viewing_7days,
    
    window_start, window_end, n_releases_window,
    genres, hhi, variety,
    
    prev_window_start, prev_window_end, n_releases_prev7,
    
    IMDb_rating, media_type,
    
    year_month,
    
    tmdb_popularity,
    
    week, week_id, n_releases_week, obs_id)
