# Merge datasets into one dataset
## Make sure TMDb has 1 observation per IMDb ID
TMDb_clean <- TMDb %>%
  distinct(imdb_id, .keep_all = TRUE)

viewing_14_days_clean <- viewing_14_days %>% 
  distinct(imdb_id, .keep_all = TRUE)

titles <- releases %>%
  left_join(TMDb_clean, by = "imdb_id") %>%
  left_join(viewing_14_days_clean, by = "imdb_id") %>% 
  left_join(title_basics, by = c("imdb_id" = "tconst")) %>%
  left_join(title_ratings, by = c("imdb_id" = "tconst")) %>% 
  arrange(release_day)

titles <- titles %>% 
  rename(IMDb_rating = averageRating)

# Create # releases in release window of 7 days before and after release
titles <- titles %>% 
  mutate(window_start = release_day - 7,
         window_end = release_day + 7)

titles <- titles %>% 
  rowwise() %>% 
  mutate(n_releases_window = sum(
    releases$release_day >= window_start &
    releases$release_day <= window_end
  )) %>% ungroup()

# Create # releases in release window of 14 days before and after release for robustness check
titles <- titles %>% 
  mutate(window14_start = release_day - 14,
         window14_end = release_day + 14)

titles <- titles %>%
  rowwise() %>%
  mutate(n_releases_window14 = sum(
    releases$release_day >= window14_start &
      releases$release_day <= window14_end
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

#Create genre variety in a release window
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

#Create genre variety for 14 day release window
windows14 <- titles %>%
  select(focal_obs_id = obs_id, window14_start, window14_end)

genres_window14 <- windows14 %>%
  left_join(genres_long, by = character()) %>%
  filter(release_day >= window14_start & release_day <= window14_end)

hhi_data14 <- genres_window14 %>%
  group_by(focal_obs_id, genres) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(focal_obs_id) %>%
  mutate(share = n / sum(n)) %>%
  summarise(hhi14 = sum(share^2), .groups = "drop") %>%
  mutate(variety14 = 1 - hhi14)

titles <- titles %>% 
  left_join(hhi_data14, by = c('obs_id' = 'focal_obs_id'))


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
    imdb_id, service, original, release_day, viewing_7days, viewing_14days,
    
    window_start, window_end, n_releases_window,
    genres, hhi, variety,
    window14_start, window14_end, n_releases_window14,
    hhi14, variety14,
    
    IMDb_rating, media_type,
    
    year_month,
    
    week, week_id, n_releases_week, obs_id)

#Select only titles in the dataset that have 7 days before or 7 days after the release available
min_date <- min(releases$release_day)
max_date <- max(releases$release_day)

titles7 <- titles %>%
  filter(
    release_day >= min_date + 7,
    release_day <= max_date - 7
  )

#Select only titles in the dataset that have 14 days before or 14 days after the release available
titles14 <- titles %>%
  filter(
    release_day >= min_date + 14,
    release_day <= max_date - 14
  )
