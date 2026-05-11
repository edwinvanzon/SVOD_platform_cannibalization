#################################
# Merge datasets into one dataset
# Make sure TMDb has 1 observation per IMDb ID
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

########################
# Create release windows
# Main model: 7 days prior and 7 days after release
titles <- titles %>% 
  mutate(window_start = release_day - 7,
         window_end = release_day + 7)

# Robustness 1: 7 days after and 7 days before as control variable
titles <- titles %>%
  mutate(
    window_after7_start = release_day,
    window_after7_end   = release_day + 7,
    window_before7_start = release_day - 7,
    window_before7_end   = release_day
  )

# Robustness 2: 14 days after and 14 days before as control variable
titles <- titles %>%
  mutate(
    window_after14_start = release_day,
    window_after14_end   = release_day + 14,
    window_before14_start = release_day - 14,
    window_before14_end   = release_day
  )

################################################
# Count the number of releases per release window
titles <- titles %>% 
  rowwise() %>% 
  mutate(
    n_releases_window = sum(
      releases$release_day >= window_start &
      releases$release_day <= window_end
    ),
    n_releases_after7 = sum(
      releases$release_day >= window_after7_start &
      releases$release_day <= window_after7_end
    ),
    n_releases_before7 = sum(
      releases$release_day >= window_before7_start &
      releases$release_day <= window_before7_end
    ),
    n_releases_after14 = sum(
      releases$release_day >= window_after14_start &
      releases$release_day <= window_after14_end
    ),
    n_releases_before14 = sum(
      releases$release_day >= window_before14_start &
      releases$release_day <= window_before14_end
      )) %>% ungroup()


########################################
# Create genre variety in release windows
titles <- titles %>% 
  mutate(obs_id = row_number())

genres_long <- titles %>%
  select(window_title_id = obs_id, release_day, genres) %>%
  separate_rows(genres, sep = ",")


# Main model: +-7 days
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

# Robustness 1: 7 days after
windows_after7 <- titles %>%
  select(focal_obs_id = obs_id, window_start = window_after7_start, window_end = window_after7_end)

genres_window_after7 <- windows_after7 %>%
  left_join(genres_long, by = character()) %>%
  filter(release_day >= window_start & release_day <= window_end)

hhi_data_after7 <- genres_window_after7 %>%
  group_by(focal_obs_id, genres) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(focal_obs_id) %>%
  mutate(share = n / sum(n)) %>%
  summarise(hhi_after7 = sum(share^2), .groups = "drop") %>%
  mutate(variety_after7 = 1 - hhi_after7)

titles <- titles %>% 
  left_join(hhi_data_after7, by = c('obs_id' = 'focal_obs_id'))

# Robustness 2: 14 days after
windows_after14 <- titles %>%
  select(focal_obs_id = obs_id, window_start = window_after14_start, window_end = window_after14_end)

genres_window_after14 <- windows_after14 %>%
  left_join(genres_long, by = character()) %>%
  filter(release_day >= window_start & release_day <= window_end)

hhi_data_after14 <- genres_window_after14 %>%
  group_by(focal_obs_id, genres) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(focal_obs_id) %>%
  mutate(share = n / sum(n)) %>%
  summarise(hhi_after14 = sum(share^2), .groups = "drop") %>%
  mutate(variety_after14 = 1 - hhi_after14)

titles <- titles %>% 
  left_join(hhi_data_after14, by = c('obs_id' = 'focal_obs_id'))

##############################
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

################################
# Restructure the titles dataset
titles <- titles %>% 
  select(
    imdb_id, service, original, release_day, viewing_7days, viewing_14days,
    
    window_start, window_end, n_releases_window,
    genres, hhi, variety,
    
    window_after7_start, window_after7_end, n_releases_after7,
    hhi_after7, variety_after7,
    window_before7_start, window_before7_end, n_releases_before7,
    
    window_after14_start, window_after14_end, n_releases_after14,
    hhi_after14, variety_after14,
    window_before14_start, window_before14_end, n_releases_before14,    
    
    IMDb_rating, media_type,
    year_month,
    week, week_id, n_releases_week, obs_id)

#####################################
min_date <- min(releases$release_day)
max_date <- max(releases$release_day)

# Robstness 2:
# Select only titles in the dataset that have 14 days before or 14 days after the release available

titles_r2 <- titles %>% 
  filter(
    release_day >= min_date + 14,
    release_day <= max_date -14
  )

# Main model and robustness 1:
# Select only titles in the dataset that have 7 days before or 7 days after the release available
titles <- titles %>%
  filter(
    release_day >= min_date + 7,
    release_day <= max_date - 7
  )
