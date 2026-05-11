####################
# Robustness check 1:
model_r1 <- lm(log_viewing_7days ~ 
              n_releases_after7 +
              original +
              variety_after7 +
              n_releases_after7:original +
              n_releases_after7:variety_after7 +
              IMDb_rating +
              media_type +
              n_releases_before7 +
              factor(service) +
              factor(year_month),
            data = titles)

summary(model_r1)

titles_r2 <- titles %>% 
  filter(
    release_day >= min_date + 14,
    release_day <= max_date -14
  )

# Log transform the DV
titles_r2 <- titles_r2 %>% 
  mutate(log_viewing_14days = log(viewing_14days)) 

model_r2 <- lm(log_viewing_14days ~ 
                 n_releases_after14 +
                 original +
                 variety_after14 +
                 n_releases_after14:original +
                 n_releases_after14:variety_after14 +
                 IMDb_rating +
                 media_type +
                 n_releases_before14 +
                 factor(service) +
                 factor(year_month),
               data = titles_r2)

summary(model_r2)