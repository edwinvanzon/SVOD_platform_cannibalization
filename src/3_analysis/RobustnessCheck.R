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

model_r1_clustered <- coeftest(model_r1, vcov = vcovCL(model_r1, cluster = ~ week))
model_r1_clustered

#########################
#Explanatory Analysis

# Log transform the DV
titles_ea <- titles_ea %>% 
  mutate(log_viewing_14days = log(viewing_14days)) 

model_ea <- lm(log_viewing_14days ~ 
                 n_releases_window14 +
                 original +
                 variety14 +
                 n_releases_window14:original +
                 n_releases_window14:variety14 +
                 IMDb_rating +
                 media_type +
                 factor(service) +
                 factor(year_month),
               data = titles_ea)

summary(model_ea)

model_ea_clustered <- coeftest(model_ea, vcov = vcovCL(model_ea, cluster = ~ week))
model_ea_clustered
