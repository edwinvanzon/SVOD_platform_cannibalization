# Descriptives
summary(titles)

titles %>%
  summarise(
    mean_views = mean(viewing_7days, na.rm = TRUE),
    sd_views = sd(viewing_7days, na.rm = TRUE),
    
    mean_releases = mean(n_releases_window, na.rm = TRUE),
    sd_releases = sd(n_releases_window, na.rm = TRUE),
    
    mean_variety = mean(variety, na.rm = TRUE),
    sd_variety = sd(variety, na.rm = TRUE),
    
    mean_prev_releases = mean(n_releases_prev7, na.rm = TRUE),
    sd_prev_releases = sd(n_releases_prev7, na.rm = TRUE),
    
    mean_rating = mean(IMDb_rating, na.rm = TRUE),
    sd_rating = sd(IMDb_rating, na.rm = TRUE),
    
    mean_original = mean(original, na.rm = TRUE),
    sd_original = sd(original, na.rm = TRUE)) %>% 
  
  print(., width = Inf)

#check the ratio of movies and series
titles %>% 
  group_by(media_type) %>% 
  summarise(n = n(), percentage = n / nrow(titles))

#Check the ratio for SVOD platforms in the dataset
titles %>%
  group_by(service) %>%
  summarise(n = n(),percentage = n / nrow(titles))


# Correlation matrix
cor_matrix <- cor(
  titles %>% 
    select(n_releases_window,
           viewing_7days,
           original,
           variety,
           IMDb_rating,
           n_releases_prev7),
  use = "complete.obs")

  
# Figures
# releases per week
mean_releases <- mean(week_data$n_releases_week)
figure_1 <- week_data %>% 
  ggplot(aes(x = week_id, y = n_releases_week)) +
  geom_hline(yintercept = mean_releases, 
             linetype = "dashed", 
             color = "red") +
  geom_line(color = "black") +
  labs(title = "Number of releases per week",
       x = "Week_id",
       y = "# releases") +
  theme_minimal()
figure_1

# Average views per release window H1
window_data <- titles %>%
  select(window_start, window_end, n_releases_window) %>%
  distinct()

window_data <- window_data %>%
  left_join(
    titles %>% select(release_day, viewing_7days),
    by = character()
  ) %>%
  filter(release_day >= window_start & release_day <= window_end)

window_data <- window_data %>%
  group_by(window_start, window_end, n_releases_window) %>%
  summarise(
    avg_views_per_title = mean(viewing_7days, na.rm = TRUE),
    .groups = "drop"
  )

figure_2 <- window_data %>% 
  ggplot(aes(x = n_releases_window, y = avg_views_per_title)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Release Density and Average Views per Title within a release window",
       x = "Number of Releases",
       y = "Average Views per Title") +
  theme_minimal()
figure_2


# histogram
figure_3 <- titles %>% ggplot(aes(x = viewing_7days)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs(title = "Distribution of Views per Title",
    x = "Views (7 days)",
    y = "Frequency") +
  theme_minimal()
figure_3

# Saving figures
ggsave("src/output/figure_1.png", plot = figure_1, width = 8, height = 6)
ggsave("src/output/figure_2.png", plot = figure_2, width = 8, height = 6)
ggsave("src/output/figure_3.png", plot = figure_3, width = 8, height = 6)