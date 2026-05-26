#################################################
# Descriptives
summary(titles)

titles %>%
  mutate(log_viewing_7days = log(viewing_7days)) %>% 
  summarise(
    mean_views = mean(viewing_7days, na.rm = TRUE),
    sd_views = sd(viewing_7days, na.rm = TRUE),
    median_views = median(viewing_7days, na.rm = TRUE),
    
    mean_log_views = mean(log_viewing_7days, na.rm = TRUE),
    sd_log_views = sd(log_viewing_7days, na.rm = TRUE),
    
    mean_releases = mean(n_releases_window, na.rm = TRUE),
    sd_releases = sd(n_releases_window, na.rm = TRUE),
    
    mean_variety = mean(variety, na.rm = TRUE),
    sd_variety = sd(variety, na.rm = TRUE),
    
    mean_rating = mean(IMDb_rating, na.rm = TRUE),
    sd_rating = sd(IMDb_rating, na.rm = TRUE),
    
    mean_original = mean(original, na.rm = TRUE),
    sd_original = sd(original, na.rm = TRUE)) %>% 
  
  print(., width = Inf)

# Check the ratio of movies and series
titles %>% 
  group_by(media_type) %>% 
  summarise(n = n(), percentage = n / nrow(titles))

# Check the ratio for SVOD platforms in the dataset
titles %>%
  group_by(service) %>%
  summarise(n = n(),percentage = n / nrow(titles))


# Correlation matrix
cor_matrix <- cor(
  titles %>% 
    mutate(log_viewing_7days = log(viewing_7days)) %>% 
    mutate(media_type = as.numeric(media_type == "tv")) %>% 
    select(n_releases_window,
           log_viewing_7days,
           original,
           variety,
           IMDb_rating,
           media_type),
  use = "complete.obs")

cor_matrix[upper.tri(cor_matrix)] <- NA
print(cor_matrix)

##################################################
# Figures
# releases per week
mean_releases <- mean(week_data$n_releases_week)
figure_2 <- week_data %>% 
  ggplot(aes(x = week_id, y = n_releases_week)) +
  geom_hline(yintercept = mean_releases, 
             linetype = "dashed", 
             color = "red") +
  geom_line(color = "black") +
  labs(title = "Number of releases per week",
       x = "Week_id",
       y = "# releases") +
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
ggsave("src/output/figure_2_releases_per_week.png", plot = figure_2, width = 8, height = 6)
ggsave("src/output/figure_3_histogram.png", plot = figure_3, width = 8, height = 6)

