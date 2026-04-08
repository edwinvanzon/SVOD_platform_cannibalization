# Descriptives
summary(titles)

titles %>%
  summarise(
    mean_views = mean(viewing_7days, na.rm = TRUE),
    sd_views = sd(viewing_7days, na.rm = TRUE),
    
    mean_rating = mean(IMDb_rating, na.rm = TRUE),
    sd_rating = sd(IMDb_rating, na.rm = TRUE),
    
    mean_popularity = mean(tmdb_popularity, na.rm = TRUE),
    sd_popularity = sd(tmdb_popularity, na.rm = TRUE),
    
    mean_original = mean(original, na.rm = TRUE),
    sd_original = sd(original, na.rm = TRUE))

#Check the ratio for SVOD platforms in the dataset
titles %>%
  group_by(service) %>%
  summarise(n = n(),percentage = n / nrow(titles))


# Descriptives for weekly data
week_data %>% 
  summarise(
    mean_releases = mean(n_releases, na.rm = TRUE),
    sd_releases = sd(n_releases, na.rm = TRUE),
    
    mean_views = mean(avg_views_per_title, na.rm = TRUE),
    sd_views = sd(avg_views_per_title, na.rm = TRUE),
    
    mean_original = mean(original_ratio, na.rm = TRUE),
    sd_original = sd(original_ratio, na.rm = TRUE),
    
    mean_genre_variety = mean(n_genres_week, na.rm = TRUE),
    sd_genre_variety = sd(n_genres_week, na.rm = TRUE))


# Correlation matrix
cor_matrix <- cor(titles %>%
                    select(n_releases_window, viewing_7days, original, n_genres_week, IMDb_rating, tmdb_popularity),
                  use = "complete.obs")

cor(week_data %>%
      select(n_releases, avg_views_per_title),
    use = "complete.obs")


# Figures
# releases per week
mean_releases <- mean(week_data$n_releases)
figure_1 <- week_data %>% 
  ggplot(aes(x = week_id, y = n_releases)) +
  geom_hline(yintercept = mean_releases, 
             linetype = "dashed", 
             color = "red") +
  geom_line(color = "black") +
  labs(title = "Number of releases per week",
       x = "Week_id",
       y = "# releases") +
  theme_minimal()


# Average views per release window H1
figure_2 <- week_data %>% ggplot(aes(x = n_releases, y = avg_views_per_title)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Release Density and Average Views per Title",
    x = "Number of Releases",
    y = "Average Views per Title") +
  theme_minimal()

# Originals H2
figure_3 <- week_data %>% ggplot(aes(x = original_ratio, y = avg_views_per_title)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Original Content Share and Performance",
    x = "Share of Original Titles",
    y = "Average Views per Title") +
  theme_minimal()

# Genre variety H3
figure_4 <- week_data %>% ggplot(aes(x = n_genres_week, y = avg_views_per_title)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Content Variety and Performance",
    x = "Number of Genres",
    y = "Average Views per Title") +
  theme_minimal()

# histogram
figure_5 <- titles %>% ggplot(aes(x = viewing_7days)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs(title = "Distribution of Views per Title",
    x = "Views (7 days)",
    y = "Frequency") +
  theme_minimal()

# Saving figures
ggsave("src/output/figure_1.png", plot = figure_1, width = 8, height = 6)
ggsave("src/output/figure_2.png", plot = figure_2, width = 8, height = 6)
ggsave("src/output/figure_3.png", plot = figure_3, width = 8, height = 6)
ggsave("src/output/figure_4.png", plot = figure_4, width = 8, height = 6)
ggsave("src/output/figure_5.png", plot = figure_5, width = 8, height = 6)
