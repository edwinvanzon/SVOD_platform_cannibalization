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

summary(model_r1)$r.squared
summary(model_r1)$adj.r.squared

mean(titles$variety_after7)
sd(titles$variety_after7)


# Plot variety interaction for robustness check
variety_levels_r1 <- c(0.866, 0.887, 0.908)
variety_labels_r1 <- c("Low (0.866)", "Mean (0.887)", "High (0.908)")

releases_seq_r1 <- seq(5, 25, by = 1)

df_var_r1 <- data.frame(
  releases = rep(releases_seq_r1, length(variety_levels_r1)),
  variety_after7 = rep(variety_levels_r1, each = length(releases_seq_r1)),
  label = rep(variety_labels_r1, each = length(releases_seq_r1))
)

df_var_r1$n_releases_after7 <- df_var_r1$releases
df_var_r1$original <- 0
df_var_r1$IMDb_rating <- mean(titles$IMDb_rating)
df_var_r1$media_type <- "movie"
df_var_r1$service <- "amazon"
df_var_r1$year_month <- levels(titles$year_month)[1]
df_var_r1$n_releases_before7 <- mean(titles$n_releases_before7)

pred_r1 <- predict(model_r1, newdata = df_var_r1, interval = "confidence", level = 0.95)
df_var_r1$log_views <- pred_r1[, "fit"]
df_var_r1$lower <- pred_r1[, "lwr"]
df_var_r1$upper <- pred_r1[, "upr"]

df_var_r1$label <- factor(df_var_r1$label, levels = variety_labels_r1)

plot_variety_r1 <- ggplot(df_var_r1, aes(x = releases, y = log_views,
                                         color = label, linetype = label,
                                         fill = label)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = c(
    "Low (0.866)"  = "#C0392B",
    "Mean (0.887)" = "#7F8C8D",
    "High (0.908)" = "#27AE60"
  )) +
  scale_fill_manual(values = c(
    "Low (0.866)"  = "#C0392B",
    "Mean (0.887)" = "#7F8C8D",
    "High (0.908)" = "#27AE60"
  )) +
  scale_linetype_manual(values = c(
    "Low (0.866)"  = "dashed",
    "Mean (0.887)" = "dotdash",
    "High (0.908)" = "solid"
  )) +
  guides(fill = "none") +
  labs(
    subtitle = "Effect of release density on log(viewership) by genre variety level",
    x        = "Number of Releases in Release Window",
    y        = "Predicted log(Viewership)",
    color    = "Content Variety",
    linetype = "Content Variety",
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    legend.position = "bottom",
    plot.caption  = element_text(color = "grey50", size = 9, hjust = 0)
  )

plot_variety_r1

ggsave("src/output/figure_9_variety_r1.png", plot_variety_r1, width = 7, height = 5, dpi = 300)


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


summary(model_ea)$r.squared
summary(model_ea)$adj.r.squared

mean(titles_ea$variety14)
sd(titles_ea$variety14)
