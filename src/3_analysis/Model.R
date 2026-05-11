# Model 1 - Baseline with control variables
model_1 <- lm(log_viewing_7days ~
                n_releases_window +
                original +
                variety +
                IMDb_rating +
                media_type,
              data = titles7)

# Model 2 - Baseline with control variables and fixed effects
model_2 <- lm(log_viewing_7days ~
                n_releases_window +
                original +
                variety +
                IMDb_rating +
                media_type +
                factor(service) +
                factor(year_month),
              data = titles7)

# Model 3 - Initial model
model_3 <- model

# Compare fit statistics
model_fit <- tibble(
  model = c("Model 1: Baseline + Controls",
            "Model 2: Baseline + Controls + FE",
            "Model 3: Full model"),
  r_squared = c(summary(model_1)$r.squared,
                summary(model_2)$r.squared,
                summary(model_3)$r.squared),
  adj_r_squared = c(summary(model_1)$adj.r.squared,
                    summary(model_2)$adj.r.squared,
                    summary(model_3)$adj.r.squared),
  AIC = c(AIC(model_1), AIC(model_2), AIC(model_3)),
  BIC = c(BIC(model_1), BIC(model_2), BIC(model_3))
)

model_fit


#Estimates
model_clustered <- coeftest(model, vcov = vcovCL(model, cluster = ~ week))

model_clustered

#Full table
model_df <- data.frame(
  Term = rownames(model_clustered),
  unclass(model_clustered),
  row.names = NULL,
  check.names = FALSE
)

model_df$Signif <- symnum(model_df$`Pr(>|t|)`,
                       corr = FALSE,
                       na = FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", ""))

write_xlsx(model_df, "clustered_lm_table.xlsx")


#Interaction plots

# Plot 1: Moderation of Title Type (Original vs. Licensed)
releases_seq <- seq(5, 45, by = 1)

df_type <- data.frame(
  releases   = rep(releases_seq, 2),
  title_type = rep(c("Original", "Licensed"), each = length(releases_seq))
)

df_type$n_releases_window <- df_type$releases
df_type$original <- ifelse(df_type$title_type == "Original", 1, 0)
df_type$variety <- 0.89
df_type$IMDb_rating <- 0
df_type$media_type <- "movie"
df_type$service <- "amazon"
df_type$year_month <- "2020-01"

pred <- predict(model, newdata = df_type, interval = "confidence", level = 0.95)
df_type$log_views <- pred[, "fit"]
df_type$lower <- pred[, "lwr"]
df_type$upper <- pred[, "upr"]

plot1 <- ggplot(df_type, aes(x = releases, y = log_views,
                             color = title_type, linetype = title_type,
                             fill = title_type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = c("Original" = "#2C5F8A", "Licensed" = "#E07B39")) +
  scale_fill_manual(values = c("Original" = "#2C5F8A", "Licensed" = "#E07B39")) +
  scale_linetype_manual(values = c("Original" = "solid", "Licensed" = "dashed")) +
  labs(
    title    = "Figure X. Moderating Effect of Title Type",
    subtitle = "Effect of release density on log(viewership) by title type",
    x        = "Number of Releases in Release Window",
    y        = "Predicted log(Viewership)",
    color    = "Title Type",
    linetype = "Title Type"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    legend.position = "bottom",
    plot.caption  = element_text(color = "grey50", size = 9, hjust = 0),
    legend.title  = element_blank()
  )
plot1


# Plot 2: Moderation of Content Variety with confidence intervals
variety_levels <- c(0.86, 0.89, 0.92)
variety_labels <- c("Low (0.86)", "Mean (0.89)", "High (0.92)")

df_var <- data.frame(
  releases = rep(releases_seq, length(variety_levels)),
  variety  = rep(variety_levels, each = length(releases_seq)),
  label    = rep(variety_labels, each = length(releases_seq))
)

df_var$n_releases_window <- df_var$releases
df_var$original <- 0
df_var$IMDb_rating <- mean(titles7$IMDb_rating)
df_var$media_type <- "movie"
df_var$service <- "amazon"
df_var$year_month <- "2020-01"

pred2 <- predict(model, newdata = df_var, interval = "confidence", level = 0.95)
df_var$log_views <- pred2[, "fit"]
df_var$lower <- pred2[, "lwr"]
df_var$upper <- pred2[, "upr"]

df_var$label <- factor(df_var$label, levels = variety_labels)

plot2 <- ggplot(df_var, aes(x = releases, y = log_views,
                            color = label, linetype = label,
                            fill = label)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = c(
    "Low (0.86)"  = "#C0392B",
    "Mean (0.89)" = "#7F8C8D",
    "High (0.92)" = "#27AE60"
  )) +
  scale_fill_manual(values = c(
    "Low (0.86)"  = "#C0392B",
    "Mean (0.89)" = "#7F8C8D",
    "High (0.92)" = "#27AE60"
  )) +
  scale_linetype_manual(values = c(
    "Low (0.86)"  = "dashed",
    "Mean (0.89)" = "dotdash",
    "High (0.92)" = "solid"
  )) +
  guides(fill = "none") +
  labs(
    title    = "Figure X. Moderating Effect of Content Variety",
    subtitle = "Effect of release density on log(viewership) by genre variety level",
    x        = "Number of Releases in Release Window",
    y        = "Predicted log(Viewership)",
    color    = "Content Variety",
    linetype = "Content Variety"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    legend.position = "bottom",
    plot.caption  = element_text(color = "grey50", size = 9, hjust = 0)
  )

plot2

ggsave("src/output/plot_titletype_interaction.png", plot1, width = 7, height = 5, dpi = 300)
ggsave("src/output/plot_variety_interaction.png", plot2, width = 7, height = 5, dpi = 300)

