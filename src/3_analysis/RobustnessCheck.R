########################################################
# Exploratory Analysis:
model_ea <- lm(log_viewing_7days ~ 
              n_releases_after7 +
              n_releases_before7 +
              original +
              variety_after7 +
              variety_before7 +
              n_releases_after7:original +
              n_releases_before7:original +
              n_releases_after7:variety_after7 +
              n_releases_before7:variety_before7 +
              IMDb_rating +
              media_type +
              factor(service) +
              factor(year_month),
            data = titles)

summary(model_ea)

model_ea_clustered <- coeftest(model_ea, vcov = vcovCL(model_ea, cluster = ~ week))
model_ea_clustered

model_ea_df <- data.frame(
  Term = rownames(model_ea_clustered),
  unclass(model_ea_clustered),
  row.names = NULL,
  check.names = FALSE
)

model_ea_df$Signif <- symnum(model_ea_df$`Pr(>|t|)`,
                             corr = FALSE,
                             na = FALSE,
                             cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                             symbols = c("***", "**", "*", ".", ""))

write_xlsx(model_ea_df, "src/output/clustered_ea_table.xlsx")


summary(model_ea)$r.squared
summary(model_ea)$adj.r.squared

mean(titles$variety_after7)
sd(titles$variety_after7)
mean(titles$variety_before7)
sd(titles$variety_before7)


##################################################
#Robustness Check

# Log transform the DV
titles_rc <- titles_rc %>% 
  mutate(log_viewing_14days = log(viewing_14days)) 

model_rc <- lm(log_viewing_14days ~ 
                 n_releases_window14 +
                 original +
                 variety14 +
                 n_releases_window14:original +
                 n_releases_window14:variety14 +
                 IMDb_rating +
                 media_type +
                 factor(service) +
                 factor(year_month),
               data = titles_rc)

summary(model_rc)

model_rc_clustered <- coeftest(model_rc, vcov = vcovCL(model_rc, cluster = ~ week))
model_rc_clustered

model_rc_df <- data.frame(
  Term = rownames(model_rc_clustered),
  unclass(model_rc_clustered),
  row.names = NULL,
  check.names = FALSE
)

model_rc_df$Signif <- symnum(model_rc_df$`Pr(>|t|)`,
                             corr = FALSE,
                             na = FALSE,
                             cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                             symbols = c("***", "**", "*", ".", ""))

write_xlsx(model_rc_df, "src/output/clustered_rc_table.xlsx")

summary(model_rc)$r.squared
summary(model_rc)$adj.r.squared

mean(titles_rc$variety14)
sd(titles_rc$variety14)

