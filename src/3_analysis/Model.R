##########################
# Check the best model fit
# Model 1 - Baseline with control variables
model_1 <- lm(log_viewing_7days ~
                n_releases_window +
                original +
                variety +
                IMDb_rating +
                media_type,
              data = titles)

# Model 2 - Baseline with control variables and fixed effects
model_2 <- lm(log_viewing_7days ~
                n_releases_window +
                original +
                variety +
                IMDb_rating +
                media_type +
                factor(service) +
                factor(year_month),
              data = titles)

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

#####################################
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

write_xlsx(model_df, "src/output/clustered_lm_table.xlsx")

