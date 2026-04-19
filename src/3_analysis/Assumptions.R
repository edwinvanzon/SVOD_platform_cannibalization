# Log transform the DV
titles <- titles %>% 
  mutate(log_viewing_7days = log(viewing_7days)) 

# Check the model
model <- lm(log_viewing_7days ~ 
              n_releases_window +
              original +
              variety +
              n_releases_window:original +
              n_releases_window:variety +
              IMDb_rating +
              media_type +
              n_releases_prev7 +
              factor(service) +
              factor(year_month),
            data = titles)

model

summary(model)
tidy(model)
glance(model)

# 1. Check for linearity
plot(model, which = 1)

png("src/output/figure_4.png", width = 800, height = 600)
plot(model, which = 1)
dev.off()

# 2. Check for independence
dwtest(model)

coeftest(model, vcov = vcovCL(model, cluster = ~ window_start))

plot(titles$window_start, resid(model))
png("src/output/figure_5.png", width = 800, height = 600)
plot(titles$window_start, resid(model))
dev.off()

# 3. Homoscedasticity
bptest(model)

coeftest(model, vcov = vcovCL(model, cluster = ~ window_start))

# 4. Normality
##Q-Q plot
plot(model, which = 2)

png("src/output/figure_6.png", width = 800, height = 600)
plot(model, which = 2)
dev.off()

##histogram
residual_data <- data.frame(residuals = resid(model))

figure_7 <- ggplot(residual_data, aes(x = residuals)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30,
                 fill = "grey",
                 color = "black") +
  stat_function(fun = dnorm,
                args = list(mean = mean(residual_data$residuals),
                            sd = sd(residual_data$residuals)),
                color = "red",
                linewidth = 1) +
  labs(title = "Histogram of residuals",
       x = "Residuals",
       y = "Density") +
  theme_minimal()
figure_7

ggsave("src/output/figure_7.png", plot = figure_7, width = 8, height = 6)

##shapiro test
shapiro.test(resid(model))

