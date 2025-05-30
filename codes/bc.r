############################
# PART 0 - PREAMBLE
############################

# Function to generate data with sine-exponential relationship
set.seed(666420)
generate_sine_exp_data <- function(n = 1000, noise_sd = 10) {
  x <- runif(n, 0, 6*pi)
  # True model: y = sin(x) * exp(0.2*x) + error
  y <- sin(x) * exp(0.2*x) + rnorm(n, 0, noise_sd)
  data.frame(x = x, y = y)
}

# generate simulated data --> TRAINING
data <- generate_sine_exp_data(n = 1000, noise_sd = 10)

# Calculate true function values on a grid (for comparison)
grid_x <- seq(min(data$x), max(data$x), length.out = 500)
true_y <- sin(grid_x) * exp(0.2*grid_x)
true_data <- data.frame(x = grid_x, y = true_y)


############################
# PART 1 - P-SPLINES
############################
library(mgcv)    # For P-splines
library(ggplot2) 

# bs="ps": Use P-splines following Eilers & Marx
# k=40: Number of basis functions
# m=2: Second-order difference penalty
# Smoothing method: GCV (integrated in mgcv)
model <- gam(y ~ s(x, bs = "ps", k = 40, m = 2), data = data, method = "GCV.Cp")

# Generate predictions
new_x <- seq(min(data$x), max(data$x), length.out = 500)
new_data <- data.frame(x = new_x)
pred <- predict(model, new_data)
pred_data <- data.frame(x = new_x, y = pred)

# Create scatter plot with fitted curve
p1_spline <- ggplot() +
  geom_point(data = data, aes(x = x, y = y), alpha = 0.3, size = 0.8) +
  geom_line(data = pred_data, aes(x = x, y = y, color = "P-splines fit"), size = 1.2) +
  geom_line(data = true_data, aes(x = x, y = y, color = "True function"), 
            linetype = "dashed", size = 1.2) +
  scale_color_manual(values = c("P-splines fit" = "blue", "True function" = "red")) +
  labs(title = "P-splines Fit to Sine-Exponential Data",
       x = "x", y = "y",
       color = "Curves") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("psplines_fit.png", p1_spline, width = 10, height = 7, dpi = 300)

# Create residual plot
residuals <- data$y - predict(model, data)
resid_data <- data.frame(x = data$x, residual = residuals)

p2_spline <- ggplot(resid_data, aes(x = x, y = residual)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals from P-splines Fit",
       x = "x", y = "Residual") +
  theme_minimal()
ggsave("psplines_residuals.png", p2_spline, width = 10, height = 7, dpi = 300)

# Create diagnostic plots
par(mfrow = c(2, 2))
png("psplines_diagnostics.png", width = 10, height = 10, units = "in", res = 300)
gam.check(model)
dev.off()



# Create a plot showing different numbers of knots
knot_comparison <- list()
knot_numbers <- c(10, 20, 40, 80)

for (k in knot_numbers) {
  # Fit model with different number of knots
  temp_model <- gam(y ~ s(x, bs = "ps", k = k, m = 2), data = data, method = "GCV.Cp")
  
  # Make predictions
  temp_pred <- predict(temp_model, new_data)
  
  # Store results
  knot_comparison[[as.character(k)]] <- data.frame(
    x = new_x,
    y = temp_pred,
    knots = paste("k =", k),
    edf = sum(temp_model$edf)
  )
}

# Combine results
knot_results <- do.call(rbind, knot_comparison)

# Create knots comparison plot
p4_spline <- ggplot() +
  geom_point(data = data, aes(x = x, y = y), alpha = 0.1, size = 0.8) +
  geom_line(data = knot_results, aes(x = x, y = y, color = knots), size = 1.2) +
  geom_line(data = true_data, aes(x = x, y = y, color = "True function"), 
            linetype = "dashed", size = 1.2) +
  labs(title = "Comparison of Different Numbers of Basis Functions",
       subtitle = "P-splines with varying flexibility",
       x = "x", y = "y",
       color = "Model") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save knots comparison plot as high-resolution PNG
ggsave("psplines_knots_comparison.png", p4_spline, width = 10, height = 7, dpi = 300)


# Create residual distribution plot with Anderson-Darling test results
library(nortest)  # Load package for normality tests

# Perform Anderson-Darling test
ad_test <- ad.test(residuals)

# Create plot with test results displayed on it
p5_spline <- ggplot(resid_data, aes(x = residual)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
    geom_density(aes(y = ..count.. * (max(resid_data$residual) - min(resid_data$residual))/30), 
                 color = "red", size = 1) +
    labs(title = "Distribution of Residuals from P-splines Fit",
         subtitle = paste("Anderson-Darling test: A =", round(ad_test$statistic, 4), 
                          ", p-value =", format(ad_test$p.value, scientific = TRUE, digits = 4)),
         x = "Residual", y = "Count") +
    theme_minimal() +
    theme(plot.subtitle = element_text(size = 11, face = "italic"))

# Save the plot with the test results
ggsave("psplines_residual_distribution.png", p5_spline, width = 10, height = 6, dpi = 300)

# ===DIAGNOSTICS AND METRICS===
# Key model information
cat("Effective degrees of freedom:", sum(model$edf), "\n")
cat("Smoothing parameter:", model$sp, "\n")
cat("GCV score:", model$gcv.ubre, "\n")

# Print summary of the model
print(summary(model))



#=============================
#=============================


# ===UNECESSARY EXTRA===
# Create plot showing different PENALTY ORDERS (1,2 and 3) for comparison
penalty_comparison <- list()
penalty_orders <- c(1, 2, 3)

for (m in penalty_orders) {
  # Fit model with different penalty order
  temp_model <- gam(y ~ s(x, bs = "ps", k = 40, m = m), data = data, method = "GCV.Cp")
  # Make predictions
  temp_pred <- predict(temp_model, new_data)
  # Store results
  penalty_comparison[[as.character(m)]] <- data.frame(
    x = new_x,
    y = temp_pred,
    penalty_order = paste("m =", m),
    edf = sum(temp_model$edf)
  )
}
# Combine results
penalty_results <- do.call(rbind, penalty_comparison)
# Create comparison plot
p3_spline <- ggplot() +
  geom_point(data = data, aes(x = x, y = y), alpha = 0.1, size = 0.8) +
  geom_line(data = penalty_results, aes(x = x, y = y, color = penalty_order), size = 1.2) +
  geom_line(data = true_data, aes(x = x, y = y, color = "True function"), 
            linetype = "dashed", size = 1.2) +
  labs(title = "Comparison of Different Penalty Orders",
       subtitle = "P-splines with varying difference penalties",
       x = "x", y = "y",
       color = "Model") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("psplines_penalty_comparison.png", p3_spline, width = 10, height = 7, dpi = 300)

############################
# PART 2 -- SVM  
############################





############################
# PART 3 -- PREDICTION 
############################

# STEP 1) GENERATE INDEPENDENT TESTING DATA

# generate simulated data --> TESTING
testing_data <- generate_sine_exp_data(n = 200, noise_sd = 10)

# STEP 2) USE OUR FUNCTIONS TO MAKE PREDICTIONS

# Predictions for p-splines

# Predictions for svm

# STEP 3) CALCULATE RMSE AND PICP METRICS

# STEP 4) MAKE A UNITED GRAPH WITH PICP OR OTHER INTERVAL VISIBLE


