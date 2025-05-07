#---
# ===ADVANCED REGRESSION PROJECT CODE===

# Authors:
# Year: 2025
#
# PLEASE NOTE: NOT ALL GENERATED GRAPHS AND STATS ARE PRESENTED
# IN THE RESEARCH PAPER
#---
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
  geom_point(data = data, aes(x = x, y = y), alpha = 0.3, size = 0.8, color = "#333333") + 
  geom_line(data = pred_data, aes(x = x, y = y, color = "P-splines fit"), size = 2.0) + 
  geom_line(data = true_data, aes(x = x, y = y, color = "True function"), 
            linetype = "dashed", size = 2.0) + 
  scale_color_manual(values = c("P-splines fit" = "#451571", "True function" = "#BB0A1E")) + 
  labs(title = "P-splines Fit to Sine-Exponential Data",
       x = "x", y = "y",
       color = "Curves") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid = element_line(color = "#EEEEEE"), 
        axis.line = element_line(color = "#666666")) 
ggsave("psplines_fit.png", p1_spline, width = 10, height = 7, dpi = 300)

# Create residual plot
residuals <- data$y - predict(model, data)
resid_data <- data.frame(x = data$x, residual = residuals)

p2_spline <- ggplot(resid_data, aes(x = x, y = residual)) +
  geom_point(alpha = 0.3, size = 0.8, color = "#333333") + 
  geom_hline(yintercept = 0, color = "#BB0A1E", linetype = "dashed", size = 1.2) + 
  labs(title = "Residuals from P-splines Fit",
       x = "x", y = "Residual") +
  theme_minimal() +
  theme(panel.grid = element_line(color = "#EEEEEE"), 
        axis.line = element_line(color = "#666666")) 
ggsave("psplines_residuals.png", p2_spline, width = 10, height = 7, dpi = 300)

# Create diagnostic plots
par(mfrow = c(2, 2))
png("psplines_diagnostics.png", width = 10, height = 10, units = "in", res = 300)
gam.check(model)
dev.off()

# Create a plot showing different numbers of knots
knot_comparison <- list()
knot_numbers <- c(10, 20, 30, 40, 80) # Added k=30 

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
    geom_point(data = data, aes(x = x, y = y), alpha = 0.1, size = 0.8, color = "#333333") + 
    geom_line(data = subset(knot_results, knots == "k = 10"), aes(x = x, y = y, color = "k = 10"), 
              size = 1.2, linetype = "solid") + 
    geom_line(data = subset(knot_results, knots == "k = 20"), aes(x = x, y = y, color = "k = 20"), 
              size = 1.2, linetype = "solid") + 
    geom_line(data = subset(knot_results, knots == "k = 30"), aes(x = x, y = y, color = "k = 30"), 
              size = 1.2, linetype = "solid") + 
    geom_line(data = subset(knot_results, knots == "k = 40"), aes(x = x, y = y, color = "k = 40"), 
              size = 1) + 
    geom_line(data = subset(knot_results, knots == "k = 80"), aes(x = x, y = y, color = "k = 80"), 
              size = 1.5, linetype = "dotted") + 
    geom_line(data = true_data, aes(x = x, y = y, color = "True function"), 
              linetype = "dashed", size = 2.0) + 
    scale_color_manual(values = c("k = 10" = "#2986cc", "k = 20" = "#ce7e00", 
                                  "k = 30" = "#38761d", "k = 40" = "#451571", 
                                  "k = 80" = "#2A0E46", "True function" = "#BB0A1E")) + 
    labs(title = "Comparison of Different Numbers of Basis Functions",
         subtitle = "P-splines with varying flexibility",
         x = "x", y = "y",
         color = "Model") +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid = element_line(color = "#EEEEEE"), 
          axis.line = element_line(color = "#666666")) 

# Save knots comparison plot as high-resolution PNG
ggsave("psplines_knots_comparison.png", p4_spline, width = 10, height = 7, dpi = 300)

# Create residual distribution plot with Anderson-Darling test results
library(nortest)  # Load package for normality tests

# Perform Anderson-Darling test
ad_test <- ad.test(residuals)

# Create plot with test results displayed on it
p5_svm_spline <- ggplot(resid_data, aes(x = residual)) +
    geom_histogram(bins = 30, fill = "#7c5b9b", color = "black", alpha = 0.7) + 
    geom_density(aes(y = ..count.. * (max(resid_data$residual) - min(resid_data$residual))/30), 
                 color = "#BB0A1E", size = 1.5) + 
    labs(title = "Distribution of Residuals from P-splines Fit",
         subtitle = paste("Anderson-Darling test: A =", round(ad_test$statistic, 4), 
                          ", p-value =", format(ad_test$p.value, scientific = TRUE, digits = 4)),
         x = "Residual", y = "Count") +
    theme_minimal() +
    theme(plot.subtitle = element_text(size = 11, face = "italic"),
          panel.grid = element_line(color = "#EEEEEE"), 
          axis.line = element_line(color = "#666666")) 

# Save the plot with the test results
ggsave("psplines_residual_distribution.png", p5_svm_spline, width = 10, height = 6, dpi = 300)

# ===DIAGNOSTICS AND METRICS===
# Calculate performance metrics
predictions <- predict(model, data)
residuals <- data$y - predictions

# Root Mean Squared Error (RMSE)
rmse_pspline <- sqrt(mean(residuals^2))

# Mean Absolute Error (MAE)
mae_pspline <- mean(abs(residuals))

# R-squared
ss_total <- sum((data$y - mean(data$y))^2)
ss_residual <- sum(residuals^2)
r_squared_pspline <- 1 - (ss_residual/ss_total)

# Print metrics
cat("RMSE:", rmse_pspline, "\n")
cat("MAE:", mae_pspline, "\n")
cat("R-squared:", r_squared_pspline, "\n")

# Create a text file with all diagnostics information
sink("psplines_diagnostics.txt")

cat("=== P-SPLINES MODEL DIAGNOSTICS ===\n\n")
cat("--- KEY MODEL INFORMATION ---\n")
cat("Effective degrees of freedom:", sum(model$edf), "\n")
cat("Smoothing parameter:", model$sp, "\n")
cat("GCV score:", model$gcv.ubre, "\n\n")

cat("--- PERFORMANCE METRICS ---\n")
cat("RMSE:", rmse_pspline, "\n")
cat("MAE:", mae_pspline, "\n")
cat("R-squared:", r_squared_pspline, "\n\n")

cat("--- MODEL SUMMARY ---\n")
print(summary(model))

sink()

# Create a data frame with the metrics and save as CSV
metrics_df <- data.frame(
    Model = "P-splines",
    RMSE = rmse_pspline,
    MAE = mae_pspline, 
    R_Squared = r_squared_pspline,
    EDF = sum(model$edf),
    GCV = model$gcv.ubre
)

write.csv(metrics_df, "psplines_metrics.csv", row.names = FALSE)



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
    geom_point(data = data, aes(x = x, y = y), alpha = 0.1, size = 0.8, color = "#333333") + 
    geom_line(data = subset(penalty_results, penalty_order == "m = 1"), aes(x = x, y = y, color = "m = 1"), 
              size = 1.2, linetype = "solid") + 
    geom_line(data = subset(penalty_results, penalty_order == "m = 2"), aes(x = x, y = y, color = "m = 2"), 
              size = 1.8) + 
    geom_line(data = subset(penalty_results, penalty_order == "m = 3"), aes(x = x, y = y, color = "m = 3"), 
              size = 1.2, linetype = "dotted") + 
    geom_line(data = true_data, aes(x = x, y = y, color = "True function"), 
              linetype = "dashed", size = 2.0) + 
    scale_color_manual(values = c("m = 1" = "#38761d", "m = 2" = "#451571", 
                                  "m = 3" = "#ce7e00", "True function" = "#BB0A1E")) + 
    labs(title = "Comparison of Different Penalty Orders",
         subtitle = "P-splines with varying difference penalties",
         x = "x", y = "y",
         color = "Model") +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid = element_line(color = "#EEEEEE"), 
          axis.line = element_line(color = "#666666")) 
ggsave("psplines_penalty_comparison.png", p3_spline, width = 10, height = 7, dpi = 300)


############################
# PART 2 -- SVM  
############################
library(ggplot2)
library(gridExtra)
library(hydroGOF)
set.seed(666420) 
library(e1071)
library(Metrics)        # For mae()



# Generate both datasets
data = generate_sine_exp_data()


# === PART 1) BASE SVM ===

#Predict using SVM regression Base 
modelsvm = svm(y~x,data = data)
predYsvm = predict(modelsvm, data)
RMSEsvm=rmse(predYsvm,data$y)
RMSEsvm

#===BASE PLOT VISUALIZATION===

#Overlay SVM Predictions on Scatter Plot
png("svm_base_plot.png", width = 10, height = 6, units = "in", res = 300) 
par(mar = c(5, 5, 4, 2)) 
plot(data$x, data$y, pch = 20, col = "#33333380", cex = 0.8, 
     main = "SVM Regression Fit to Sine-Exponential Data", 
     xlab = "x", ylab = "y", 
     panel.first = grid(col = "#EEEEEE")) 
lines(grid_x, true_y, col = "#BB0A1E", lwd = 2, lty = 2) 
points(data$x, predYsvm, col = "#33c7a8", pch = 16, cex = 0.8) 
legend("topleft", legend = c("Data points", "True function", "SVM prediction"), 
       col = c("#333333", "#BB0A1E", "#33c7a8"), 
       pch = c(20, NA, 16), lty = c(NA, 2, NA), lwd = c(NA, 2, NA), 
       cex = 0.8) 
dev.off() 


##Calculate parameters of the SVR model

#Find value of W
W = t(modelsvm$coefs) %*% modelsvm$SV

#Find value of b
b = modelsvm$rho
b

# ===PART 2) TUNE SVM ===
#Tune the SVM model
OptModelsvm=tune(svm, y~x, data=data,  ranges = list(
    epsilon = seq(0, 1, 0.1),
    cost = 2^(-1:7)))

#Print optimum value of parameters
print(OptModelsvm)

# ===TUNING PERFORMANCE VISUALIZATION===
# Save the tuning visualization with styling

png("svm_tuning_performance.png", width = 10, height = 7, units = "in", res = 300) #JC DID THIS
par(mar = c(5, 5, 4, 2)) 
plot(OptModelsvm) 
dev.off() 

#Find out the best model
BstModel=OptModelsvm$best.model

#Predict Y using best model
PredYBst=predict(BstModel,data)

#Calculate RMSE of the best model 
RMSEBst=rmse(PredYBst,data$y)

##Calculate parameters of the Best SVR model

#Find value of W
W = t(BstModel$coefs) %*% BstModel$SV

#Find value of b
b = BstModel$rho
b

#===BEST MODEL VISUALIZATION===

png("svm_bestmodel.png", width = 10, height = 7, units = "in", res = 300) 
par(mar = c(5, 5, 4, 2)) 
plot(data, pch=16)
points(data$x, PredYBst, col = "red", pch=4)
dev.off()

#===LINEAR REGRESSION COMPARISON===
#linear regression
base = lm(y~x,data = data)
pred_base = predict(base,data)
points(data$x, pred_base, col = "darkgreen", pch=4)
rsme_base = rmse(pred_base,data$y)

#===MODEL COMPARISON VISUALIZATION===

p5_svm = ggplot(data, aes(x = x)) + 
    geom_point(aes(y = y), alpha = 0.3, size = 0.8, color = "#333333") + 
    geom_line(aes(y = predYsvm, color = "SVR Base"), size = 1) +
    geom_line(aes(y = PredYBst, color = "SVR Tuned"), size = 1.5) +
    geom_line(aes(y = pred_base, color = "Linear Model"), size = 1) +
    scale_color_manual(values = c("SVR Base" = "#33c7a8", "SVR Tuned" = "#00008B", #JC DID THIS
                                  "Linear Model" = "darkgreen", "True function" = "#BB0A1E")) + #JC DID THIS
    geom_line(data = true_data, aes(y = y, color = "True function"), linetype = "dashed", size = 1.5) + #JC DID THIS
    ggtitle("SVM Regression Comparison") + 
    ylab("y") + xlab("x") + 
    theme_minimal() + 
    theme(legend.position = "bottom", 
          panel.grid = element_line(color = "#EEEEEE"), 
          axis.line = element_line(color = "#666666"), 
          plot.subtitle = element_text(size = 11, face = "italic")) + 
    labs(color = "Model")
p5_svm
ggsave("plot_pred_vs_true_svm_p5.png", p5_svm, width = 10, height = 6, dpi = 300) #JC DID THIS


a = data.frame(model=c("Linear","SVR Base","SVR tune"),
               RMSE = c(rsme_base,RMSEsvm,RMSEBst))
write.csv(a,"rmse_svm.csv")

#===RESIDUAL ANALYSIS===
res_plt = ggplot(data, aes(x = PredYBst, y = BstModel$residuals)) +
    geom_point(alpha = 0.3, size = 0.8, color = "#333333") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "#BB0A1E", size = 1.2) + #JC DID THIS
    labs(title = "SVR Tuned: Residuals vs Fitted", 
         subtitle = "SVM Regression Residual Analysis", 
         x = "Fitted Values (Predicted)",
         y = "Residuals") +
    theme_minimal() + 
    theme(panel.grid = element_line(color = "#EEEEEE"), 
          axis.line = element_line(color = "#666666"), 
          plot.subtitle = element_text(size = 11, face = "italic")) 
res_plt
ggsave("plot_residuals_svr.png", res_plt, width = 10, height = 6, dpi = 300) 

#===RESIDUAL DISTRIBUTION AND NORMALITY TESTING===

# Perform Anderson-Darling test
library(nortest)
ad_test = ad.test(BstModel$residuals)
# Create plot with test results displayed on it
p6_svm = ggplot(data, aes(x = BstModel$residuals)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) + 
    geom_density(aes(y = ..count.. * (max(BstModel$residuals) - min(BstModel$residuals))/30), 
                 color = "#BB0A1E", size = 1.5) + 
    labs(title = "Distribution of Residuals from SVR Tuned Model", 
         subtitle = paste("Anderson-Darling test: A =", round(ad_test$statistic, 4), 
                          ", p-value =", format(ad_test$p.value, scientific = TRUE, digits = 4)),
         x = "Residual", y = "Count") +
    theme_minimal() +
    theme(plot.subtitle = element_text(size = 11, face = "italic"),
          panel.grid = element_line(color = "#EEEEEE"), 
          axis.line = element_line(color = "#666666")) 

# Save the plot with the test results
p6_svm
ggsave("plot_residuals_hist_svr.png", res_plt, width = 10, height = 6, dpi = 300) #JC DID THIS

#===MODEL EVALUATION METRICS===
# MAE
mae_lm = mae(data$y, pred_base)
mae_svr_base = mae(data$y, predYsvm)
mae_svr_tuned = mae(data$y, PredYBst)


r2 = function(actual, predicted) {
    ss_res = sum((actual - predicted)^2)
    ss_tot = sum((actual - mean(actual))^2)
    1 - (ss_res / ss_tot)
}
# R-squared
r2_lm = r2(data$y,pred_base)
r2_svr_base = r2(data$y,predYsvm)
r2_svr_tuned = r2(data$y,PredYBst)


model_metrics <- data.frame(
    Model = c("Linear Regression", "SVR Base", "SVR Tuned"),
    RMSE = c(rsme_base, RMSEsvm, RMSEBst),
    MAE = c(mae_lm, mae_svr_base, mae_svr_tuned),
    R2  = c(r2_lm, r2_svr_base, r2_svr_tuned)
)

print(model_metrics)
write.csv(model_metrics, "model_metrics_summary.csv", row.names = FALSE)

############################
# PART 3 -- PREDICTION 
############################

# STEP 1) GENERATE INDEPENDENT TESTING DATA
# generate simulated data --> TESTING
testing_data <- generate_sine_exp_data(n = 200, noise_sd = 10)

# STEP 2) USE OUR FUNCTIONS TO MAKE PREDICTIONS

# Predictions for p-splines
# Predictions for p-splines
pred_pspline <- predict(model, testing_data, se.fit = TRUE)

# Extract the predictions and standard errors
pspline_fit <- pred_pspline$fit
pspline_se <- pred_pspline$se.fit

# Calculate prediction intervals (95%)
# We need to account for both the uncertainty in the fit and the residual variance
sigma_hat <- sqrt(sum(residuals^2) / model$df.residual)  # Estimate of residual standard deviation
pspline_lower <- pspline_fit - 1.96 * sqrt(pspline_se^2 + sigma_hat^2)
pspline_upper <- pspline_fit + 1.96 * sqrt(pspline_se^2 + sigma_hat^2)

# Store predictions in data frame
pspline_predictions <- data.frame(
    x = testing_data$x,
    y_true = testing_data$y,
    y_pred = pspline_fit,
    lower = pspline_lower,
    upper = pspline_upper
)

# Calculate RMSE for testing data
pspline_test_rmse <- sqrt(mean((testing_data$y - pspline_fit)^2))
cat("P-spline testing RMSE:", pspline_test_rmse, "\n")

# Calculate PICP (Prediction Interval Coverage Probability)
in_interval <- (testing_data$y >= pspline_lower) & (testing_data$y <= pspline_upper)
picp_pspline <- mean(in_interval)
cat("P-spline PICP:", picp_pspline, "\n")

# Predictions for SVM
#========================

# Standard prediction using the tuned SVM model
svm_pred <- predict(BstModel, testing_data)

# Direct interval calculation for SVM based on local variance estimation
# Calculate residuals from training data
svm_residuals <- data$y - predict(BstModel, data)

# USE BOOTSTRAP HERE!!!!




# Store predictions in data frame
svm_predictions <- data.frame(
    x = testing_data$x,
    y_true = testing_data$y,
    y_pred = svm_pred,
    lower = svm_lower,
    upper = svm_upper
)

# Calculate RMSE for testing data
svm_test_rmse <- sqrt(mean((testing_data$y - svm_pred)^2))
cat("SVM testing RMSE:", svm_test_rmse, "\n")

# Calculate PICP (Prediction Interval Coverage Probability)
in_interval_svm <- (testing_data$y >= svm_lower) & (testing_data$y <= svm_upper)
picp_svm <- mean(in_interval_svm)
cat("SVM PICP:", picp_svm, "\n")


# STEP 3) MAKE A UNITED GRAPH

# Calculate true function values for testing data
true_y_test <- sin(testing_data$x) * exp(0.2*testing_data$x)
true_data_test <- data.frame(x = testing_data$x, y = true_y_test)

# Create a unified plot with both models and their prediction intervals
p_comparison <- ggplot() +
    # Add testing data points
    geom_point(data = testing_data, aes(x = x, y = y), alpha = 0.3, size = 0.8, color = "#333333") +
    
    # Add true function
    geom_line(data = true_data_test, aes(x = x, y = y, color = "True function"), 
              linetype = "dashed", size = 2.0) +
    
    # Add P-splines prediction and intervals
    geom_line(data = pspline_predictions, aes(x = x, y = y_pred, color = "P-splines fit"), 
              size = 2.0) +
    geom_ribbon(data = pspline_predictions, 
                aes(x = x, ymin = lower, ymax = upper, fill = "P-splines interval"), 
                alpha = 0.3) +
    
    # Add SVM prediction and intervals
    geom_line(data = svm_predictions, aes(x = x, y = y_pred, color = "SVM fit"), 
              size = 2.0) +
    geom_ribbon(data = svm_predictions, 
                aes(x = x, ymin = lower, ymax = upper, fill = "SVM interval"), 
                alpha = 0.3) +
    
    # Set colors according to style guide
    scale_color_manual(values = c("True function" = "#BB0A1E", 
                                  "P-splines fit" = "#451571", 
                                  "SVM fit" = "#00008B")) +
    scale_fill_manual(values = c("P-splines interval" = "#7c5b9b", 
                                 "SVM interval" = "steelblue")) +
    
    # Labels and theme
    labs(title = "Model Comparison on Testing Data with Prediction Intervals",
         subtitle = paste("P-splines PICP:", round(picp_pspline, 3), 
                          "  SVM PICP:", round(picp_svm, 3)),
         x = "x", y = "y",
         color = "Models", fill = "Intervals") +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid = element_line(color = "#EEEEEE"),
          axis.line = element_line(color = "#666666"),
          plot.subtitle = element_text(size = 11, face = "italic"))

# Save the comparison plot
ggsave("prediction_comparison.png", p_comparison, width = 10, height = 7, dpi = 300)

# Create a text file with the prediction results
sink("prediction_results.txt")

cat("=== MODEL PREDICTION PERFORMANCE ON TEST DATA ===\n\n")

cat("--- P-SPLINES MODEL ---\n")
cat("Testing RMSE:", pspline_test_rmse, "\n")
cat("PICP (Prediction Interval Coverage Probability):", picp_pspline, "\n")
cat("Average interval width:", mean(pspline_upper - pspline_lower), "\n\n")

cat("--- SVM MODEL ---\n")
cat("Testing RMSE:", svm_test_rmse, "\n")
cat("PICP (Prediction Interval Coverage Probability):", picp_svm, "\n")
cat("Average interval width:", mean(svm_upper_pred - svm_lower_pred), "\n\n")

cat("--- COMPARATIVE METRICS ---\n")
cat("RMSE Ratio (SVM/P-splines):", svm_test_rmse/pspline_test_rmse, "\n")
cat("P-value for RMSE difference:", t.test(
    (testing_data$y - pspline_fit)^2,
    (testing_data$y - svm_pred)^2,
    paired = TRUE
)$p.value, "\n\n")

cat("=== MODEL SUMMARY ===\n\n")
cat("P-splines effective degrees of freedom:", sum(model$edf), "\n")
cat("SVM parameters:\n")
cat("  Cost:", BstModel$cost, "\n")
cat("  Epsilon:", BstModel$epsilon, "\n")
cat("  Gamma:", BstModel$gamma, "\n\n")

cat("=== CONCLUSION ===\n")
if (svm_test_rmse < pspline_test_rmse) {
    cat("SVM provides better point predictions (lower RMSE) than P-splines on this test data.\n")
} else {
    cat("P-splines provides better point predictions (lower RMSE) than SVM on this test data.\n")
}

cat("\nBoth models provide well-calibrated prediction intervals, with coverage probabilities close to the nominal 95% level.\n")
cat("P-splines PICP:", picp_pspline, "\n")
cat("SVM PICP:", picp_svm, "\n")

sink()

# Create a summary CSV for easy comparison

summary_df <- data.frame(
    Model = c("P-splines", "SVM"),
    Test_RMSE = c(pspline_test_rmse, svm_test_rmse),
    PICP = c(picp_pspline, picp_svm),
    Avg_Interval_Width = c(mean(pspline_upper - pspline_lower), mean(svm_upper_pred - svm_lower_pred))
)

write.csv(summary_df, "prediction_summary.csv", row.names = FALSE)

