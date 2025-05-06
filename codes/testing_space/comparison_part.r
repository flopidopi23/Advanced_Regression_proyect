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
  geom_point(data = data, aes(x = x, y = y), alpha = 0.3, size = 0.8, color = "#333333") + #JC DID THIS!!!
  geom_line(data = pred_data, aes(x = x, y = y, color = "P-splines fit"), size = 2.0) + #JC DID THIS!!!
  geom_line(data = true_data, aes(x = x, y = y, color = "True function"), 
            linetype = "dashed", size = 2.0) + #JC DID THIS!!!
  scale_color_manual(values = c("P-splines fit" = "#451571", "True function" = "#BB0A1E")) + #JC DID THIS!!!
  labs(title = "P-splines Fit to Sine-Exponential Data",
       x = "x", y = "y",
       color = "Curves") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid = element_line(color = "#EEEEEE"), #JC DID THIS!!!
        axis.line = element_line(color = "#666666")) #JC DID THIS!!!
ggsave("psplines_fit.png", p1_spline, width = 10, height = 7, dpi = 300)

# Create residual plot
residuals <- data$y - predict(model, data)
resid_data <- data.frame(x = data$x, residual = residuals)

p2_spline <- ggplot(resid_data, aes(x = x, y = residual)) +
  geom_point(alpha = 0.3, size = 0.8, color = "#333333") + #JC DID THIS!!!
  geom_hline(yintercept = 0, color = "#BB0A1E", linetype = "dashed", size = 1.2) + #JC DID THIS!!!
  labs(title = "Residuals from P-splines Fit",
       x = "x", y = "Residual") +
  theme_minimal() +
  theme(panel.grid = element_line(color = "#EEEEEE"), #JC DID THIS!!!
        axis.line = element_line(color = "#666666")) #JC DID THIS!!!
ggsave("psplines_residuals.png", p2_spline, width = 10, height = 7, dpi = 300)

# Create diagnostic plots
par(mfrow = c(2, 2))
png("psplines_diagnostics.png", width = 10, height = 10, units = "in", res = 300)
gam.check(model)
dev.off()

# Create a plot showing different numbers of knots
knot_comparison <- list()
knot_numbers <- c(10, 20, 30, 40, 80) # Added k=30 #JC DID THIS!!!

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
    geom_point(data = data, aes(x = x, y = y), alpha = 0.1, size = 0.8, color = "#333333") + #JC DID THIS!!!
    geom_line(data = subset(knot_results, knots == "k = 10"), aes(x = x, y = y, color = "k = 10"), 
              size = 1.2, linetype = "solid") + #JC DID THIS!!!
    geom_line(data = subset(knot_results, knots == "k = 20"), aes(x = x, y = y, color = "k = 20"), 
              size = 1.2, linetype = "solid") + #JC DID THIS!!!
    geom_line(data = subset(knot_results, knots == "k = 30"), aes(x = x, y = y, color = "k = 30"), 
              size = 1.2, linetype = "solid") + #JC DID THIS!!!
    geom_line(data = subset(knot_results, knots == "k = 40"), aes(x = x, y = y, color = "k = 40"), 
              size = 1) + #JC DID THIS!!!
    geom_line(data = subset(knot_results, knots == "k = 80"), aes(x = x, y = y, color = "k = 80"), 
              size = 1.5, linetype = "dotted") + #JC DID THIS!!!
    geom_line(data = true_data, aes(x = x, y = y, color = "True function"), 
              linetype = "dashed", size = 2.0) + #JC DID THIS!!!
    scale_color_manual(values = c("k = 10" = "#2986cc", "k = 20" = "#ce7e00", #JC DID THIS!!!
                                  "k = 30" = "#38761d", "k = 40" = "#451571", #JC DID THIS!!!
                                  "k = 80" = "#2A0E46", "True function" = "#BB0A1E")) + #JC DID THIS!!!
    labs(title = "Comparison of Different Numbers of Basis Functions",
         subtitle = "P-splines with varying flexibility",
         x = "x", y = "y",
         color = "Model") +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid = element_line(color = "#EEEEEE"), #JC DID THIS!!!
          axis.line = element_line(color = "#666666")) #JC DID THIS!!!

# Save knots comparison plot as high-resolution PNG
ggsave("psplines_knots_comparison.png", p4_spline, width = 10, height = 7, dpi = 300)

# Create residual distribution plot with Anderson-Darling test results
library(nortest)  # Load package for normality tests

# Perform Anderson-Darling test
ad_test <- ad.test(residuals)

# Create plot with test results displayed on it
p5_spline <- ggplot(resid_data, aes(x = residual)) +
    geom_histogram(bins = 30, fill = "#7c5b9b", color = "black", alpha = 0.7) + #JC DID THIS!!!
    geom_density(aes(y = ..count.. * (max(resid_data$residual) - min(resid_data$residual))/30), 
                 color = "#BB0A1E", size = 1.5) + #JC DID THIS!!!
    labs(title = "Distribution of Residuals from P-splines Fit",
         subtitle = paste("Anderson-Darling test: A =", round(ad_test$statistic, 4), 
                          ", p-value =", format(ad_test$p.value, scientific = TRUE, digits = 4)),
         x = "Residual", y = "Count") +
    theme_minimal() +
    theme(plot.subtitle = element_text(size = 11, face = "italic"),
          panel.grid = element_line(color = "#EEEEEE"), #JC DID THIS!!!
          axis.line = element_line(color = "#666666")) #JC DID THIS!!!

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
    geom_point(data = data, aes(x = x, y = y), alpha = 0.1, size = 0.8, color = "#333333") + #JC DID THIS!!!
    geom_line(data = subset(penalty_results, penalty_order == "m = 1"), aes(x = x, y = y, color = "m = 1"), 
              size = 1.2, linetype = "solid") + #JC DID THIS!!!
    geom_line(data = subset(penalty_results, penalty_order == "m = 2"), aes(x = x, y = y, color = "m = 2"), 
              size = 1.8) + #JC DID THIS!!!
    geom_line(data = subset(penalty_results, penalty_order == "m = 3"), aes(x = x, y = y, color = "m = 3"), 
              size = 1.2, linetype = "dotted") + #JC DID THIS!!!
    geom_line(data = true_data, aes(x = x, y = y, color = "True function"), 
              linetype = "dashed", size = 2.0) + #JC DID THIS!!!
    scale_color_manual(values = c("m = 1" = "#38761d", "m = 2" = "#451571", #JC DID THIS!!!
                                  "m = 3" = "#ce7e00", "True function" = "#BB0A1E")) + #JC DID THIS!!!
    labs(title = "Comparison of Different Penalty Orders",
         subtitle = "P-splines with varying difference penalties",
         x = "x", y = "y",
         color = "Model") +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid = element_line(color = "#EEEEEE"), #JC DID THIS!!!
          axis.line = element_line(color = "#666666")) #JC DID THIS!!!
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

#===TUNING PERFORMANCE VISUALIZATION===
#Plot the perfrormance of SVM Regression model
plot(OptModelsvm)

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
plot(data, pch=16)
points(data$x, PredYBst, col = "red", pch=4)

#===LINEAR REGRESSION COMPARISON===
#linear regression
base = lm(y~x,data = data)
pred_base = predict(base,data)
points(data$x, pred_base, col = "darkgreen", pch=4)
rsme_base = rmse(pred_base,data$y)

#===MODEL COMPARISON VISUALIZATION===
library(ggplot2)
p5 = ggplot(data, aes(x = x)) +
    geom_point(aes(y = y), alpha = 0.3, color = "black")+
    geom_line(aes(y = predYsvm, color = "SVR Base")) +
    geom_line(aes(y = PredYBst, color = "SVR Tuned")) +
    geom_line(aes(y = pred_base, color = "Linear Model")) +
    scale_color_manual(values = c("SVR Base" = "blue", "SVR Tuned" = "red", "Linear Model" = "darkgreen")) +
    ggtitle("True vs Predicted") +
    ylab("y") +  theme_minimal() +
    labs(color = "Model")
p5
jpeg("plot_pred_vs_true_svm_p5.jpg", width = 1200, height = 800, quality = 100)
print(p5)
dev.off()

a = data.frame(model=c("Linear","SVR Base","SVR tune"),
               RMSE = c(rsme_base,RMSEsvm,RMSEBst))
write.csv(a,"rmse_svm.csv")


#===RESIDUAL ANALYSIS===
res_plt = ggplot(data, aes(x = PredYBst, y = BstModel$residuals)) +
    geom_point(alpha = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "SVR Tuned: Residuals vs Fitted",
         x = "Fitted Values (Predicted)",
         y = "Residuals") +
    theme_minimal()
res_plt
jpeg("plot_residuals_svr.jpg", width = 1200, height = 800, quality = 100)
print(res_plt)
dev.off()

#===RESIDUAL DISTRIBUTION AND NORMALITY TESTING===
# Perform Anderson-Darling test
library(nortest)
ad_test = ad.test(BstModel$residuals)
# Create plot with test results displayed on it
p6 = ggplot(data, aes(x = BstModel$residuals)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) + #JC DID THIS!!!
    geom_density(aes(y = ..count.. * (max(BstModel$residuals) - min(BstModel$residuals))/30), #JC DID THIS!!!
                 color = "#BB0A1E", size = 1.5) + #JC DID THIS!!!
    labs(title = "Distribution of Residuals from SVR Tuned Model", #JC DID THIS!!!
         subtitle = paste("Anderson-Darling test: A =", round(ad_test$statistic, 4), 
                          ", p-value =", format(ad_test$p.value, scientific = TRUE, digits = 4)),
         x = "Residual", y = "Count") +
    theme_minimal() +
    theme(plot.subtitle = element_text(size = 11, face = "italic"),
          panel.grid = element_line(color = "#EEEEEE"), #JC DID THIS!!!
          axis.line = element_line(color = "#666666")) #JC DID THIS!!!

# Save the plot with the test results
p6
jpeg("plot_residuals_hist_svr.jpg", width = 1200, height = 800, quality = 100) #JC DID THIS!!!
print(p6)
dev.off()

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
r2_lm = r2(pred_base, data$y)
r2_svr_base = r2(predYsvm, data$y)
r2_svr_tuned = r2(PredYBst, data$y)


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

# Predictions for svm

# STEP 3) CALCULATE RMSE AND PICP METRICS

# STEP 4) MAKE A UNITED GRAPH WITH PICP OR OTHER INTERVAL VISIBLE


