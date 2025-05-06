library(ggplot2)
library(gridExtra)
library(hydroGOF)
set.seed(666420) 
library(e1071)
library(Metrics)        # For mae()


# Function to generate data with sine-exponential relationship
generate_sine_exp_data = function(n = 1000, noise_sd = 10) {
  x = runif(n, 0, 6*pi)
  # True model: y = sin(x) * exp(0.2*x) + error
  y = sin(x) * exp(0.2*x) + rnorm(n, 0, noise_sd)
  data.frame(x = x, y = y)
}

# Generate both datasets
sine_exp_data = generate_sine_exp_data()

# Visualize the sine-exponential data
p1 = ggplot(sine_exp_data, aes(x = x, y = y)) +
  geom_point(alpha = 0.6) +
  ggtitle("Sine-Exponential Relationship") +
  theme_minimal()

p1


#Predict using SVM regression Base 
modelsvm = svm(y~x,data = sine_exp_data)
predYsvm = predict(modelsvm, sine_exp_data)
RMSEsvm=rmse(predYsvm,sine_exp_data$y)
RMSEsvm

#Overlay SVM Predictions on Scatter Plot
plot(sine_exp_data$x,sine_exp_data$y)
points(sine_exp_data$x, predYsvm, col = "red", pch=16)
points(sine_exp_data$x,pred_scale,col="blue")
##Calculate parameters of the SVR model

#Find value of W
W = t(modelsvm$coefs) %*% modelsvm$SV

#Find value of b
b = modelsvm$rho
b


#Tune the SVM model
OptModelsvm=tune(svm, y~x, data=sine_exp_data,  ranges = list(
  epsilon = seq(0, 1, 0.1),
  cost = 2^(-1:7)))

#Print optimum value of parameters
print(OptModelsvm)

#Plot the perfrormance of SVM Regression model
plot(OptModelsvm)

#Find out the best model
BstModel=OptModelsvm$best.model

#Predict Y using best model
PredYBst=predict(BstModel,sine_exp_data)

#Calculate RMSE of the best model 
RMSEBst=rmse(PredYBst,sine_exp_data$y)

##Calculate parameters of the Best SVR model

#Find value of W
W = t(BstModel$coefs) %*% BstModel$SV

#Find value of b
b = BstModel$rho
b

plot(sine_exp_data, pch=16)
points(sine_exp_data$x, PredYBst, col = "red", pch=4)

#linear regression
base = lm(y~x,data = sine_exp_data)
pred_base = predict(base,sine_exp_data)
points(sine_exp_data$x, pred_base, col = "darkgreen", pch=4)
rsme_base = rmse(pred_base,sine_exp_data$y)

library(ggplot2)
p5 = ggplot(sine_exp_data, aes(x = x)) +
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


res_plt = ggplot(sine_exp_data, aes(x = PredYBst, y = BstModel$residuals)) +
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
# Perform Anderson-Darling test
library(nortest)
ad_test = ad.test(BstModel$residuals)
# Create plot with test results displayed on it
p6 = ggplot(sine_exp_data, aes(x = BstModel$residuals)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_density(aes(y = ..count.. * (max(BstModel$residuals) - min(BstModel$residualsl))/30), 
               color = "red", size = 1) +
  labs(title = "Distribution of Residuals from P-splines Fit",
       subtitle = paste("Anderson-Darling test: A =", round(ad_test$statistic, 4), 
                        ", p-value =", format(ad_test$p.value, scientific = TRUE, digits = 4)),
       x = "Residual", y = "Count") +
  theme_minimal() +
  theme(plot.subtitle = element_text(size = 11, face = "italic"))

# Save the plot with the test results
p6
jpeg("plot_residuals_hist_svr.jpg", width = 1200, height = 800, quality = 100)
print(p6)
dev.off()

# MAE
mae_lm = mae(sine_exp_data$y, pred_base)
mae_svr_base = mae(sine_exp_data$y, predYsvm)
mae_svr_tuned = mae(sine_exp_data$y, PredYBst)


r2 = function(actual, predicted) {
  ss_res = sum((actual - predicted)^2)
  ss_tot = sum((actual - mean(actual))^2)
  1 - (ss_res / ss_tot)
}
# R-squared
r2_lm = r2(pred_base, sine_exp_data$y)
r2_svr_base = r2(predYsvm, sine_exp_data$y)
r2_svr_tuned = r2(PredYBst, sine_exp_data$y)


model_metrics <- data.frame(
  Model = c("Linear Regression", "SVR Base", "SVR Tuned"),
  RMSE = c(rsme_base, RMSEsvm, RMSEBst),
  MAE = c(mae_lm, mae_svr_base, mae_svr_tuned),
  R2  = c(r2_lm, r2_svr_base, r2_svr_tuned)
)

print(model_metrics)
write.csv(model_metrics, "model_metrics_summary.csv", row.names = FALSE)
