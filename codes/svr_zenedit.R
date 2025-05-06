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
plot(data$x,data$y)
points(data$x, predYsvm, col = "red", pch=16)
points(data$x,pred_scale,col="blue")
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
