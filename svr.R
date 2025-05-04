library(ggplot2)
library(gridExtra)
library(hydroGOF)
set.seed(666420) 


# Function to generate data with sine-exponential relationship
generate_sine_exp_data <- function(n = 1000, noise_sd = 10) {
  x <- runif(n, 0, 6*pi)
  # True model: y = sin(x) * exp(0.2*x) + error
  y <- sin(x) * exp(0.2*x) + rnorm(n, 0, noise_sd)
  data.frame(x = x, y = y)
}

# Generate both datasets
sine_exp_data <- generate_sine_exp_data()

# Visualize the sine-exponential data
p1 <- ggplot(sine_exp_data, aes(x = x, y = y)) +
  geom_point(alpha = 0.6) +
  ggtitle("Sine-Exponential Relationship") +
  theme_minimal()

p1
library(e1071)

modelsvm = svm(y~x,sine_exp_data)
sine_exp_data$x_scale = scale(sine_exp_data$x)

#Predict using SVM regression
predYsvm = predict(modelsvm, sine_exp_data)
#scale version

model_scale = svm(y~x_scale,data = sine_exp_data)
pred_scale = predict(model_scale,sine_exp_data)
rmse_scal = rmse(pred_scale,sine_exp_data$y)
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
RMSEsvm=rmse(predYsvm,sine_exp_data$y)
RMSEsvm


#Tune the SVM model
OptModelsvm=tune(svm, y~x, data=sine_exp_data,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))

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
points(sine_exp_data$x, predYsvm, col = "blue", pch=3)
points(sine_exp_data$x, PredYBst, col = "red", pch=4)

base = lm(y~x,data = sine_exp_data)
pred_base = predict(base,sine_exp_data)
points(sine_exp_data$x, pred_base, col = "darkgreen", pch=4)
rsme_base = rmse(pred_base,sine_exp_data$y)
