model <- lm(Volume ~ Price.Movement, data = AAPL)

t_stat <- coef(model)["Price.Movement"] / sqrt(vcov(model)["Price.Movement", "Price.Movement"])

t_stat
crit_val <- qt(0.975, df = nrow(AAPL) - 2)
crit_val
if (abs(t_stat) > crit_val) {  cat("Reject the null hypothesis")} else {
  cat("Do not reject the null hypothesis")
  
  install.packages("scatterplot3d")
  library(scatterplot3d)
  
  fit_2_sp <- scatterplot3d(AAPL$Volume, AAPL$Price.Movement, AAPL$Date, angle = 60, color = "dodgerblue", pch = 1, ylab = "Price Movement", xlab = "Volume", zlab = "Date")  
  
  model <- lm(Price.Movement ~ Volume + Date, data = AAPL)
  summary(model)  
  
  library(ggplot2)
  ggplot(data = AAPL, aes(x = Volume, y = Price.Movement)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(x = "Trading Volume", y = "Price Movement", title = "Relationship between Trading Volume and Price Movement")
  
  model <- nls(Price.Movement ~ a + b * Volume + c * Volume^2, data = AAPL, start = list(a = 0, b = 0, c = 0))
  summary(model)  
  
  plot(model$fitted.values, model$residuals^2, xlab="Fitted Values", ylab="Squared Residuals")
  abline(h=mean(model$residuals^2), col="red")  
  acf(model$residuals, main="Autocorrelation Function of Residuals")
  pacf(model$residuals, main="Partial Autocorrelation Function of Residuals")  