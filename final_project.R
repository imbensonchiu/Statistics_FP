#Import Packages
library(xts)
library(forecast)
library(prais)
library(olsrr)

#Import Data
data = read.csv("/Users/bensonchiu/Documents/111-2/Statistics/final_project/final_data_5.csv")
data$Time <-as.Date(paste0(data$Time, "-01"), format="%Y-%m-%d")
data$T_t <-as.numeric(data$T_t)
data_xts = as.xts(data, order.by = data$Time)
data_ts = ts(data_xts, frequency = 4)

#Descriptive statistics
plot(as.numeric(Male.Rate) ~ as.numeric(T_t), data = data_ts, ylim = c(0.5, 2.3), lty = 2,  xlab = "Year",
     ylab = "Rate per 100,000", xaxt = "n", col = "lightblue3")
abline(lm(as.numeric(Male.Rate) ~ as.numeric(T_t), data = data_ts), col = "lightblue4")
axis(1, at = seq(0, 72, by = 12), labels = 2016:2022)
points(as.numeric(Female.Rate) ~ as.numeric(T_t), data = data_ts, col = "indianred3")
abline(lm(as.numeric(Female.Rate) ~ as.numeric(T_t), data = data_ts), col = "indianred4")

#The regression for the female - starts from 2020-2
pw_f_2020_2 <- prais_winsten(as.numeric(Female.Rate) ~ as.numeric(Type) + as.numeric(T_t) + as.numeric(P_t_2020_2) + fourier(data_ts, K = 2), data = data_ts, index = "Time")
summary(pw_f_2020_2)
#Regression plot
plot(as.numeric(Female.Rate) ~ as.numeric(T_t), data = data_ts, type = "l", ylim = c(0, 2.5), lty = 2,  xlab = "Year",
     ylab = "Rate per 100,000", xaxt = "n", col = "rosybrown")

axis(1, at = seq(0, 72, by = 12), labels = 2016:2022)
beta_0 = pw_f_2020_2$coefficients["(Intercept)"]
beta_1 = pw_f_2020_2$coefficients["as.numeric(T_t)"]
beta_2 = pw_f_2020_2$coefficients["as.numeric(Type)"]
beta_3 = pw_f_2020_2$coefficients["as.numeric(P_t_2020_2)"]
abline(a = beta_0 , b = beta_1, col = "royalblue4")
segments(50, (beta_1) * 50 + beta_2 + beta_0, 74, (beta_3) * (74 - 50) + (beta_1) * 50 + beta_2 + beta_0, col = "tomato3")
abline(v = 50, lty = 3)

#The regression for the male - starts from 2020-2
pw_m_2020_2 <- prais_winsten(as.numeric(Male.Rate) ~ as.numeric(Type) + as.numeric(T_t) + as.numeric(P_t_2020_2) + fourier(data_ts, K = 2), data = data_ts, index = "Time")
summary(pw_m_2020_2)
#Regression plot
points(as.numeric(Male.Rate) ~ as.numeric(T_t), data = data_ts, type = "l", lty = 2, col = "slategray")

axis(1, at = seq(0, 72, by = 12), labels = 2016:2022)
beta_0 = pw_m_2020_2$coefficients["(Intercept)"]
beta_1 = pw_m_2020_2$coefficients["as.numeric(T_t)"]
beta_2 = pw_m_2020_2$coefficients["as.numeric(Type)"]
beta_3 = pw_m_2020_2$coefficients["as.numeric(P_t_2020_2)"]
abline(a = beta_0 , b = beta_1, col = "royalblue4")
segments(50, (beta_1) * 50 + beta_2 + beta_0, 74, (beta_3) * (74 - 50) + (beta_1) * 50 + beta_2 + beta_0, col = "tomato3")
abline(v = 50, lty = 3)

#Homoscedasticity
plot(y = pw_f_2020_2$residuals, x = pw_f_2020_2$fitted.values, ylab = "Residual", xlab = "Fitted Values", ylim = c(-1, 1), main = "Female Rate (from 2020-2)")
#Normality
hist(pw_f_2020_2$residuals)
qqnorm(pw_f_2020_2$residuals)
ols_test_normality(pw_f_2020_2$residuals)
#Homoscedasticity
plot(y = pw_m_2020_2$residuals, x = pw_m_2020_2$fitted.values, ylab = "Residual", xlab = "Fitted Values", ylim = c(-1, 1), main = "Male Rate (from 2020-2)")
#Normality
hist(pw_m_2020_2$residuals)
qqnorm(pw_m_2020_2$residuals)
ols_test_normality(pw_m_2020_2$residuals)

###############################################################

#The regression for the female - starts from 2021-5
pw_f_2021_5 <- prais_winsten(as.numeric(Female.Rate) ~ as.numeric(Type) + as.numeric(T_t) + as.numeric(P_t_2021_5) + fourier(data_ts, K = 2), data = data_ts, index = "Time")
summary(pw_f_2021_5)
#Regression plot
plot(as.numeric(Female.Rate) ~ as.numeric(T_t), data = data_ts, type = "l", ylim = c(0, 2.5), lty = 2,  xlab = "Year",
     ylab = "Rate per 100,000", xaxt = "n", col = "rosybrown")

axis(1, at = seq(0, 72, by = 12), labels = 2016:2022)
beta_0 = pw_f_2021_5$coefficients["(Intercept)"]
beta_1 = pw_f_2021_5$coefficients["as.numeric(T_t)"]
beta_2 = pw_f_2021_5$coefficients["as.numeric(Type)"]
beta_3 = pw_f_2021_5$coefficients["as.numeric(P_t_2021_5)"]
abline(a = beta_0 , b = beta_1, col = "royalblue4", x = c(0,65))
segments(65, (beta_1) * 65 + beta_2 + beta_0, 74, (beta_3) * (74 - 65) + (beta_1) * 65 + beta_2 + beta_0, col = "tomato3")
abline(v = 65, lty = 3)

#The regression for the male - starts from 2021-5
pw_m_2021_5 <- prais_winsten(as.numeric(Male.Rate) ~ as.numeric(Type) + as.numeric(T_t) + as.numeric(P_t_2021_5) + fourier(data_ts, K = 2), data = data_ts, index = "Time")
summary(pw_m_2021_5)
#Regression plot
points(as.numeric(Male.Rate) ~ as.numeric(T_t), data = data_ts, type = "l", lty = 2, col = "slategray")
axis(1, at = seq(0, 72, by = 12), labels = 2016:2022)
beta_0 = pw_m_2021_5$coefficients["(Intercept)"]
beta_1 = pw_m_2021_5$coefficients["as.numeric(T_t)"]
beta_2 = pw_m_2021_5$coefficients["as.numeric(Type)"]
beta_3 = pw_m_2021_5$coefficients["as.numeric(P_t_2021_5)"]
abline(a = beta_0 , b = beta_1, col = "royalblue4", x = c(0,65))
segments(65, (beta_1) * 65 + beta_2 + beta_0, 74, (beta_3) * (74 - 65) + (beta_1) * 65 + beta_2 + beta_0, col = "tomato3")
abline(v = 65, lty = 3)
#Homoscedasticity
plot(y = pw_f_2021_5$residuals, x = pw_f_2021_5$fitted.values, ylab = "Residual", xlab = "Fitted Values", ylim = c(-1, 1), main = "Female Rate (from 2021-5)")
#Normality
hist(pw_f_2021_5$residuals)
qqnorm(pw_f_2021_5$residuals)
ols_test_normality(pw_f_2021_5$residuals)

#Homoscedasticity
plot(y = pw_m_2021_5$residuals, x = pw_m_2021_5$fitted.values, ylab = "Residual", xlab = "Fitted Values", ylim = c(-1, 1), main = "Male Rate (from 2021-5)")
#Normality
hist(pw_m_2021_5$residuals)
qqnorm(pw_m_2021_5$residuals)
ols_test_normality(pw_m_2021_5$residuals)









