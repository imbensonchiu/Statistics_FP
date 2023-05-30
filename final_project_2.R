library(xts)
library(forecast)
library(prais)
data = read.csv("/Users/bensonchiu/Documents/111-2/Statistics/final_project/final_data_4.csv")
data$Time <-as.Date(paste0(data$Time, "-01"), format="%Y-%m-%d")

data$T_t <-as.numeric(data$T_t)
data_xts = as.xts(data, order.by = data$Time)
data_ts = ts(data_xts, frequency = 4)
pw <- prais_winsten(as.numeric(Rate) ~ as.numeric(Type) + as.numeric(T_t) + as.numeric(P_t_2020_2) + fourier(data_ts, K = 2), data = data_ts, index = "Time")
summary(pw)

#Homoscedasticity
plot(y = pw$residuals, x = pw$fitted.values, ylab = "Residual", xlab = "Fitted Values", ylim = c(-1, 1))

plot(as.numeric(Rate) ~ as.numeric(T_t), data = data_ts, type = "l", ylim = c(0, 2), lty = 2,  xlab = "Year",
     ylab = "Rate per 100,000", xaxt = "n")
axis(1, at = seq(0, 72, by = 12), labels = 2016:2022)



shapiro.test(pw$residual)