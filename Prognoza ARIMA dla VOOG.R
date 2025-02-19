# Instalacja wymaganych pakietów
if (!require(quantmod)) install.packages("quantmod")
if (!require(forecast)) install.packages("forecast")
if (!require(tseries)) install.packages("tseries")

library(quantmod)
library(forecast)
library(tseries)

# Pobranie danych
symbol <- "VOOG"
start_date <- "2022-01-01"
end_date <- "2025-01-29"

getSymbols(symbol, src = "yahoo", from = start_date, to = end_date)

# Wybór cen zamknięcia
etf <- Cl(get(symbol))

# Tworzenie szeregu czasowego
daily_avg_ts <- ts(etf, start = c(2022, 1), frequency = 252)

# Wykres szeregu czasowego
plot(daily_avg_ts, main = "Szereg czasowy - ceny zamknięcia VOOG", xlab = "Dni", ylab = "Cena zamknięcia")

# Sprawdzenie stacjonarności
adf_result <- adf.test(daily_avg_ts, alternative = "stationary")
pp_result <- pp.test(daily_avg_ts)
print(adf_result)
print(pp_result)

# Wykresy ACF i PACF
acf(daily_avg_ts, lag.max = 20, main = "Autokorelacja ACF")
pacf(daily_avg_ts, lag.max = 20, main = "Autokorelacja PACF")

# Modelowanie ARIMA
fit <- auto.arima(daily_avg_ts)
summary(fit)

# Prognoza na 2 dni
days_ahead <- 2
forecasted <- forecast(fit, h = days_ahead)
print(forecasted)

# Ocena modelu
accuracy(fit)

# Sprawdzenie stabilności modelu
ar_coeffs <- coef(fit)[grep("^ar", names(coef(fit)))]
ma_coeffs <- coef(fit)[grep("^ma", names(coef(fit)))]

if (length(ar_coeffs) > 0) {
  ar_poly <- c(1, -ar_coeffs)
  ar_roots <- polyroot(ar_poly)
  ar_inverse_roots <- abs(1 / ar_roots)
  print("AR Inverse Roots:")
  print(ar_inverse_roots)
  if (all(ar_inverse_roots < 1)) {
    print("The AR component is stationary.")
  } else {
    print("The AR component is not stationary.")
  }
} else {
  print("No AR terms in the model.")
}

if (length(ma_coeffs) > 0) {
  ma_poly <- c(1, -ma_coeffs)
  ma_roots <- polyroot(ma_poly)
  ma_inverse_roots <- abs(1 / ma_roots)
  print("MA Inverse Roots:")
  print(ma_inverse_roots)
  if (all(ma_inverse_roots < 1)) {
    print("The MA component is invertible.")
  } else {
    print("The MA component is not invertible.")
  }
} else {
  print("No MA terms in the model.")
}

# Wykres prognozy
last_17_days <- tail(etf, 17)
last_17_dates <- tail(index(get(symbol)), 17)
forecast_dates <- seq(from = as.Date(last(last_17_dates)) + 1, by = "day", length.out = days_ahead)
all_dates <- c(last_17_dates, forecast_dates)
combined_values <- c(last_17_days, forecasted$mean)

plot(all_dates, combined_values, type = "o", col = "blue", lwd = 2, pch = 16,
     main = "Prognoza 2-dniowa dla VOOG",
     xlab = "Data", ylab = "Cena zamknięcia", xaxt = "n")
axis(1, at = all_dates, labels = format(all_dates, "%Y-%m-%d"), las = 2, cex.axis = 0.7)
lines(forecast_dates, forecasted$mean, col = "red", type = "o", lwd = 2, pch = 16)
legend("topleft", legend = c("Ostatnie 17 dni", "Prognoza 2-dniowa"), 
       col = c("blue", "red"), lty = 1, lwd = 2, pch = 16, bty = "o", inset = 0.02, 
       cex = 0.8, x.intersp = 0.5, y.intersp = 0.8)

