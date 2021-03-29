y_0 <- 0

white_noise <- rnorm(n = 100)
y <- rep(0, 101)

for (i in 1:100){
  y[i+1] = exp(-0.0004) * y[i] + 2 * white_noise[i]
}


plot(y, type = 'l')

model <- lm(y[-1] ~ y[-101])
summary(model)

plot(model$residuals, type = 'l')
r <- model$residuals

summary(lm(r[-1] ~ r[-100]))
var(y)



y <- rep(0, 1001)
y[1] <- rnorm(1, 0, 1)

A = 0.999

for (i in 1:1000){
  y[i+1] = A * y[i] + rnorm(1, 0, 1)
}

plot(y, type = 'l')

summary(lm(y[-1] ~ y[-1001]))

plot(x = y[1:1000], y = y[2:1001], col = 'red')
abline(a = 0, b = 1, lw = 2, col = 'blue')
var(y)

library(quantmod)
getSymbols('^FTSE', env = .GlobalEnv, from = '2002-03-15', to = '2009-02-24', periodicity = 'daily')
base_data <- FTSE$FTSE.Adjusted
base_data <- base_data[complete.cases(base_data)]
plot(base_data)

returns <- (as.numeric(base_data[-1]) - as.numeric(base_data[-nrow(base_data)])) / as.numeric(base_data[-nrow(base_data)]) * 100
plot(returns, type = 'l')

length(returns)
summary(lm(returns[-1] ~ returns[-length(returns)]))

len <- length(returns) - 200
AR <- rep(NA, len)
for (i in 1:len){
  start <- i
  end <- i + 200
  AR[i] <- summary(lm(returns[(i+1):(i+200)] ~ returns[i:(i+199)]))$coefficients[2,1]
}
plot(AR, type = 'l')

library(forecast)
base_data <- log(base_data)
plot(base_data)
trend <- ma(base_data, order = 10, centre = T)
plot(as.numeric(base_data), type = 'l')
lines(trend, col = 'blue', lw = 2.5)
trend_returns <- (trend[-length(trend)] - trend[-1]) / trend[-length(trend)]

beta <- 300
len <- length(trend_returns) - beta
AR <- rep(NA, len)
for (i in 1:len){
  start <- i
  end <- i + beta
  AR[i] <- summary(lm(trend_returns[(i+1):(i+beta)] ~ trend_returns[i:(i+(beta-1))]))$coefficients[2,1]
}
AR <- c(rep(NA, beta), AR)

plot(AR, type = 'l')
###########
alpha <- 5
len <- length(returns) - alpha
variance <- rep(NA, len)
for (i in 1:len){
  start <- i
  end <- i + alpha
  variance[i] <- var(returns[start:end])
}
variance <- c(rep(NA, alpha), variance)
lines(variance, col = 'red', lw = 2)
plot(variance, type = 'l')
plot(as.numeric(base_data), type = 'l')



AR <- c(rep(0, 200), AR)
par(mfrow = c(2,1))
plot(base_data)
plot(AR, type = 'l', ylim = c(0.85, 0.98))

par(mfrow = c(1,1))
plot(base_data)
trend <- ma(base_data, order = 20)
plot(trend, lw = 2)





