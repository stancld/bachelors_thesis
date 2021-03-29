library(quantmod)
getSymbols(Symbols = '^DJI', env = .GlobalEnv, from = '1986-01-01', to = '2018-12-31')

prices = DJI$DJI.Adjusted
prices = as.numeric(prices)
returns = 100 * (prices[-1] - prices[-8317]) / prices[-8317]

plot(returns, type = 'l')

avg = mean(returns)
std = (var(returns)) ** 0.5


x = seq(-5, 5, length.out = 1000)
hx = dnorm(x, mean = avg, sd = std)



h <- hist(returns, xlim = c(-5,5), breaks = 150, freq = F,
          c = 'grey',
          xlab = 'Daily returns (%)', ylab = 'Probability', font.lab = 2,
          main = 'Dow Jones Index vs Gaussian Distribution')
h
lines(x, hx, lw = 3, col = 'blue')


read.csv('C:/Data/DJIA.csv')


cumulative_loss <- 1
losses <- vector()

for (i in 1:8316) {
  if (returns[i] < 0){
    loss <- 1 + returns[i] / 100
    cumulative_loss <- cumulative_loss * loss
  } else if (cumulative_loss != 1) {
    losses <- append(losses, (cumulative_loss - 1) * 100)
    cumulative_loss <- 1
  }
}

losses

losses_tbl <- as.tibble(losses)
losses_tbl

bins <- seq(0, -30, length.out = 1e3)
sums <- vector()

for (i in 1:1000){
  occurrence <- as.numeric(losses_tbl %>%
                             filter(value <= bins[i] & value > bins[i+1]) %>%
                             count())
  sums <- append(sums, occurrence)
}

df <- as.data.frame(sums)
df$bins = bins


df <- as.tibble(df)
df
df <- df %>%
  filter(sums > 0)
df

plot(x = df$bins, y = log(df$sums))

A = 4
B = 0.78
x = seq(-8, 0, length.out = 100)
y = A + B*x

lines(x, y, lw = 2, col = 'blue')

## Using hustorical data
data <- Quandl("BCB/UDJIAD1", api_key="tPinskm3xuuY7uhTcaMs")
(data <- data[31263:73,])


prices = as.numeric(data$Value)
returns = 100 * (prices[-1] - prices[-length(prices)]) / prices[-length(prices)]

cumulative_loss <- 1
losses <- vector()

for (i in 1:length(returns)) {
  if (returns[i] < 0){
    loss <- 1 + returns[i] / 100
    cumulative_loss <- cumulative_loss * loss
  } else if (cumulative_loss != 1) {
    losses <- append(losses, (cumulative_loss - 1) * 100)
    cumulative_loss <- 1
  }
}

losses_tbl <- as.tibble(losses)
losses_tbl

bins <- seq(-32, 0, length.out = 250)
sums <- vector()

for (i in 1:250){
  occurrence <- as.numeric(losses_tbl %>%
                             filter(value >= bins[i] & value < bins[i+1]) %>%
                             count())
  if (i == 1){
    sums <- append(sums, occurrence)
  } else sums <- append(sums, sums[i-1] + occurrence)
}

df <- as.data.frame(sums)
df$bins = bins

df <- as.tibble(df) %>%
  distinct(sums, .keep_all = TRUE)
df

plot(x = df$bins, y = log(df$sums), type = 'p', pch = 3, cex = .45,
     xlab = 'Drawdowns (%)', ylab = 'Log(Cumulative Number)', font.lab = 2,
     main = 'Null hypothesis vs Negative drawdowns on DJIA', 
     xlim = c(-32,0), ylim = c(0,9))

A = max(log(sums)) - 0.02
B = 0.6
x = seq(-17, 0, length.out = 100)
y = A + B*x

lines(x, y, col = 'blue', lw = 2)

legend(-33, 9.25, legend = c('Null hypothesis', 'Drawdowns'), 
       col = c('blue', 'black'), lty = 1:0, pch = c('','+'))

## Simulating this on artifical data
(mean = mean(returns))
(std = (var(returns))**0.5)

returns = rnorm(n = 29932, mean = mean, sd = std)

cumulative_loss <- 1
losses <- vector()

for (i in 1:length(returns)) {
  if (returns[i] < 0){
    loss <- 1 + returns[i] / 100
    cumulative_loss <- cumulative_loss * loss
  } else if (cumulative_loss != 1) {
    losses <- append(losses, (cumulative_loss - 1) * 100)
    cumulative_loss <- 1
  }
}

losses_tbl <- as.tibble(losses)
losses_tbl

bins <- seq(-32, 0, length.out = 250)
sums <- vector()

for (i in 1:250){
  occurrence <- as.numeric(losses_tbl %>%
                             filter(value >= bins[i] & value < bins[i+1]) %>%
                             count())
  if (i == 1){
    sums <- append(sums, occurrence)
  } else sums <- append(sums, sums[i-1] + occurrence)
}

df <- as.data.frame(sums)
df$bins = bins

df <- as.tibble(df) %>%
  distinct(sums, .keep_all = TRUE)
df

plot(x = df$bins, y = log(df$sums), type = 'p', pch = 3, cex = .5,
     xlab = 'Drawdowns (%)', ylab = 'Log(Cumulative Number)', font.lab = 2,
     main = 'Null hypothesis vs Artificial negative drawdowns',
     xlim = c(-32,0), ylim = c(0,9))

A = max(log(sums)) 
B = 0.65
x = seq(-17, 0, length.out = 100)
y = A + B*x

lines(x, y, col = 'blue', lw = 2)

legend(-33, 9.25, legend = c('Null hypothesis', 'Drawdowns'), 
       col = c('blue', 'black'), lty = 1:0, pch = c('','+'))

       