library(quantmod)
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
library(tidyverse)

setwd("C:/Data/Skola/Bachelor's Thesis/R Codes")

getSymbols('^DJI', env = .GlobalEnv, from = '2001-03-15', to = '2008-07-01', periodicity = 'daily')
data <- log(`BTC-USD`[,6])



##### Period1
start <- which(time(data) == '2017-11-15') - 200
end <- which(time(data) == '2017-11-15')

(data1 <- data[start:end])
MA <- rep(NA, end - start + 1)
time_frame <- 1:200

gaussian_kernel <- function(sigma, r, t){
  (1 / (sqrt(2*pi) * sigma)) * exp(-((r-t)^2) / (2 * sigma^2))
}


for (t in time_frame){
  MA[t] <- sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 5, r = x, t = t) * data1[x])) /
    sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 5, r = x, t = t)))
}

residuals <- rep(NA, end - start + 1)
residuals[1:200] <- data1[1:200] - MA[1:200]
plot(residuals, type = 'l')

### 100 days
# AR(1)
residuals.ar <- residuals[1:200]
AR <- rep(NA, 100)

for (i in 1:100){
  AR[i] <- summary(lm(residuals.ar[(i+1):(i+99)] ~ residuals.ar[i:(i+98)]))$coefficients[2,1]
}
plot(AR, type = 'l')

# Variance
variance <- rep(NA, 100)
for (i in 1:100){
  start <- i
  end <- i + 100
  variance[i] <- sqrt(var(residuals[start:end]))
}
plot(variance, type = 'l')

# Kendall's tau
AR.tau <- AR[complete.cases(AR)]
m <- cbind(AR.tau[51:100], 1:50)
(AR.kendall = cor(m, method = 'kendall', use = 'pairwise')[2,1])

VAR.tau <- variance[complete.cases(variance)]
w <- cbind(VAR.tau[1:100], 1:100)
(VAR.kendall = cor(w, method = 'kendall', use = 'pairwise')[2,1])

# signifiance
N = 100 * 99 / 2
sigmic = sqrt((2*(2*N + 5)) / (9*N*(N-1)))

(Z.AR = AR.kendall / sigmic)
(Z.VAR = VAR.kendall / sigmic)


res <- residuals[complete.cases(residuals)]
adf.test(res)
kpss.test(res)
pp.test(res)






start <- which(time(data) == '2017-12-15') - 100
end <- which(time(data) == '2017-12-15')


(data1 <- (data[start:end]))
MA <- rep(NA, end - start + 1)
time_frame <- 1:100

gaussian_kernel <- function(sigma, r, t){
  (1 / (sqrt(2*pi) * sigma)) * exp(-((r-t)^2) / (2 * sigma^2))
}


for (t in time_frame){
  MA[t] <- sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 5, r = x, t = t) * data1[x])) /
    sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 5, r = x, t = t)))
}

residuals <- rep(NA, end - start + 1)
residuals[1:100] <- data1[1:100] - MA[1:100]
plot(residuals, type = 'l')


### 50 days
# AR(1)
residuals.ar <- residuals[1:100]
AR <- rep(NA, 50)

for (i in 1:50){
  AR[i] <- summary(lm(residuals.ar[(i+1):(i+49)] ~ residuals.ar[i:(i+48)]))$coefficients[2,1]
}
plot(AR, type = 'l')

# Variance
variance <- rep(NA, 50)
for (i in 1:50){
  start <- i
  end <- i + 50
  variance[i] <- sqrt(var(residuals[start:end]))
}
plot(variance, type = 'l')

# Kendall's tau
AR.tau <- AR[complete.cases(AR)]
m <- cbind(AR.tau[26:50], 1:25)
(AR.kendall = cor(m, method = 'kendall', use = 'pairwise')[2,1])

VAR.tau <- variance[complete.cases(variance)]
w <- cbind(VAR.tau[1:50], 1:50)
(VAR.kendall = cor(w, method = 'kendall', use = 'pairwise')[2,1])

# signifiance
N = 100 * 99 / 2
sigmic = sqrt((2*(2*N + 5)) / (9*N*(N-1)))

(Z.AR = AR.kendall / sigmic)
(Z.VAR = VAR.kendall / sigmic)








##### Fucking kendall for market correlations
### Data mining
#getSymbols(c("^DJI", "^GSPC","^N225", "^FTSE", '^MXX', '^GDAXI'), env=.GlobalEnv, from = '2004-01-01', to = '2008-05-24', periodicity = 'daily')

djia <- DJI$DJI.Adjusted
gspc <- GSPC$GSPC.Adjusted
nikkei <- N225$N225.Adjusted
ftse <- FTSE$FTSE.Adjusted
mex <- MXX$MXX.Adjusted
german <- GDAXI$GDAXI.Adjusted


start <- which(time(djia) == '2007-10-15') - 200
end <- start + 300

nasdaq <- nasdaq[start:end,]
gspc <- gspc[start:end,]
nikkei <- nikkei[start:end,]
ftse <- ftse[start:end,]
mex <- mex[start:end,]
german <- german[start:end,]

DATA <- as.data.frame(cbind(as.numeric(nasdaq), as.numeric(mex), as.numeric(gspc), as.numeric(nikkei), as.numeric(ftse), as.numeric(german)))
colnames(DATA) <- c('NASDAQ', 'MXX', 'GSPC', 'N225', 'FTSE', 'DAX')
DATA

for (j in 1:6){
  for (i in 1:301){
    if (is.na(DATA[i,j]) == T){
      DATA[i,j] <- DATA[i-1, j]
    }
  }
}

DATA[!complete.cases(DATA),]

(DATA <- log(DATA))



### Deriving residuals
# DJIA
time_frame <- 1:200
MA.djia <- rep(NA, 100)

for (t in time_frame){
  MA.djia[t] <- sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t) * DATA[x,1])) /
    sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t)))
}

residuals.djia <- DATA[1:200,1] - MA.djia[1:200]
plot(residuals.djia)



# MXX
time_frame <- 1:200
MA.mex <- rep(NA, 100)

for (t in time_frame){
  MA.mex[t] <- sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t) * DATA[x,2])) /
    sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t)))
}

residuals.mex <- DATA[1:200,2] - MA.mex[1:200]
plot(residuals.mex)

# SP500
time_frame <- 1:200
MA.gspc <- rep(NA, 100)

for (t in time_frame){
  MA.gspc[t] <- sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t) * DATA[x,3])) /
    sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t)))
}

residuals.gspc <- DATA[1:200,3] - MA.gspc[1:200]
plot(residuals.gspc)


# Nikkei
MA.n225 <- rep(NA, 100)
time_frame <- 1:200

for (t in time_frame){
  MA.n225[t] <- sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t) * DATA[x,4])) /
    sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t)))
}

residuals.n225 <- DATA[1:200,4] - MA.n225[1:200]
plot(residuals.n225)


# FTSE
MA.ftse <- rep(NA, 100)
time_frame <- 1:200

for (t in time_frame){
  MA.ftse[t] <- sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t) * DATA[x,5])) /
    sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t)))
}

residuals.ftse <- DATA[1:200,5] - MA.ftse[1:200]
plot(residuals.ftse)

# DAX
MA.ger <- rep(NA, 100)
time_frame <- 1:200

for (t in time_frame){
  MA.ger[t] <- sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t) * DATA[x,6])) /
    sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t)))
}

residuals.ger <- DATA[1:200,6] - MA.ger[1:200]
plot(residuals.ger)


# 2. computing correlations
djiaXmex <- rep(NA, 100)
for (i in 1:100){
  end <- i + 99
  djiaXmex[i] <- cor(x = residuals.djia[i:end],
                       y = residuals.mex[i:end])
}
djiaXmex <- c(rep(NA, 100), djiaXmex, rep(NA, 101))
plot(djiaXmex, type = 'l')

mex.tau <- djiaXmex[complete.cases(djiaXmex)]
m <- cbind(mex.tau, 1:100)
(mex.kendall = cor(m, method = 'kendall', use = 'pairwise')[2,1])


djiaXgspc <- rep(NA, 100)
for (i in 1:100){
  end <- i + 99
  djiaXgspc[i] <- cor(x = residuals.djia[i:end],
                          y = residuals.gspc[i:end])
}
djiaXgspc <- c(rep(NA, 100), djiaXgspc, rep(NA, 101))
plot(djiaXgspc, type = 'l')

gspc.tau <- djiaXgspc[complete.cases(djiaXgspc)]
m <- cbind(gspc.tau, 1:100)
(gspc.kendall = cor(m, method = 'kendall', use = 'pairwise')[2,1])


djiaXn225 <- rep(NA, 100)
for (i in 1:100){
  end <- i + 99
  djiaXn225[i] <- cor(x = residuals.djia[i:end],
                        y = residuals.n225[i:end])
}
djiaXn225 <- c(rep(NA, 100), djiaXn225, rep(NA, 101))
plot(djiaXn225, type = 'l')

n225.tau <- djiaXn225[complete.cases(djiaXn225)]
m <- cbind(n225.tau, 1:100)
(n225.kendall = cor(m, method = 'kendall', use = 'pairwise')[2,1])


djiaXftse <- rep(NA, 100)
for (i in 1:100){
  end <- i + 99
  djiaXftse[i] <- cor(x = residuals.djia[i:end],
                        y = residuals.ftse[i:end])
}
djiaXftse <- c(rep(NA, 100), djiaXftse, rep(NA, 101))
plot(djiaXftse, type = 'l')

ftse.tau <- djiaXftse[complete.cases(djiaXftse)]
m <- cbind(ftse.tau, 1:100)
(ftse.kendall = cor(m, method = 'kendall', use = 'pairwise')[2,1])


djiaXger <- rep(NA, 100)
for (i in 1:100){
  end <- i + 99
  djiaXger[i] <- cor(x = residuals.djia[i:end],
                       y = residuals.ger[i:end])
}
djiaXger <- c(rep(NA, 100), djiaXger, rep(NA, 101))
plot(djiaXger, type = 'l')

ger.tau <- djiaXger[complete.cases(djiaXger)]
m <- cbind(ger.tau, 1:100)
(ger.kendall = cor(m, method = 'kendall', use = 'pairwise')[2,1])



