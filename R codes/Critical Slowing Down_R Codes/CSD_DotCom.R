library(quantmod)
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
library(tidyverse)

setwd("C:/Data/Skola/Bachelor's Thesis/R Codes")

#####
# Loading and handling data
getSymbols('^IXIC', env = .GlobalEnv, from = '1999-01-11', to = '2002-05-24', periodicity = 'daily')
data <- IXIC$IXIC.Adjusted
head(data)

start <- which(time(data) == '2000-03-10') - 200
end <- which(time(data) == '2000-03-10') + 100

data <- data[start:end,]
length(data)

for (i in 1:301){
  if (is.na(data[i,1]) == T){
    data[i,1] <- data[i-1,1]
  }
}

data <- log(data)
plot(data, type = 'l')
#####
# Smoothing
MA <- rep(NA, end - start + 1)
time_frame <- 1:200
gaussian_kernel <- function(sigma, r, t){
  (1 / (sqrt(2*pi) * sigma)) * exp(-((r-t)^2) / (2 * sigma^2))
}


for (t in time_frame){
  MA[t] <- sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t) * data[x])) /
    sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t)))
}

residuals <- rep(NA, end - start + 1)
residuals[1:200] <- data[1:200] - MA[1:200]
plot(residuals)

#####
# AR(1)
residuals.ar <- residuals[1:200]
AR <- rep(NA, 100)

for (i in 1:200){
  AR[i] <- summary(lm(residuals.ar[(i+1):(i+99)] ~ residuals.ar[i:(i+98)]))$coefficients[2,1]
}

AR <- c(rep(NA, 100), AR, rep(NA, 101))
plot(AR)
#####
# variance
variance <- rep(NA, 100)
for (i in 1:200){
  start <- i
  end <- i + 99
  variance[i] <- var(residuals[start:end])
}

variance <- c(rep(NA, 100), variance, rep(NA, 101))
plot(variance)

#####
# Kendall's tau
AR.tau <- AR[complete.cases(AR)]
m <- cbind(AR.tau[51:200], 151:200)
(AR.kendall = cor(m, method = 'kendall', use = 'pairwise')[2,1])

VAR.tau <- variance[complete.cases(variance)]
w <- cbind(VAR.tau[41:200], 141:200)
(VAR.kendall = cor(w, method = 'kendall', use = 'pairwise')[2,1])

# signifiance
N = 100 * 99 / 2
sigmic = sqrt((2*(2*N + 5)) / (9*N*(N-1)))

(Z.AR = AR.kendall / sigmic)
(Z.VAR = VAR.kendall / sigmic)

#####
# Plotting
##### P1, price------
p1 <- ggplot(data = NULL, mapping = aes(x = -200:100, y = data)) +
  geom_line(size = 0.8, colour = 'black') +
  geom_line(aes(x = -200:100, y = MA), size = 0.8, colour = 'red', alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = 'dashed', size = 0.7) + 
  geom_vline(xintercept = -100, linetype = 'dotted', size = 0.7) +
  geom_segment(aes(x = -200, y = 8.25, xend = -100, yend = 8.25), arrow = arrow(length = unit(0.2, 'cm'))) +
  geom_segment(aes(x = -100, y = 8.25, xend = -200, yend = 8.25), arrow = arrow(length = unit(0.2, 'cm'))) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 10),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 9, angle = 45),
        plot.title = element_text(face = 'bold', hjust = 0.5, size = 16)) +
  scale_x_discrete(limits = c(-200, -1100, -100, -100, 0, 100, 100)) +
  annotate("text", label = "Sliding window", x = -1100, y = 8.32,
           fontface = 2, size = 3) +
  geom_label(aes(x = 91.5, y = 8.526, fontface = 2),
             label = 'NASDAQ', fill = 'grey')

p1

##### P2, residuals------
p2 <- ggplot(data = NULL, mapping = aes(x = -200:100, y = residuals)) +
  geom_segment(mapping = aes(xend = -200:100 ,yend = 0), size = 0.8) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 10),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 9, angle = 45)) +
  scale_x_discrete(limits = c(-200, -1100, -100, -100, 0, 100, 100)) +
  geom_label(aes(x = 90.5, y = 0.08, fontface = 2),
             label = 'Residuals', fill = 'grey')

p2

##### P3, AR(1)------
p3 <- ggplot(data = NULL, mapping = aes(x = -200:100, y = AR)) +
  geom_line(size = 0.8) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 10),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 9, angle = 45)) +
  scale_x_discrete(limits = c(-200, -1100, -100, -100, 0, 100, 100)) +
  geom_label(aes(x = 95.3, y = 0.85, fontface = 2),
             label = 'AR(1)', fill = 'grey') +
  geom_label(aes(x = - 170, y = 0.744),
             label = paste0("Kendall's tau = ", round(AR.kendall,3), ' (p-val = 2e-16)'),
             size = 3)

p3


##### P4, var------
p4 <- ggplot(data = NULL, mapping = aes(x = -200:100, y = sqrt(variance))) +
  geom_line(size = 0.8) +
  xlab("Time (days)") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 10),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 9, angle = 45),
        axis.title.x = element_text(face = 'bold', colour = 'black', size = 14)) +
  scale_x_discrete(limits = c(-200, -1100, -100, -100, 0, 100, 100)) +
  geom_label(aes(x = 92, y = 0.0281, fontface = 2),
             label = 'Std. dev', fill = 'grey') +
  geom_label(aes(x = - 170, y = 0.0231),
             label = paste0("Kendall's tau = ", round(VAR.kendall,3), ' (p-val = 2e-16)'),
             size = 3)

p4

##### Multiplot
#
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g4 <- ggplotGrob(p4)
g <- rbind(g1, g2, g3, g4, size = 'first')
g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths, g4$widths)
grid.newpage()
grid.draw(g)

png('CSD_Dotcom.png', width = 900, height = 600)
grid.newpage()
grid.draw(g)
dev.off()






###### Correlation with other markets
### Data mining
getSymbols(c("^IXIC", "^GSPC","^N225", "^FTSE", '^MXX', '^GDAXI'), env=.GlobalEnv, from = '1999-01-11', to = '2002-05-24', periodicity = 'daily')

nasdaq <- IXIC$IXIC.Adjusted
gspc <- GSPC$GSPC.Adjusted
nikkei <- N225$N225.Adjusted
ftse <- FTSE$FTSE.Adjusted
mex <- MXX$MXX.Adjusted
german <- GDAXI$GDAXI.Adjusted


start <- which(time(data) == '2000-03-10') - 200
end <- which(time(data) == '2000-03-10') + 100

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
# Nasdaq
time_frame <- 1:200
MA.nasdaq <- rep(NA, 100)

for (t in time_frame){
  MA.nasdaq[t] <- sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t) * DATA[x,1])) /
    sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t)))
}

residuals.nasdaq <- DATA[1:200,1] - MA.nasdaq[1:200]
plot(residuals.nasdaq)



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
MA.n225 <- rep(NA, end - start + 1)
time_frame <- 1:200

for (t in time_frame){
  MA.n225[t] <- sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t) * DATA[x,4])) /
    sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t)))
}

residuals.n225 <- DATA[1:200,4] - MA.n225[1:200]
plot(residuals.n225)


# FTSE
MA.ftse <- rep(NA, end - start + 1)
time_frame <- 1:200

for (t in time_frame){
  MA.ftse[t] <- sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t) * DATA[x,5])) /
    sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t)))
}

residuals.ftse <- DATA[1:200,5] - MA.ftse[1:200]
plot(residuals.ftse)

# DAX
MA.ger <- rep(NA, end - start + 1)
time_frame <- 1:200

for (t in time_frame){
  MA.ger[t] <- sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t) * DATA[x,6])) /
    sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t)))
}

residuals.ger <- DATA[1:200,6] - MA.ger[1:200]
plot(residuals.ger)


# 2. computing correlations
nasdaqXmex <- rep(NA, 100)
for (i in 1:100){
  end <- i + 99
  nasdaqXmex[i] <- cor(x = residuals.nasdaq[i:end],
                        y = residuals.mex[i:end])
}
nasdaqXmex <- c(rep(NA, 100), nasdaqXmex, rep(NA, 101))
plot(nasdaqXmex, type = 'l')

mex.tau <- nasdaqXmex[complete.cases(nasdaqXmex)]
m <- cbind(mex.tau, 1:100)
(mex.kendall = cor(m, method = 'kendall', use = 'pairwise')[2,1])


nasdaqXgspc <- rep(NA, 100)
for (i in 1:100){
  end <- i + 99
  nasdaqXgspc[i] <- cor(x = residuals.nasdaq[i:end],
                     y = residuals.gspc[i:end])
}
nasdaqXgspc <- c(rep(NA, 100), nasdaqXgspc, rep(NA, 101))
plot(nasdaqXgspc, type = 'l')

gspc.tau <- nasdaqXgspc[complete.cases(nasdaqXgspc)]
m <- cbind(gspc.tau, 1:100)
(gspc.kendall = cor(m, method = 'kendall', use = 'pairwise')[2,1])


nasdaqXn225 <- rep(NA, 100)
for (i in 1:100){
  end <- i + 99
  nasdaqXn225[i] <- cor(x = residuals.nasdaq[i:end],
                     y = residuals.n225[i:end])
}
nasdaqXn225 <- c(rep(NA, 100), nasdaqXn225, rep(NA, 101))
plot(nasdaqXn225, type = 'l')

n225.tau <- nasdaqXn225[complete.cases(nasdaqXn225)]
m <- cbind(n225.tau, 1:100)
(n225.kendall = cor(m, method = 'kendall', use = 'pairwise')[2,1])


nasdaqXftse <- rep(NA, 100)
for (i in 1:100){
  end <- i + 99
  nasdaqXftse[i] <- cor(x = residuals.nasdaq[i:end],
                     y = residuals.ftse[i:end])
}
nasdaqXftse <- c(rep(NA, 100), nasdaqXftse, rep(NA, 101))
plot(nasdaqXftse, type = 'l')

ftse.tau <- nasdaqXftse[complete.cases(nasdaqXftse)]
m <- cbind(ftse.tau, 1:100)
(ftse.kendall = cor(m, method = 'kendall', use = 'pairwise')[2,1])


nasdaqXger <- rep(NA, 100)
for (i in 1:100){
  end <- i + 99
  nasdaqXger[i] <- cor(x = residuals.nasdaq[i:end],
                    y = residuals.ger[i:end])
}
nasdaqXger <- c(rep(NA, 100), nasdaqXger, rep(NA, 101))
plot(nasdaqXger, type = 'l')

ger.tau <- nasdaqXger[complete.cases(nasdaqXger)]
m <- cbind(ger.tau, 1:100)
(ger.kendall = cor(m, method = 'kendall', use = 'pairwise')[2,1])


