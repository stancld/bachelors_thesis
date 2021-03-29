library(quantmod)
library(lubridate)
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
library(tidyverse)
library(tseries)
library(lomb)

getSymbols("BTC-USD", env = .GlobalEnv, from = '2015-08-21', to = '2018-02-28', periodicity = 'daily')
data <- log(`BTC-USD`[,6])
plot(data)



##### Plotting
data$time <- decimal_date(time(data))
head(data)

data$B <- abs((t_c - data$time))^m
data$C1 <- (abs((t_c - data$time))^m) * cos(omega * log(abs(t_c - data$time)))
data$C2 <- (abs((t_c - data$time))^m) * sin(omega * log(abs(t_c - data$time)))
head(data)

y_pred <- lmodel$coefficients[1] + lmodel$coefficients[2] * data$B + lmodel$coefficients[3] * data$C1 + lmodel$coefficients[4] * data$C2
plot(y_pred$B)

data2 <- data[,1:2]
data2$B <- abs((t_c - data2$time))^m
data2$C1 <- (abs((t_c - data2$time))^m) * cos(omega * log(abs(t_c - data2$time)))
data2$C2 <- (abs((t_c - data2$time))^m) * sin(omega * log(abs(t_c - data2$time)))
head(data2)

y_pred1 <- lmodel$coefficients[1] + lmodel$coefficients[2] * data2$B + lmodel$coefficients[3] * data2$C1 + lmodel$coefficients[4] * data2$C2




p2 <- ggplot(data = NULL) +
  geom_line(mapping = aes(x = decimal_date(time(data)), y = data$BTC.USD.Adjusted), size = 0.8) +
  geom_line(mapping = aes(x = decimal_date(time(data)), y = y_pred$B), size = 0.8, col = "#56B4E9") +
  geom_line(mapping = aes(x = decimal_date(time(data)), y = y_pred1$B), size = 0.8, col = 'darkgreen') +
  geom_vline(xintercept = decimal_date(time(data[which.max(data$BTC.USD.Adjusted),])), linetype = 'dotted', size = 0.8, col = 'red') +
  geom_vline(xintercept = 2017.926 , linetype = 'dashed', size = 0.8, col = 'darkgreen') +
  geom_vline(xintercept = 2018.078 , linetype = 'dashed', size = 0.8, col = "#56B4E9") +
  geom_vline(xintercept = decimal_date(ymd('2017-11-15')), linetype = 'dashed', size = 0.7, col = "#56B4E9") +
  geom_vline(xintercept = decimal_date(ymd('2017-10-15')), linetype = 'dashed', size = 0.7, col = 'darkgreen') +
  theme_gray() +
  ylab("") +
  xlab('Time (years)') +
  scale_y_continuous(limits = c(5.0, 10.1), expand = c(0,0)) +
  scale_x_continuous(limits = c(min(decimal_date(time(data))),
                                max(decimal_date(time(data)))), expand = c(0,0)) +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 11),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 11),
        axis.title.x = element_text(face = 'bold', colour = 'black', size = 14),
        plot.margin = margin(0, 0, 0, 0, 'cm')) +
  geom_label(aes(x = 2015.709, y = 10.01), fontface = 2,
             label = 'BTC-USD', fill = 'grey')

p2

png('LPPL2_BTC_1.png', width = 900, height = 375)
p2
dev.off()


##### Period 1!
end1 <- 362 ## 2005-03-15
(data1 <- data[1:end1,])
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'BTC1.csv')

##### Training
(ABC <- read.csv('BTC1_results.csv'))
(ABC <- ABC[,5:7])
FITS <- rep(NA, nrow(ABC))

for (i in 1:length(FITS)){
  
  t_c <- ABC[i,1]
  m <- ABC[i,2]
  omega <- ABC[i,3]
  
  data1$B <- (t_c - data1$t)^m
  
  data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
  data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))
  
  lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
  FITS[i] <- as.numeric(summary(lmodel)[8])
}
print(FITS)

(best <- which.max(FITS))

### The best fit
t_c <- ABC[best,1]
m <- ABC[best,2]
omega <- ABC[best,3]

data1$B <- (t_c - data1$t)^m

data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))

lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
summary(lmodel)




##### Period 2!
end1 <- 393 ## 2005-03-15
(data1 <- data[1:end1,])
(data1 <- (data1))
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'BTC2.csv')

##### Training
(ABC <- read.csv('BTC2_results.csv'))
(ABC <- ABC[,5:7])
FITS <- rep(NA, nrow(ABC))

for (i in 1:length(FITS)){
  
  t_c <- ABC[i,1]
  m <- ABC[i,2]
  omega <- ABC[i,3]
  
  data1$B <- (t_c - data1$t)^m
  
  data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
  data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))
  
  lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
  FITS[i] <- as.numeric(summary(lmodel)[8])
}
print(FITS)

(best <- which.max(FITS))

### The best fit
t_c <- ABC[best,1]
m <- ABC[best,2]
omega <- ABC[best,3]

data1$B <- (t_c - data1$t)^m

data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))

lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
summary(lmodel)




##### Period 3!
end1 <- 423 ## 2005-03-15
(data1 <- data[1:end1,])
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'BTC3.csv')

##### Training
(ABC <- read.csv('BTC3_results.csv'))
(ABC <- ABC[,5:7])
FITS <- rep(NA, nrow(ABC))

for (i in 1:length(FITS)){
  
  t_c <- ABC[i,1]
  m <- ABC[i,2]
  omega <- ABC[i,3]
  
  data1$B <- (t_c - data1$t)^m
  
  data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
  data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))
  
  lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
  FITS[i] <- as.numeric(summary(lmodel)[8])
}
print(FITS)

(best <- which.max(FITS))

### The best fit
t_c <- ABC[best,1]
m <- ABC[best,2]
omega <- ABC[best,3]

data1$B <- (t_c - data1$t)^m

data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))

lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
summary(lmodel)




##### Period 4!
end1 <- 453 ## 2005-03-15
(data1 <- data[1:end1,])
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'BTC4.csv')

##### Training
(ABC <- read.csv('BTC4_results.csv'))
(ABC <- ABC[,5:7])
FITS <- rep(NA, nrow(ABC))

for (i in 1:length(FITS)){
  
  t_c <- ABC[i,1]
  m <- ABC[i,2]
  omega <- ABC[i,3]
  
  data1$B <- (t_c - data1$t)^m
  
  data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
  data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))
  
  lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
  FITS[i] <- as.numeric(summary(lmodel)[8])
}
print(FITS)

(best <- which.max(FITS))

### The best fit
t_c <- ABC[best,1]
m <- ABC[best,2]
omega <- ABC[best,3]

data1$B <- (t_c - data1$t)^m

data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))

lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
summary(lmodel)


##### Period 5!
end1 <- 483 ## 2005-03-15
(data1 <- data[1:end1,])
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'BTC5.csv')

##### Training
(ABC <- read.csv('BTC5_results.csv'))
(ABC <- ABC[,5:7])
FITS <- rep(NA, nrow(ABC))

for (i in 1:length(FITS)){
  
  t_c <- ABC[i,1]
  m <- ABC[i,2]
  omega <- ABC[i,3]
  
  data1$B <- (t_c - data1$t)^m
  
  data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
  data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))
  
  lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
  FITS[i] <- as.numeric(summary(lmodel)[8])
}
print(FITS)

(best <- which.max(FITS))

### The best fit
t_c <- ABC[best,1]
m <- ABC[best,2]
omega <- ABC[best,3]

data1$B <- (t_c - data1$t)^m

data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))

lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
summary(lmodel)



##### Period 6!
end1 <- 514 ## 2005-03-15
(data1 <- data[1:end1,])
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'BTC6.csv')

##### Training
(ABC <- read.csv('BTC6_results.csv'))
(ABC <- ABC[,5:7])
FITS <- rep(NA, nrow(ABC))

for (i in 1:length(FITS)){
  
  t_c <- ABC[i,1]
  m <- ABC[i,2]
  omega <- ABC[i,3]
  
  data1$B <- (t_c - data1$t)^m
  
  data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
  data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))
  
  lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
  FITS[i] <- as.numeric(summary(lmodel)[8])
}
print(FITS)

(best <- which.max(FITS))

### The best fit
t_c <- ABC[best,1]
m <- ABC[best,2]
omega <- ABC[best,3]

data1$B <- (t_c - data1$t)^m

data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))

lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
summary(lmodel)






##### Period 7!
end1 <- 545 ## 2005-03-15
(data1 <- data[1:end1,])
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'BTC7.csv')

##### Training
(ABC <- read.csv('BTC7_results.csv'))
(ABC <- ABC[,5:7])
FITS <- rep(NA, nrow(ABC))

for (i in 1:length(FITS)){
  
  t_c <- ABC[i,1]
  m <- ABC[i,2]
  omega <- ABC[i,3]
  
  data1$B <- (t_c - data1$t)^m
  
  data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
  data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))
  
  lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
  FITS[i] <- as.numeric(summary(lmodel)[8])
}
print(FITS)

(best <- which.max(FITS))

### The best fit
t_c <- ABC[best,1]
m <- ABC[best,2]
omega <- ABC[best,3]

data1$B <- (t_c - data1$t)^m

data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))

lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
summary(lmodel)





##### Period 8!
end1 <- 573 ## 2005-03-15
(data1 <- data[1:end1,])
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'BTC8.csv')

##### Training
(ABC <- read.csv('BTC8_results.csv'))
(ABC <- ABC[,5:7])
FITS <- rep(NA, nrow(ABC))

for (i in 1:length(FITS)){
  
  t_c <- ABC[i,1]
  m <- ABC[i,2]
  omega <- ABC[i,3]
  
  data1$B <- (t_c - data1$t)^m
  
  data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
  data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))
  
  lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
  FITS[i] <- as.numeric(summary(lmodel)[8])
}
print(FITS)

(best <- which.max(FITS))

### The best fit
t_c <- ABC[best,1]
m <- ABC[best,2]
omega <- ABC[best,3]

data1$B <- (t_c - data1$t)^m

data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))

lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
summary(lmodel)




##### Period 9!
end1 <- 605 ## 2005-03-15
(data1 <- data[1:end1,])
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'BTC9.csv')

##### Training
(ABC <- read.csv('BTC9_results.csv'))
(ABC <- ABC[,5:7])
FITS <- rep(NA, nrow(ABC))

for (i in 1:length(FITS)){
  
  t_c <- ABC[i,1]
  m <- ABC[i,2]
  omega <- ABC[i,3]
  
  data1$B <- (t_c - data1$t)^m
  
  data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
  data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))
  
  lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
  FITS[i] <- as.numeric(summary(lmodel)[8])
}
print(FITS)

(best <- which.max(FITS))

### The best fit
t_c <- ABC[best,1]
m <- ABC[best,2]
omega <- ABC[best,3]

data1$B <- (t_c - data1$t)^m

data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))

lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
summary(lmodel)





##### Period 10!
end1 <- 635 ## 2005-03-15
(data1 <- data[1:end1,])
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'BTC10.csv')

##### Training
(ABC <- read.csv('BTC10_results.csv'))
(ABC <- ABC[,5:7])
FITS <- rep(NA, nrow(ABC))

for (i in 1:length(FITS)){
  
  t_c <- ABC[i,1]
  m <- ABC[i,2]
  omega <- ABC[i,3]
  
  data1$B <- (t_c - data1$t)^m
  
  data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
  data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))
  
  lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
  FITS[i] <- as.numeric(summary(lmodel)[8])
}
print(FITS)

(best <- which.max(FITS))

### The best fit
t_c <- ABC[best,1]
m <- ABC[best,2]
omega <- ABC[best,3]

data1$B <- (t_c - data1$t)^m

data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))

lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
summary(lmodel)





##### Period 11!
end1 <- 666 ## 2005-03-15
(data1 <- data[1:end1,])
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'BTC11.csv')

##### Training
(ABC <- read.csv('BTC11_results.csv'))
(ABC <- ABC[,5:7])
FITS <- rep(NA, nrow(ABC))

for (i in 1:length(FITS)){
  
  t_c <- ABC[i,1]
  m <- ABC[i,2]
  omega <- ABC[i,3]
  
  data1$B <- (t_c - data1$t)^m
  
  data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
  data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))
  
  lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
  FITS[i] <- as.numeric(summary(lmodel)[8])
}
print(FITS)

(best <- which.max(FITS))

### The best fit
t_c <- ABC[best,1]
m <- ABC[best,2]
omega <- ABC[best,3]

data1$B <- (t_c - data1$t)^m

data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))

lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
summary(lmodel)






##### Period 12!
end1 <- 696 ## 2005-03-15
(data1 <- data[1:end1,])
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'BTC12.csv')

##### Training
(ABC <- read.csv('BTC12_results.csv'))
(ABC <- ABC[,5:7])
FITS <- rep(NA, nrow(ABC))

for (i in 1:length(FITS)){
  
  t_c <- ABC[i,1]
  m <- ABC[i,2]
  omega <- ABC[i,3]
  
  data1$B <- (t_c - data1$t)^m
  
  data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
  data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))
  
  lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
  FITS[i] <- as.numeric(summary(lmodel)[8])
}
print(FITS)

(best <- which.max(FITS))

### The best fit
t_c <- ABC[best,1]
m <- ABC[best,2]
omega <- ABC[best,3]

data1$B <- (t_c - data1$t)^m

data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))

lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
summary(lmodel)






##### Period 13!
end1 <- 727 ## 2005-03-15
(data1 <- data[1:end1,])
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'BTC13.csv')

##### Training
(ABC <- read.csv('BTC13_results.csv'))
(ABC <- ABC[,5:7])
FITS <- rep(NA, nrow(ABC))

for (i in 1:length(FITS)){
  
  t_c <- ABC[i,1]
  m <- ABC[i,2]
  omega <- ABC[i,3]
  
  data1$B <- (t_c - data1$t)^m
  
  data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
  data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))
  
  lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
  FITS[i] <- as.numeric(summary(lmodel)[8])
}
print(FITS)

(best <- which.max(FITS))

### The best fit
t_c <- ABC[best,1]
m <- ABC[best,2]
omega <- ABC[best,3]

data1$B <- (t_c - data1$t)^m

data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))

lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
summary(lmodel)






##### Period 14!
end1 <- 758 ## 2005-03-15
(data1 <- data[1:end1,])
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'BTC14.csv')

##### Training
(ABC <- read.csv('BTC14_results.csv'))
(ABC <- ABC[,5:7])
FITS <- rep(NA, nrow(ABC))

for (i in 1:length(FITS)){
  
  t_c <- ABC[i,1]
  m <- ABC[i,2]
  omega <- ABC[i,3]
  
  data1$B <- (t_c - data1$t)^m
  
  data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
  data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))
  
  lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
  FITS[i] <- as.numeric(summary(lmodel)[8])
}
print(FITS)

(best <- which.max(FITS))

### The best fit
t_c <- ABC[best,1]
m <- ABC[best,2]
omega <- ABC[best,3]

data1$B <- (t_c - data1$t)^m

data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))

lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
summary(lmodel)



##### Period 15!
end1 <- 788 ## 2005-03-15
(data1 <- data[1:end1,1])
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'BTC15.csv')

##### Training
(ABC <- read.csv('BTC15_results.csv'))
(ABC <- ABC[,5:7])
FITS <- rep(NA, nrow(ABC))

for (i in 1:length(FITS)){
  
  t_c <- ABC[i,1]
  m <- ABC[i,2]
  omega <- ABC[i,3]
  
  data1$B <- (t_c - data1$t)^m
  
  data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
  data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))
  
  lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
  FITS[i] <- as.numeric(summary(lmodel)[8])
}
print(FITS)

(best <- which.max(FITS))

### The best fit
t_c <- ABC[best,1]
m <- ABC[best,2]
omega <- ABC[best,3]

data1$B <- (t_c - data1$t)^m

data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))

lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
summary(lmodel)





##### FINALE
end1 <- 818 ## 2005-03-15
(data1 <- data[1:end1,])
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'BTC16.csv')

##### Training
(ABC <- read.csv('BTC16_results.csv'))
(ABC <- ABC[,5:7])
FITS <- rep(NA, nrow(ABC))

for (i in 1:length(FITS)){
  
  t_c <- ABC[i,1]
  m <- ABC[i,2]
  omega <- ABC[i,3]
  
  data1$B <- (t_c - data1$t)^m
  
  data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
  data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))
  
  lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
  FITS[i] <- as.numeric(summary(lmodel)[8])
}
print(FITS)

(best <- which.max(FITS))

### The best fit
t_c <- ABC[best,1]
m <- ABC[best,2]
omega <- ABC[best,3]

data1$B <- (t_c - data1$t)^m

data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))

lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
summary(lmodel)


