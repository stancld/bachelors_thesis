library(quantmod)
library(lubridate)
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
library(tidyverse)
library(tseries)

setwd("C:/Data/Skola/Bachelor's Thesis/R Codes/LPPL2/Out-of-sample predicitions/")

getSymbols('^DJI', env = .GlobalEnv, from = '2003-01-01', to = '2009-07-01', periodicity = 'daily')
data <- log(DJI$DJI.Adjusted)
data <- data[1:which.max(data)+100]
plot(data)

p2 <- ggplot(data = NULL) +
  geom_rect(mapping = aes(xmin = decimal_date(ymd('2003-3-15')), ymin = 9.1,
                          xmax = decimal_date(ymd('2006-9-15')), ymax = 9.65),
            fill = 'lightblue', alpha = 0.55) +
  geom_rect(mapping = aes(xmin = decimal_date(ymd('2005-10-25')), ymin = 9.1,
                          xmax = decimal_date(ymd('2007-9-17')), ymax = 9.65),
            fill = 'green', alpha = 0.55) +
  geom_line(mapping = aes(x = decimal_date(time(data)), y = data$DJI.Adjusted), size = 0.8) +
  geom_vline(xintercept = decimal_date(time(data[which.max(data)])), linetype = 'dashed', size = 0.8, col = 'red') +
  theme_gray() +
  ylab("") +
  xlab('Time (years)') +
  scale_y_continuous(limits = c(9.1, 9.65), expand = c(0,0)) +
  scale_x_continuous(limits = c(min(decimal_date(time(data))),
                                max(decimal_date(time(data)))), expand = c(0,0)) +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 11),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 11),
        axis.title.x = element_text(face = 'bold', colour = 'black', size = 14),
        plot.margin = margin(0, 0, 0, 0, 'cm')) +
  geom_label(aes(x = 2003.5, y = 9,6), fontface = 2,
             label = 'DJIA', fill = 'grey')

p2

png('LPPL2_DJIA.png', width = 900, height = 300)
p2
dev.off()




##### Period 1!
end1 <- 504 ## 2005-03-15
(data1 <- data[1:end1,])
(data1 <- log(data1))
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'DJIA1.csv')

##### Training
(ABC <- read.csv('DJIA1_results.csv'))
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
end2 <- 568 ## 2005-06-15
(data1 <- data[1:end2,])
(data1 <- log(data1))
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'DJIA2.csv')

##### Training
(ABC <- read.csv('DJIA2_results.csv'))
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
end3 <- 632 ## 2005-09-15
(data1 <- data[1:end3,])
(data1 <- log(data1))
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'DJIA3.csv')



##### Period 4!
end3 <- 696 ## 2005-12-15
(data1 <- data[1:end3,])
(data1 <- log(data1))
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'DJIA4.csv')

##### Period 5!
end5<- 756 ## 2006-03-15
(data1 <- data[1:end5,])
(data1 <- log(data1))
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'DJIA5.csv')

##### Period 6!
end5<- 820 ## 2006-06-15
(data1 <- data[1:end5,])
(data1 <- log(data1))
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'DJIA6.csv')

##### Training
(ABC <- read.csv('DJIA6_results.csv'))
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
end5<- 884 ## 2006-09-15
(data1 <- data[1:end5,])
(data1 <- log(data1))
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'DJIA7.csv')

data1$B <- (t_c - data1$t)^m

data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))

lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
summary(lmodel)



##### Period 8!
start5 <- 660
end5 <- 948

(data1 <- data[start5:end5,])
(data1 <- log(data1))
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'DJIA8.csv')

##### Training
(ABC <- read.csv('DJIA8_results.csv'))
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
start5 <- 660
end5 <- 1007

(data1 <- data[start5:end5,])
(data1 <- log(data1))
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'DJIA9.csv')

##### Training
(ABC <- read.csv('DJIA9_results.csv'))
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
start5 <- 660
end5 <- 1050

(data1 <- data[start5:end5,])
(data1 <- log(data1))
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'DJIA10.csv')

##### Training
(ABC <- read.csv('DJIA10_results.csv'))
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
t_c <- 2007.934
m <- 0.1368
omega <- 10.925

data1$B <- (t_c - data1$t)^m

data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))

lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
summary(lmodel)


##### Period 11!
start5 <- 660
end5 <- 1071

(data1 <- data[start5:end5,])
(data1 <- log(data1))
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'DJIA11.csv')

### The best fit
t_c <- 2008.068
m <- 0.3046
omega <- 8.1486

data1$B <- (t_c - data1$t)^m

data1$C1 <- ((t_c - data1$t)^m) * cos(omega * log(abs(t_c - data1$t)))
data1$C2 <- ((t_c - data1$t)^m) * sin(omega * log(abs(t_c - data1$t)))

lmodel <- lm(data1$price ~ data1$B + data1$C1 + data1$C2)
summary(lmodel)




##### Period 12!
start5 <- 660
end5 <- 1091

(data1 <- data[start5:end5,])
(data1 <- log(data1))
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'DJIA12.csv')

##### Training
(ABC <- read.csv('DJIA12_results.csv'))
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
start5 <- 660
end5 <- 1113

(data1 <- data[start5:end5,])
(data1 <- log(data1))
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'DJIA13.csv')

##### Training
(ABC <- read.csv('DJIA13_results.csv'))
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



##### LAST EDITION!!
start5 <- 660
end5 <- 1134

(data1 <- data[start5:end5,])
(data1 <- log(data1))
data1$t <- decimal_date(time(data1))
colnames(data1) <- c('price', 't')
head(data1)

write.csv(data1, 'DJIA14.csv')

##### Training
(ABC <- read.csv('DJIA14_results.csv'))
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





start5 <- 700
end5 <- 1200

(data1 <- data[start5:end5,])
(data1 <- log(data1))
data1$t <- decimal_date(time(data1))