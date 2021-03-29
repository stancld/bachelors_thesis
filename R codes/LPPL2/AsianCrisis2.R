library(quantmod)
library(lubridate)
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
library(tidyverse)
library(tseries)

setwd("C:/Data/Skola/Bachelor's Thesis/R Codes/LPPL2")


getSymbols('^HSI', env = .GlobalEnv, from = '1995-02-12', to = '1999-05-24', periodicity = 'daily')
data <- HSI$HSI.Adjusted
plot(data)

end <- which(time(data) == '1997-10-17') + 100
data <- data[1:end,]
head(data)
for (i in 1:end){
  if (is.na(data[i,1]) == T){
    data[i,1] <- data[i-1,1]
  }
}

data$HSI.Adjusted <- log(data$HSI.Adjusted)
plot(data)
data1 <- data

nrow(data)
plot(data)

data$time <- decimal_date(time(data))
data1$time <-decimal_date(time(data1))
head(data)

(data <- as.data.frame(data))
rownames(data) <- NULL
head(data)

head(data)
end2 <- which(time(data1) == '1997-10-17') - 40
data <- data[1:end2,]
write.csv(data, 'AsianCrisis.csv')

plot(data$HSI.Adjusted,type = 'l')
head(data)

data <- base_data
(data <- data[,1:2])

(ABC <- read.csv('ABC1.csv'))
ABC <- ABC[,5:7]
FITS <- rep(NA, nrow(ABC))

for (i in 1:length(FITS)){
  
  t_c <- ABC[i,1]
  m <- ABC[i,2]
  omega <- ABC[i,3]
  
  data$B <- (t_c - data$t)^m
  head(data)
  
  data$C1 <- ((t_c - data$t)^m) * cos(omega * log(abs(t_c - data$t)))
  data$C2 <- ((t_c - data$t)^m) * sin(omega * log(abs(t_c - data$t)))
  head(data)
  
  lmodel <- lm(data$price ~ data$B + data$C1 + data$C2)
  FITS[i] <- as.numeric(summary(lmodel)[8])
}
print(FITS)

##### Trainig
best <- 6
(t_c = ABC[best,1])
(m = ABC[best,2])
(omega = ABC[best,3])


data$B <- (t_c - data$time)^m
head(data)

data$C1 <- ((t_c - data$t)^m) * cos(omega * log(abs(t_c - data$t)))
data$C2 <- ((t_c - data$t)^m) * sin(omega * log(abs(t_c - data$t)))
head(data)

lmodel <- lm(data$HSI.Adjusted ~ data$B + data$C1 + data$C2)
summary(lmodel)

##### Extrapolating
end <- which(time(HSI) == '1997-10-17') + 101
data2 <- log(HSI$HSI.Adjusted[1:end])
(data2$time <- decimal_date(time(data2)))
for (i in 1:nrow(data2)){
  if (is.na(data2$HSI.Adjusted[i]) == T){
    data2$HSI.Adjusted[i] <- data2$HSI.Adjusted[i-1]
  }
}
plot(data2$HSI.Adjusted)
head(data2)


data2$B <- abs((t_c - data2$time))^m
data2$C1 <- (abs((t_c - data2$time))^m) * cos(omega * log(abs(t_c - data2$time)))
data2$C2 <- (abs((t_c - data2$time))^m) * sin(omega * log(abs(t_c - data2$time)))

y_pred <- lmodel$coefficients[1] + lmodel$coefficients[2] * data2$B + lmodel$coefficients[3] * data2$C1 + lmodel$coefficients[4] * data2$C2

plot(as.numeric(data2$HSI.Adjusted), type = 'l', ylim = c(9,10))
lines(as.numeric(y_pred), col = 'blue')
abline(v = which.max(y_pred$B), col = 'red')

##### Plotting
p2 <- ggplot(data = NULL, mapping = aes(x = decimal_date(time(data2)), y = data2$HSI.Adjusted)) +
  geom_line(size = 0.8) +
  geom_line(mapping = aes(x = decimal_date(time(data2)), y = y_pred$B), size = 0.8, col = 'blue') +
  geom_vline(xintercept = t_c , linetype = 'dashed', size = 0.8, col = 'red') +
  geom_vline(xintercept = decimal_date(ymd('1997-10-17')), linetype = 'dashed', size = 0.7, col = 'black') +
  geom_vline(xintercept = 1997.6, linetype = 'dotted', size = 0.8, col = 'black') +
  ylab("") +
  xlab('Time (years)') +
  scale_y_continuous(limits = c(8.9, 9.85), expand = c(0,0)) +
  scale_x_continuous(limits = c(min(decimal_date(time(data2))),
                                max(decimal_date(time(data2)))), expand = c(0,0)) +
  theme_gray() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 11),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 11),
        axis.title.x = element_text(face = 'bold', colour = 'black', size = 14),
        plot.margin = margin(0, 0, 0, 0, 'cm')) +
  geom_label(aes(x = 1995.23, y = 9.83), fontface = 2,
             label = 'Hang Seng', fill = 'grey')

p2

png('LPPL2_AsianCrisis.png', width = 900, height = 300)
p2
dev.off()


