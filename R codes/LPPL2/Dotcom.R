library(quantmod)
library(lubridate)
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
library(tidyverse)
library(tseries)

setwd("C:/Data/Skola/Bachelor's Thesis/R Codes/LPPL2")


getSymbols('^IXIC', env = .GlobalEnv, from = '1995-01-01', to = '2001-06-30', periodicity = 'daily')
data <- IXIC$IXIC.Adjusted
plot(data)
head(data)

end <- which(time(data) == '2000-03-10') + 101
data <- data[1:end,]
data$IXIC.Adjusted <- log(data$IXIC.Adjusted)
nrow(data)
plot(data)

data$time <- decimal_date(time(data))
head(data)
tail(data)
data1 <- data

(data <- as.data.frame(data))
rownames(data) <- NULL
head(data)


head(data)
end2 <- which.max(data$IXIC.Adjusted) - 50 
data <- data[1:end2,]
tail(data)
write.csv(data, 'Dotcom.csv')

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
t_c <- 2000.308
m <- 0.373
omega <- 9.504


data$B <- (t_c - data$t)^m
head(data)

data$C1 <- ((t_c - data$t)^m) * cos(omega * log(abs(t_c - data$t)))
data$C2 <- ((t_c - data$t)^m) * sin(omega * log(abs(t_c - data$t)))
head(data)

lmodel <- lm(data$price ~ data$B + data$C1 + data$C2)
summary(lmodel)

##### Predicting
data1 <- log(IXIC$IXIC.Adjusted)
end <- which(time(data1) == '2000-03-10') + 101
data1 <- data1[1:end,]
data1$time <- decimal_date(time(data1))
head(data1)

data1$B <- abs((t_c - data1$time))^m
data1$C1 <- (abs((t_c - data1$time))^m) * cos(omega * log(abs(t_c - data1$time)))
data1$C2 <- (abs((t_c - data1$time))^m) * sin(omega * log(abs(t_c - data1$time)))

y_pred <- lmodel$coefficients[1] + lmodel$coefficients[2] * data1$B + lmodel$coefficients[3] * data1$C1 + lmodel$coefficients[4] * data1$C2
y_pred0 <- lmodel$coefficients[1] + lmodel$coefficients[2] * data$B + lmodel$coefficients[3] * data$C1 + lmodel$coefficients[4] * data$C2

plot(as.numeric(data1$IXIC.Adjusted), type = 'l', ylim = c(6.5,8.8))
lines(as.numeric(y_pred), col = 'blue')
abline(v = which.max(y_pred$B), col = 'red')


##### Plotting
p2 <- ggplot(data = NULL, mapping = aes(x = decimal_date(time(data1)), y = data1$IXIC.Adjusted)) +
  geom_line(size = 0.8) +
  geom_line(mapping = aes(x = decimal_date(time(data1)), y = y_pred$B), size = 0.8, col = 'blue') +
  geom_vline(xintercept = t_c , linetype = 'dashed', size = 0.8, col = 'red') +
  geom_vline(xintercept = decimal_date(ymd('2000-03-10')), linetype = 'dashed', size = 0.7, col = 'black') +
  geom_vline(xintercept = 1999.992, linetype = 'dotted', size = 0.8, col = 'black') +
  theme_bw() +
  ylab("") +
  xlab('Time (years)') +
  scale_y_continuous(limits = c(6.5, 8.88), expand = c(0,0)) +
  scale_x_continuous(limits = c(min(decimal_date(time(data1))),
                                max(decimal_date(time(data1)))), expand = c(0,0)) +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 10),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 9, angle = 45),
        axis.title.x = element_text(face = 'bold', colour = 'black', size = 14),
        plot.margin = margin(0, 0, 0, 0, 'cm')) +
  geom_label(aes(x = 1995.172, y = 8.83), fontface = 2,
             label = 'NASDAQ', fill = 'grey')

p2

png('LPPL2_Dotcom.png', width = 900, height = 300)
p2
dev.off()
