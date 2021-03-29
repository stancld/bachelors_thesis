library(quantmod)
library(lubridate)
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
library(tidyverse)
library(tseries)

setwd("C:/Data/Skola/Bachelor's Thesis/R Codes/LPPL2")


getSymbols('^MXX', env = .GlobalEnv, from = '1993-06-10', to = '1995-06-24', periodicity = 'daily')
data <- MXX$MXX.Adjusted
plot(data)

end <- which.max(MXX$MXX.Adjusted) + 101
data <- data[1:end,]
head(data)
for (i in 1:end){
  if (is.na(data[i,1]) == T){
    data[i,1] <- data[i-1,1]
  }
}

data$MXX.Adjusted <- log(data$MXX.Adjusted)
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
end2 <- which.max(MXX$MXX.Adjusted) - 35
data <- data[1:end2,]
#write.csv(data, 'PesoCrisis.csv')

plot(data$MXX.Adjusted,type = 'l')
head(data)

t_c = 1994.1798
m = 0.2531
omega = 8.206


##### Trainig
data$B <- (t_c - data$time)^m
head(data)

data$C1 <- ((t_c - data$time)^m) * cos(omega * log(abs(t_c - data$time)))
data$C2 <- ((t_c - data$time)^m) * sin(omega * log(abs(t_c - data$time)))
head(data)

lmodel <- lm(data$MXX.Adjusted ~ data$B + data$C1 + data$C2)
summary(lmodel)

##### Predicting
data1$B <- abs((t_c - data1$time))^m
data1$C1 <- (abs((t_c - data1$time))^m) * cos(omega * log(abs(t_c - data1$time)))
data1$C2 <- (abs((t_c - data1$time))^m) * sin(omega * log(abs(t_c - data1$time)))

y_pred <- lmodel$coefficients[1] + lmodel$coefficients[2] * data1$B + lmodel$coefficients[3] * data1$C1 + lmodel$coefficients[4] * data1$C2
y_pred0 <- lmodel$coefficients[1] + lmodel$coefficients[2] * data$B + lmodel$coefficients[3] * data$C1 + lmodel$coefficients[4] * data$C2

plot(as.numeric(data1$MXX.Adjusted), type = 'l', ylim = c(7.2,8.4))
lines(as.numeric(y_pred), col = 'blue')
abline(v = which.max(y_pred$B), col = 'red')

residuals <- (data$MXX.Adjusted - y_pred0)
plot(residuals)


adf.test(residuals, k = 1)
kpss.test(residuals)
pp.test(residuals)


##### Plotting
p2 <- ggplot(data = NULL, mapping = aes(x = decimal_date(time(data1)), y = data1$MXX.Adjusted)) +
  geom_line(size = 0.8) +
  geom_line(mapping = aes(x = decimal_date(time(data1)), y = y_pred$B), size = 0.8, col = 'blue') +
  geom_vline(xintercept = t_c , linetype = 'dashed', size = 0.8, col = 'red') +
  geom_vline(xintercept = decimal_date(time(data1[which.max(data1$MXX.Adjusted)])), linetype = 'dashed', size = 0.7, col = 'black') +
  geom_vline(xintercept = 1993.97, linetype = 'dotted', size = 0.8, col = 'black') +
  ylab("") +
  xlab('Time (years)') +
  scale_y_continuous(limits = c(7.25, 8.53), expand = c(0,0)) +
  scale_x_continuous(limits = c(min(decimal_date(time(data1))),
                                max(decimal_date(time(data1)))), expand = c(0,0)) +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 10),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 9, angle = 45),
        axis.title.x = element_text(face = 'bold', colour = 'black', size = 14),
        plot.margin = margin(0, 0, 0, 0, 'cm')) +
  geom_label(aes(x = 1993.455, y = 8.503), fontface = 2,
             label = 'MXX', fill = 'grey')

p2

png('LPPL2_PesoCrisis.png', width = 900, height = 300)
p2
dev.off()
