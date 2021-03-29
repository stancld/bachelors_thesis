library(quantmod)
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
library(tidyverse)
library(lubridate)

setwd("C:/Data/Skola/Bachelor's Thesis/R Codes")

getSymbols("BTC-USD", env = .GlobalEnv, from = '2015-08-20', to = '2018-02-23', periodicity = 'daily')
data <- log(`BTC-USD`[,6])

##### Period1
start <- 1
end <- which(time(data) == '2017-11-15')

(data1 <- data[start:end])
MA <- rep(NA, end - start + 1)
time_frame <- 1:end

gaussian_kernel <- function(sigma, r, t){
  (1 / (sqrt(2*pi) * sigma)) * exp(-((r-t)^2) / (2 * sigma^2))
}


for (t in time_frame){
  MA[t] <- sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 5, r = x, t = t) * data1[x])) /
    sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 5, r = x, t = t)))
}

residuals <- rep(NA, end - start + 1)
residuals[1:end] <- data1[1:end] - MA[1:end]
plot(residuals, type = 'l')

### 100 days
# AR(1)
residuals.ar <- residuals[1:nrow(data)]
AR <- rep(NA, end - 100)

end1 <- end - 100
for (i in 1:end1){
  AR[i] <- summary(lm(residuals.ar[(i+1):(i+99)] ~ residuals.ar[i:(i+98)]))$coefficients[2,1]
}
AR <- c(rep(NA, 100), AR, rep(NA, 100))
plot(AR, type = 'l')

# Variance
variance <- rep(NA, end - 100)
for (i in 1:end1){
  start <- i
  end2 <- i + 100
  variance[i] <- sqrt(var(residuals[start:end2]))
}
plot(variance, type = 'l')
variance <- c(rep(NA, 100), variance, rep(NA, 100))
print(length(variance))
plot(variance, type = 'l')


###### PLOTS
p1 <- ggplot(data = NULL, mapping = aes(x = decimal_date(time(data)), y = data$`BTC-USD.Adjusted`)) +
  geom_line(size = 0.8, colour = 'black') +
  geom_line(aes(x = decimal_date(time(data)), y = c(MA,rep(NA,100))), size = 0.8, colour = 'red', alpha = 0.8) +
  geom_vline(xintercept = max(decimal_date(time(data1))), linetype = 'dashed', size = 0.8) + 
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = c(min(decimal_date(time(data))),
                                max(decimal_date(time(data)))), expand = c(0,0)) +
  scale_y_continuous(limits = c(5.3, 10.5), expand = c(0,0)) +
  theme_gray() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 11),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 11),
        plot.title = element_text(face = 'bold', hjust = 0.5, size = 16)) +
  geom_label(aes(x = max(decimal_date(time(data))) - 0.07, y = 10.3, fontface = 2),
             label = 'BTC-USD', fill = 'grey')

p1

##### P2, residuals------
#residuals <- c(residuals, rep(NA, 100))

p2 <- ggplot(data = NULL, mapping = aes(x = decimal_date(time(data)), y = residuals)) +
  geom_segment(mapping = aes(xend = decimal_date(time(data)), yend = 0), size = 0.7) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 11),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 11)) +
  scale_x_continuous(limits = c(min(decimal_date(time(data))),
                                max(decimal_date(time(data)))), expand = c(0,0)) +
  geom_label(aes(x = max(decimal_date(time(data))) - 0.08, y = 0.2, fontface = 2),
             label = 'Residuals', fill = 'grey')

p2

##### P3, AR(1)------
p3 <- ggplot(data = NULL, mapping = aes(x = decimal_date(time(data)), y = AR)) +
  geom_line(size = 0.8) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 11),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 11)) +
  scale_x_continuous(limits = c(min(decimal_date(time(data))),
                                max(decimal_date(time(data)))), expand = c(0,0)) +
  geom_label(aes(x = max(decimal_date(time(data))) - 0.046, y = 0.93, fontface = 2),
             label = 'AR(1)', fill = 'grey')

p3


##### P4, var------
p4 <- ggplot(data = NULL, mapping = aes(x = decimal_date(time(data)), y = sqrt(variance))) +
  geom_line(size = 0.8) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 11),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 11),
        axis.title.x = element_text(face = 'bold', colour = 'black', size = 14)) +
  scale_x_continuous(limits = c(min(decimal_date(time(data))),
                                max(decimal_date(time(data)))), expand = c(0,0)) +
  geom_label(aes(x = max(decimal_date(time(data))) - 0.07, y = 0.245, fontface = 2),
             label = 'Std. dev', fill = 'grey')

p4


g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g4 <- ggplotGrob(p4)
g <- rbind(g1, g2, g3, g4, size = 'first')
g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths, g4$widths)
grid.newpage()
grid.draw(g)

png('BTC.png', width = 900, height = 600)
grid.newpage()
grid.draw(g)
dev.off()
