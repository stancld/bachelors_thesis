library(quantmod)
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
library(tidyverse)

setwd("C:/Data/Skola/Bachelor's Thesis/R Codes")

#####
# Loading and handling data
getSymbols('^HSI', env = .GlobalEnv, from = '1996-01-11', to = '1998-05-24', periodicity = 'daily')
data <- HSI$HSI.Adjusted
head(data)

start <- which(time(data) == '1997-10-17') - 200
end <- which(time(data) == '1997-10-17') + 100

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

for (i in 1:100){
  AR[i] <- summary(lm(residuals.ar[(i+1):(i+99)] ~ residuals.ar[i:(i+98)]))$coefficients[2,1]
}

AR <- c(rep(NA, 100), AR, rep(NA, 101))
plot(AR)
#####
# variance
variance <- rep(NA, 100)
for (i in 1:100){
  start <- i
  end <- i + 99
  variance[i] <- var(residuals[start:end])
}

variance <- c(rep(NA, 100), variance, rep(NA, 101))
plot(variance)

#####
# Kendall's tau
AR.tau <- AR[complete.cases(AR)]
m <- cbind(AR.tau, 101:200)
(AR.kendall = cor(m, method = 'kendall', use = 'pairwise')[2,1])

VAR.tau <- variance[complete.cases(variance)]
w <- cbind(VAR.tau, 101:200)
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
  geom_segment(aes(x = -200, y = 9.05, xend = -100, yend = 9.05), arrow = arrow(length = unit(0.2, 'cm'))) +
  geom_segment(aes(x = -100, y = 9.05, xend = -200, yend = 9.05), arrow = arrow(length = unit(0.2, 'cm'))) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 10),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 9, angle = 45),
        plot.title = element_text(face = 'bold', hjust = 0.5, size = 16)) +
  scale_x_discrete(limits = c(-200, -150, -100, -50, 0, 50, 100)) +
  annotate("text", label = "Sliding window", x = -150, y = 9.11,
           fontface = 2, size = 3) +
  geom_label(aes(x = 96.85, y = 9.71, fontface = 2),
             label = 'HSI', fill = 'grey')

p1

##### P2, residuals------
p2 <- ggplot(data = NULL, mapping = aes(x = -200:100, y = residuals)) +
  geom_segment(mapping = aes(xend = -200:100 ,yend = 0), size = 0.8) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 10),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 9, angle = 45)) +
  scale_x_discrete(limits = c(-200, -150, -100, -50, 0, 50, 100)) +
  geom_label(aes(x = 90.5, y = 0.065, fontface = 2),
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
  scale_x_discrete(limits = c(-200, -150, -100, -50, 0, 50, 100)) +
  geom_label(aes(x = 95.3, y = 0.923, fontface = 2),
             label = 'AR(1)', fill = 'grey') +
  geom_label(aes(x = - 170, y = 0.784),
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
  scale_x_discrete(limits = c(-200, -150, -100, -50, 0, 50, 100)) +
  geom_label(aes(x = 92, y = 0.029, fontface = 2),
             label = 'Std. dev', fill = 'grey') +
  geom_label(aes(x = - 170, y = 0.018),
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
  
  png('CSD_AsianCrisis.png', width = 900, height = 600)
  grid.newpage()
  grid.draw(g)
  dev.off()
