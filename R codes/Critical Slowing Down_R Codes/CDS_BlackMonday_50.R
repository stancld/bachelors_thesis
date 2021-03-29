library(quantmod)
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
library(tidyverse)

setwd("C:/Data/Skola/Bachelor's Thesis/R Codes")

#####
# Loading and handling data
getSymbols('^GSPC', env = .GlobalEnv, from = '1986-03-15', to = '1988-06-24', periodicity = 'daily')
data <- GSPC$GSPC.Adjusted
data <- log(data)
head(data)

start <- which(time(data) == '1987-10-16') - 100
end <- which(time(data) == '1987-10-16') + 100

data <- data[start:end,]
length(data)

#####
# Smoothing
MA <- rep(NA, end - start + 1)
time_frame <- 1:100

gaussian_kernel <- function(sigma, r, t){
  (1 / (sqrt(2*pi) * sigma)) * exp(-((r-t)^2) / (2 * sigma^2))
}


for (t in time_frame){
  MA[t] <- sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 5, r = x, t = t) * data[x])) /
    sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 5, r = x, t = t)))
}

residuals <- rep(NA, end - start + 1)
residuals[1:200] <- data[1:200] - MA[1:200]
plot(residuals)

#####
# AR(1)
residuals.ar <- residuals[1:100]
AR <- rep(NA, 50)

for (i in 1:50){
  AR[i] <- summary(lm(residuals.ar[(i+1):(i+49)] ~ residuals.ar[i:(i+48)]))$coefficients[2,1]
}

AR <- c(rep(NA, 50), AR, rep(NA, 101))
plot(AR)
#####
# variance
variance <- rep(NA, 50)
for (i in 1:50){
  start <- i
  end <- i + 49
  variance[i] <- var(residuals[start:end])
}

variance <- c(rep(NA, 50), variance, rep(NA, 101))


#####
# Kendall's tau
AR.tau <- AR[complete.cases(AR)]
m <- cbind(AR.tau, time(data)[51:100])
(AR.kendall = cor(m, method = 'kendall', use = 'pairwise')[2,1])

VAR.tau <- variance[complete.cases(variance)]
w <- cbind(VAR.tau, time(data)[51:100])
(VAR.kendall = cor(w, method = 'kendall', use = 'pairwise')[2,1])

# signifiance
N = 50 * 49 / 2
sigmic = sqrt((2*(2*N + 5)) / (9*N*(N-1)))

(Z.AR = AR.kendall / sigmic)
(Z.VAR = VAR.kendall / sigmic)

#####
# Plotting
##### P1, price------
p1 <- ggplot(data = data, mapping = aes(x = -100:100, y = GSPC.Adjusted)) +
  geom_line(size = 0.8, colour = 'black') +
  geom_line(aes(x = -100:100, y = MA), size = 0.8, colour = 'red', alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = 'dashed', size = 0.7) + 
  geom_vline(xintercept = -50, linetype = 'dotted', size = 0.7) +
  geom_segment(aes(x = -100, y = 5.45, xend = -50, yend = 5.45), arrow = arrow(length = unit(0.2, 'cm'))) +
  geom_segment(aes(x = -50, y = 5.45, xend = -100, yend = 5.45), arrow = arrow(length = unit(0.2, 'cm'))) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 10),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 9, angle = 45),
        plot.title = element_text(face = 'bold', hjust = 0.5, size = 16)) +
  scale_x_discrete(limits = c(-100, -75, -50, -25, 0, 50, 100)) +
  annotate("text", label = "Sliding window", x = -75, y = 5.485,
           fontface = 2, size = 3) +
  geom_label(aes(x = 92.9, y = 5.83, fontface = 2),
             label = 'S&P500', fill = 'grey')

p1

##### P2, residuals------
p2 <- ggplot(data = NULL, mapping = aes(x = -100:100, y = residuals)) +
  geom_segment(mapping = aes(xend = -100:100 ,yend = 0), size = 0.8) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 10),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 9, angle = 45)) +
  scale_x_discrete(limits = c(-100, -75, -50, -25, 0, 50, 100)) +
  geom_label(aes(x = 90.5, y = 0.0325, fontface = 2),
             label = 'Residuals', fill = 'grey')

p2

##### P3, AR(1)------
p3 <- ggplot(data = NULL, mapping = aes(x = -100:100, y = AR)) +
  geom_line(size = 0.8) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 10),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 9, angle = 45)) +
  scale_x_discrete(limits = c(-100, -75, -50, -25, 0, 50, 100)) +
  geom_label(aes(x = 95.3, y = 0.875, fontface = 2),
             label = 'AR(1)', fill = 'grey') +
  geom_label(aes(x = - 70, y = 0.754),
             label = paste0("Kendall's tau = ", round(AR.kendall,3), ' (p-val = 2e-16)'),
             size = 3)

p3


##### P4, var------
p4 <- ggplot(data = NULL, mapping = aes(x = -100:100, y = sqrt(variance))) +
  geom_line(size = 0.8) +
  xlab("Time (days)") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 10),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 9, angle = 45),
        axis.title.x = element_text(face = 'bold', colour = 'black', size = 14)) +
  scale_x_discrete(limits = c(-100, -75, -50, -25, 0, 50, 100)) +
  geom_label(aes(x = 92, y = 0.0178, fontface = 2),
             label = 'Std. dev', fill = 'grey') +
  geom_label(aes(x = - 70, y = 0.01525),
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

png('CSD_BlackMonday87.png', width = 900, height = 600)
grid.newpage()
grid.draw(g)
dev.off()


##### Market correlation
# 1. Getting Data
getSymbols(c("^GSPC","^N225", "^FTSE"), env=.GlobalEnv, from = '1986-03-15', to = '1988-06-24', periodicity="daily")

gspc <- GSPC$GSPC.Adjusted
nikkei <- N225$N225.Adjusted
ftse <- FTSE$FTSE.Adjusted


start <- which(time(gspc) == '1987-10-16') - 100
end <- which(time(gspc) == '1987-10-16') + 100

gspc <- gspc[start:end,]
nikkei <- nikkei[start:end,]
ftse <- ftse[start:end,]

DATA <- as.data.frame(cbind(as.numeric(gspc), as.numeric(nikkei), as.numeric(ftse)))
colnames(DATA) <- c('GSPC', 'N225', 'FTSE')
DATA

for (j in 1:3){
  for (i in 1:201){
    if (is.na(DATA[i,j]) == T){
      DATA[i,j] <- DATA[i-1, j]
    }
  }
}

DATA[!complete.cases(DATA),]

(DATA <- log(DATA))

#####
# Smoothing
gaussian_kernel <- function(sigma, r, t){
  (1 / (sqrt(2*pi) * sigma)) * exp(-((r-t)^2) / (2 * sigma^2))
}

# SP500
time_frame <- 1:100
MA.gspc <- rep(NA, 100)

for (t in time_frame){
  MA.gspc[t] <- sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 5, r = x, t = t) * DATA[x,1])) /
    sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 5, r = x, t = t)))
}

residuals.gspc <- DATA[1:100,1] - MA.gspc[1:100]
plot(residuals.gspc)


# Nikkei
MA.n225 <- rep(NA, end - start + 1)
time_frame <- 1:100

for (t in time_frame){
  MA.n225[t] <- sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t) * DATA[x,2])) /
    sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t)))
}

residuals.n225 <- DATA[1:100,2] - MA.n225[1:100]
plot(residuals.n225)


# FTSE
MA.ftse <- rep(NA, end - start + 1)
time_frame <- 1:100

for (t in time_frame){
  MA.ftse[t] <- sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t) * DATA[x,3])) /
    sum(sapply(time_frame, function(x) gaussian_kernel(sigma = 10, r = x, t = t)))
}

residuals.ftse <- DATA[1:100,3] - MA.ftse[1:100]
plot(residuals.ftse)

# 2. computing correlations
gspcXnikkei <- rep(NA, 50)
for (i in 1:50){
  end <- i + 49
  gspcXnikkei[i] <- cor(x = residuals.gspc[i:end],
                        y = residuals.n225[i:end])
}
gspcXnikkei <- c(rep(NA, 50), gspcXnikkei, rep(NA, 101))

gspcXftse <- rep(NA, 50)
for (i in 1:50){
  end <- i + 49
  gspcXftse[i] <- cor(x = residuals.gspc[i:end],
                      y = residuals.ftse[i:end])
}
gspcXftse <- c(rep(NA, 50), gspcXftse, rep(NA, 101))

# Kendall's tau
nikkei.tau <- gspcXnikkei[complete.cases(gspcXnikkei)]
m <- cbind(nikkei.tau, 51:100)
(nikkei.kendall = cor(m, method = 'kendall', use = 'pairwise')[2,1])

ftse.tau <- gspcXftse[complete.cases(gspcXftse)]
w <- cbind(ftse.tau, 51:100)
(ftse.kendall = cor(w, method = 'kendall', use = 'pairwise')[2,1])

# signifiance
N = 50 * 49 / 20
sigmic = sqrt((2*(2*N + 5)) / (9*N*(N-1)))

(Z.nikkei = nikkei.kendall / sigmic)
(Z.ftse = ftse.kendall / sigmic)



#####
# Plotting correlation
p1 <- ggplot(data = data, mapping = aes(x = -200:100, y = GSPC.Adjusted)) +
  geom_line(size = 0.8, colour = 'black') +
  geom_line(aes(x = -200:100, y = MA), size = 0.8, colour = 'red', alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = 'dashed', size = 0.7) + 
  geom_vline(xintercept = -100, linetype = 'dotted', size = 0.7) +
  geom_segment(aes(x = -200, y = 5.45, xend = -100, yend = 5.45), arrow = arrow(length = unit(0.2, 'cm'))) +
  geom_segment(aes(x = -100, y = 5.45, xend = -200, yend = 5.45), arrow = arrow(length = unit(0.2, 'cm'))) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 10),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 9, angle = 45),
        plot.title = element_text(face = 'bold', hjust = 0.5, size = 16)) +
  scale_x_discrete(limits = c(-200, -150, -100, -50, 0, 50, 100)) +
  annotate("text", label = "Sliding window", x = -150, y = 5.485,
           fontface = 2, size = 3) +
  geom_label(aes(x = 92.9, y = 5.83, fontface = 2),
             label = 'S&P500', fill = 'grey')
p1

p2 <- ggplot(data = NULL) +
  geom_line(aes(x = -200:100, y = gspcXnikkei, colour = 'NIKKEI'), size = 0.8) +
  geom_line(aes(x = -200:100, y = gspcXftse, colour = 'FTSE'), size = 0.8) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 10),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 9, angle = 45),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.position = c(0.005, 0.97), legend.justification = c('left', 'top')) +
  scale_x_discrete(limits = c(-200, -150, -100, -50, 0, 50, 100)) +
  scale_color_manual('', breaks = c('NIKKEI', 'FTSE'),
                     values = c('darkblue', 'green')) +
  geom_label(aes(x = 87.5, y = 0.4, fontface = 2),
             label = 'Market corr.', fill = 'grey')

p2



g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)

g <- rbind(g1, g2, size = 'first')
g$widths <- unit.pmax(g1$widths, g2$widths)
grid.newpage()
grid.draw(g)

png('CSD_BlackMonday87_2.png', width = 900, height = 300)
grid.newpage()
grid.draw(g)
dev.off()










# Plotting
##### P1, price------
p1 <- ggplot(data = data, mapping = aes(x = -100:100, y = GSPC.Adjusted)) +
  geom_line(size = 0.8, colour = 'black') +
  geom_line(aes(x = -100:100, y = MA), size = 0.8, colour = 'red', alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = 'dashed', size = 0.7) + 
  geom_vline(xintercept = -50, linetype = 'dotted', size = 0.7) +
  geom_segment(aes(x = -100, y = 5.45, xend = -50, yend = 5.45), arrow = arrow(length = unit(0.2, 'cm'))) +
  geom_segment(aes(x = -50, y = 5.45, xend = -100, yend = 5.45), arrow = arrow(length = unit(0.2, 'cm'))) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 10),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 9, angle = 45),
        plot.title = element_text(face = 'bold', hjust = 0.5, size = 16)) +
  scale_x_discrete(limits = c(-100, -75, -50, -25, 0, 50, 100)) +
  annotate("text", label = "Sliding window", x = -75, y = 5.485,
           fontface = 2, size = 3) +
  geom_label(aes(x = 95.1, y = 5.826, fontface = 2),
             label = 'S&P500', fill = 'grey')

p1

##### P2, residuals------
p2 <- ggplot(data = NULL, mapping = aes(x = -100:100, y = residuals)) +
  geom_segment(mapping = aes(xend = -100:100 ,yend = 0), size = 0.8) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 10),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 9, angle = 45)) +
  scale_x_discrete(limits = c(-100, -75, -50, -25, 0, 50, 100)) +
  geom_label(aes(x = 93.35, y = 0.0325, fontface = 2),
             label = 'Residuals', fill = 'grey')

p2

##### P3, AR(1)------
p3 <- ggplot(data = NULL, mapping = aes(x = -100:100, y = AR)) +
  geom_line(size = 0.8) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 10),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 9, angle = 45)) +
  scale_x_discrete(limits = c(-100, -75, -50, -25, 0, 50, 100)) +
  geom_label(aes(x = 96.24, y = 0.875, fontface = 2),
             label = 'AR(1)', fill = 'grey') +
  geom_label(aes(x = - 79.8, y = 0.65),
             label = paste0("Kendall's tau = ", round(AR.kendall,3), ' (p-val = 2e-16)'),
             size = 3)

p3


##### P4, var------
p4 <- ggplot(data = NULL, mapping = aes(x = -100:100, y = sqrt(variance))) +
  geom_line(size = 0.8) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 10),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 9, angle = 45),
        axis.title.x = element_text(face = 'bold', colour = 'black', size = 14)) +
  scale_x_discrete(limits = c(-100, -75, -50, -25, 0, 50, 100)) +
  geom_label(aes(x = 94.8, y = 0.0178, fontface = 2),
             label = 'Std. dev', fill = 'grey') +
  geom_label(aes(x = - 80, y = 0.008),
             label = paste0("Kendall's tau = ", round(VAR.kendall,3), ' (p-val = 2e-16)'),
             size = 3)

p4

##### Pp5, corr------
p5 <- ggplot(data = NULL) +
  geom_line(aes(x = -100:100, y = gspcXnikkei, colour = 'NIKKEI'), size = 0.8) +
  geom_line(aes(x = -100:100, y = gspcXftse, colour = 'FTSE'), size = 0.8) +
  xlab("Time (days)") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 10),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 9, angle = 45),
        axis.title.x = element_text(face = 'bold', colour = 'black', size = 14),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.position = c(0.005, 0.97), legend.justification = c('left', 'top')) +
  scale_x_discrete(limits = c(-100, -75, -50, -25, 0, 50, 100)) +
  scale_color_manual('', breaks = c('NIKKEI', 'FTSE'),
                     values = c('darkblue', 'green')) +
  geom_label(aes(x = 91.9, y = 0.4, fontface = 2),
             label = 'Market corr.', fill = 'grey')

##### Multiplot
#
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g4 <- ggplotGrob(p4)
g5 <- ggplotGrob(p5)
g <- rbind(g1, g2, g3, g4, g5, size = 'first')
g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths, g4$widths, g5$widths)
grid.newpage()
grid.draw(g)

png('CSD_BlackMonday87_50.png', width = 900, height = 750)
grid.newpage()
grid.draw(g)
dev.off()