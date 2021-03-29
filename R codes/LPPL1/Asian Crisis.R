
# Loading packages --------------------------------------------------------
library(quantmod)
library(lubridate)
library(hash)
library(ggplot2)
library(tidyverse)
library(tseries)

setwd("C:/Data/Skola/Bachelor's Thesis/R Codes/LPPL1")

# Data and Grid -----------------------------------------------------------
getSymbols('^HSI', env = .GlobalEnv, from = '1995-02-12', to = '1998-05-24', periodicity = 'daily')
base_data <- HSI$HSI.Adjusted
plot(base_data)

head(base_data)
for (i in 1:length(base_data)){
  if (is.na(base_data[i,1]) == T){
    base_data[i,1] <- base_data[i-1,1]
  }
}


base_data <- log(base_data)
end <- which(time(base_data) == '1997-10-17') - 50
base_data <- base_data[1:end,]
plot(base_data)


# Extracting time in decimal years
base_data$t <- decimal_date(time(base_data))
head(base_data)

colnames(base_data) <- c('price', 't')
head(base_data)

write.csv(base_data, 'AsianCrisis.csv')


# Defining functions ------------------------------------------------------

# Loss Function
L = function(A, B, C, t_c, m, omega, phi, price, t){
  loss <- sum((price - (A + B*abs(t_c - t)^m + (C*abs(t_c-t)^m*cos(omega * log(abs(t_c - t)) - phi))))^2)
}

TabuSearch <- function(A, B, C){
  S <- hash() # S represents current solution
  iter_without_improvement <- 0
  
  # Generating a set of initial 10 * 3 parameters
  t_c_set <- runif(n = 40, min = max(base_data$t), max = max(base_data$t) + 0.6) # adding 0.6 year represent time horizon within the crash is expected
  m_set <- runif(n = 40, min = 0.1, max = 0.9)
  omega_set <- runif(n = 40, min = 6, max = 13)
  phi_set <- runif(n = 40, min = 0, max = 2*pi)
  
  # Looking for the 10 elite solutions out of 30 initial points, the best one is then our starting point. Furthermore, we choose the worst solution to set the taboo condition.
  elite_list <- vector()
  random_solutions <- cbind(t_c_set, m_set, omega_set, phi_set)
  losses <- apply(random_solutions, 1, function(x) L(A = A, B = B, C = C, t_c = x[1], m = x[2], omega = x[3], phi = x[4], price = base_data$price, t = base_data$t))
  losses <- as.data.frame(cbind('loss' = losses, 't_c' = t_c_set, 'm' = m_set, 'omega' = omega_set, 'phi' = phi_set))
  losses <- losses %>%
    arrange(loss)
  elite_list <- losses[1:10,]
  taboo_condition <- losses[nrow(losses),1]
  S[['loss']] <- losses[1,'loss']
  S[['t_c']] <- losses[1,'t_c']
  S[['m']] <- losses[1,'m']
  S[['omega']] <- losses[1,'omega']
  S[['phi']] <- losses[1,'phi']
  if (min(losses, na.rm = TRUE) < 200){ 
    # Partitioning and setting parameters for the number of randomly drawn cells and points within them
    partitions <- c(6,6,6,6)
    n_c <- 2
    n_s <- 6
    t_c.partitions <- seq(from = max(base_data$t), to = max(base_data$t) + 4, length.out = partitions[1] + 1) # we add 1 to create 6 cells, which requires 7 borders
    m.partitions <- seq(from = 0.1, to = 0.9, length.out = partitions[2] + 1)
    omega.partitions <- seq(from = 6, to = 13, length.out = partitions[3] + 1)
    phi.partitions <- seq(from = 0, to = 2*pi, length.out = partitions[4] + 1)
    partitions_matrix <- rbind(t_c.partitions, m.partitions, omega.partitions, phi.partitions)
    
    # Searching procedure
    while (iter_without_improvement < 100){
      # Drawing n_c * n_s points for looking for new solutions
      chosen_cells <- t(sapply(partitions, function(x) sample(1:x, size = n_c, replace = FALSE)))
      drawn_points <- sapply(1:nrow(partitions_matrix), function(row) sapply(chosen_cells[row,], 
                                                                             function(x) as.vector(sapply(x,function(y) 
                                                                               runif(n = n_s, min = partitions_matrix[row,y], max = partitions_matrix[row,y + 1])))))
      colnames(drawn_points) <- c('t_c', 'm', 'omega', 'phi')
      
      # Computing the value of loss function for new points, dropping points returning losses in a taboo region
      losses <- apply(drawn_points, 1, function(x) L(A = A, B = B, C = C, t_c = x[1], m = x[2], omega = x[3], phi = x[4], price = base_data$price, t = base_data$t))
      drawn_points <- cbind('loss' = losses, drawn_points)
      drawn_points <- drawn_points[complete.cases(drawn_points),,drop = FALSE] # dropping all points with non-defined loss function
      losses <- losses[complete.cases(losses)] # dropping points from losses, too
      non.taboo <- ifelse(losses < taboo_condition, TRUE, FALSE)
      drawn_points <- drawn_points[non.taboo,,drop = FALSE]
      
      # Picking the nontaboo point with the lowest move value - move value is defined as loss at step t+1 minus loss at step t
      if (nrow(drawn_points) > 0){
        S[['loss']] <- drawn_points[which.min(drawn_points[,'loss'] - S$loss), 'loss']
        S[['t_c']] <- drawn_points[which.min(drawn_points[,'loss'] - S$loss), 't_c']
        S[['m']] <- drawn_points[which.min(drawn_points[,'loss'] - S$loss), 'm']
        S[['omega']] <- drawn_points[which.min(drawn_points[,'loss'] - S$loss), 'omega']
        S[['phi']] <- drawn_points[which.min(drawn_points[,'loss'] - S$loss), 'phi']
        
        # Executing elite_list modification in case of the loss of a new points is lower than the loss of the 10th element in the elite_list
        if (S$loss < elite_list[10,'loss']){
          elite_list <- rbind(elite_list[1:9,], drawn_points[which.min(drawn_points[,'loss'] - S$loss),]) %>%
            arrange(loss)
          iter_without_improvement <- 0
        } else {
          iter_without_improvement <- iter_without_improvement + 1
        }
      } else {
        iter_without_improvement <- iter_without_improvement + 1
      }
    }
  }
  # Filling results to the Grid
  grid <- grid %>%
    filter(a == A, b == B, c == C) %>%
    mutate(loss = elite_list[1,'loss'], t_c = elite_list[1,'t_c'], m = elite_list[1,'m'], omega = elite_list[1,'omega'], phi = elite_list[1,'phi'])
  print(elite_list)
  return(grid)
}


# Creating Grid -----------------------------------------------------------

grid <- list(a = seq(9.7, 10, length.out = 5),
             b = seq(-1, -0.1, by = 0.1),
             c = seq(0.01, 0.05, by = 0.01)) %>%
  cross_df()
grid$loss <- rep(as.numeric(NA), nrow(grid))
grid$t_c <- rep(as.numeric(NA), nrow(grid))
grid$m <- rep(as.numeric(NA), nrow(grid))
grid$omega <- rep(as.numeric(NA), nrow(grid))
grid$phi <- rep(as.numeric(NA), nrow(grid))


# Executing the algorithm and saving solutions ----------------------------
beginning <- Sys.time()
new_grid <- apply(grid, 1, function(x) TabuSearch(A = x[1], B = x[2], C = x[3]))
final <- Sys.time()
(length <- final - beginning)


data <- data.frame(matrix(unlist(new_grid), nrow=length(new_grid), byrow=T))
colnames(data) <- c('A', 'B', 'C', 'loss', 't_c', 'm', 'omega', 'phi')
data <- data %>%
  arrange(loss)
data
write.csv(data, 'AsianCrisis_TS.csv')
TOP15 <- head(data, 15)
write.csv(TOP15, 'Asian10.csv')

# OLS ---------------------------------------------------------------------
(ABC <- read.csv('ABC.csv'))
FITS <- rep(NA, nrow(ABC))

for (i in 1:length(FITS)){

  t_c <- ABC[i,1]
  m <- ABC[i,2]
  omega <- ABC[i,3]
  phi <- ABC[i,4]

  base_data$B <- (t_c - base_data$t)^m
  head(base_data)
  base_data$C <- ((t_c - base_data$t)^m) * cos(omega * log(abs(t_c - base_data$t)) - phi)
  head(base_data)

  lmodel <- lm(price ~ B + C, data = base_data)
  FITS[i] <- as.numeric(summary(lmodel)[8])
}
print(FITS)

best <- 2

t_c <- ABC[best,1]
m <- ABC[best,2]
omega <- ABC[best,3]
phi <- ABC[best,4]

base_data$B <- (t_c - base_data$t)^m
head(base_data)
base_data$C <- ((t_c - base_data$t)^m) * cos(omega * log(abs(t_c - base_data$t)) - phi)
head(base_data)

lmodel <- lm(price ~ B + C, data = base_data)
summary(lmodel)
# Extrapolating -----------------------------------------------------------
end <- which(time(HSI) == '1997-10-17') + 101
data2 <- log(HSI$HSI.Adjusted[1:end])
(data2$time <- decimal_date(time(data2)))
for (i in 1:length(data2)){
  if (is.na(data2$HSI.Adjusted[i]) == T){
    data2$HSI.Adjusted[i] <- data2$HSI.Adjusted[i-1]
  }
}
plot(data2$HSI.Adjusted)
head(data2)

data2$B <- abs((t_c - data2$time))^m
data2$C <- (abs((t_c - data2$time))^m) * cos(omega * log(abs(t_c - data2$time)) - phi)

y_pred <- lmodel$coefficients[1] + lmodel$coefficients[2] * data2$B + lmodel$coefficients[3] * data2$C


# Plotting ----------------------------------------------------------------
plot(as.numeric(data2$HSI.Adjusted), type = 'l')
lines(as.numeric(y_pred), col = 'blue')
abline(v = which.max(y_pred$B), col = 'red')

##### TOP PLOTTING
p2 <- ggplot(data = NULL, mapping = aes(x = decimal_date(time(data2)), y = data2$HSI.Adjusted)) +
  geom_line(size = 0.8) +
  geom_line(mapping = aes(x = decimal_date(time(data2)), y = y_pred$B), size = 0.8, col = 'blue') +
  geom_vline(xintercept = t_c , linetype = 'dashed', size = 0.8, col = 'red') +
  geom_vline(xintercept = decimal_date(ymd('1997-10-17')), linetype = 'dashed', size = 0.7, col = 'black') +
  geom_vline(xintercept = 1997.6, linetype = 'dotted', size = 0.8, col = 'black') +
  ylab("") +
  xlab('Time (years)') +
  scale_y_continuous(limits = c(8.9, 9.83), expand = c(0,0)) +
  scale_x_continuous(limits = c(min(decimal_date(time(data2))),
                                max(decimal_date(time(data2)))), expand = c(0,0)) +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', colour = 'black', size = 10),
        axis.text.y = element_text(face = 'bold', colour = 'black', size = 9, angle = 45),
        axis.title.x = element_text(face = 'bold', colour = 'black', size = 14),
        plot.margin = margin(0, 0, 0, 0, 'cm')) +
  geom_label(aes(x = 1995.23, y = 9.81), fontface = 2,
             label = 'Hang Seng', fill = 'grey')

p2

png('LPPL1_AsianCrisis.png', width = 900, height = 300)
p2
dev.off()


