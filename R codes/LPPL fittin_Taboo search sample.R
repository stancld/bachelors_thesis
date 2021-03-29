
# Loading packages --------------------------------------------------------
library(quantmod)
library(lubridate)
library(hash)
library(ggplot2)
library(tidyverse)

# Data and Grid -----------------------------------------------------------
getSymbols('^FTSE', env = .GlobalEnv, from = '2003-03-15', to = '2007-02-24', periodicity = 'daily')
base_data <- FTSE$FTSE.Adjusted
base_data <- base_data[complete.cases(base_data),]
plot(log(base_data))

  # Extracting time in decimal years
base_data$t <- decimal_date(time(base_data))
head(base_data)

colnames(base_data) <- c('price', 't')
head(base_data)

# Defining functions ------------------------------------------------------

  # Loss Function
L = function(A, B, C, t_c, m, omega, phi, price, t){
  C_1 <- C * cos(phi)
  C_2 <- C * sin(phi)
  loss <- sum((log(price) - (A + B*abs(t_c - t)^m) + (C_1*abs(t_c - t)^m) * cos(omega * log(abs(t_c - t))) + (C_2*abs(t_c - t)^m) * sin(omega * log(abs(t_c - t))))^2)
}

  # Gradient
derivative <- function(t_c, A, B, C, phi, m, omega, price = base_data$price, t = base_data$t) {
  C_1 <- C * cos(phi)
  C_2 <- C * sin(phi)
  derivative <- sum(2*(log(price) - (A + B*abs(t_c - t)^m) + (C_1*abs(t_c - t)^m) * cos(omega * log(abs(t_c - t))) + (C_2*abs(t_c - t)^m) * sin(omega * log(abs(t_c - t)))) *
                      (-(B*m*(t_c - t)^(m-1) + (C_1*m*(t_c - t)^(m-1)) * cos(omega*log(abs(t_c-t)))
                         - (C_1*(t_c - t)^m) * sin(omega*log(abs(t_c - t))) * omega/(t_c - t)
                         + (C_2*m*(t_c - t)^(m-1)) * sin(omega*log(abs(t_c-t)))
                         + (C_2*(t_c - t)^m) * cos(omega*log(abs(t_c - t))) * omega/(t_c - t))))
  return(derivative)
}

  # Simplified Taboo Search Algorithm
TabuSearch <- function(A, B, C, phi){
  S <- hash() # S represents current solution
  iter_without_improvement <- 0
  
  # Generating a set of initial 10 * 3 parameters
  t_c_set <- runif(n = 30, min = max(base_data$t), max = max(base_data$t) + 1.5) # adding 1.5 years represent time horizon within the crash is expected
  m_set <- runif(n = 30, min = 0.1, max = 0.9)
  omega_set <- runif(n = 30, min = 6, max = 13)
  
  # Looking for the 10 elite solutions out of 30 initial points, the best one is then our starting point. Furthermore, we choose the worst solution to set the taboo condition.
  elite_list <- vector()
  random_solutions <- cbind(t_c_set, m_set, omega_set)
  losses <- apply(random_solutions, 1, function(x) L(A = A, B = B, C = C, t_c = x[1], m = x[2], omega = x[3], phi = phi, price = base_data$price, t = base_data$t))
  losses <- as.data.frame(cbind('loss' = losses, 't_c' = t_c_set, 'm' = m_set, 'omega' = omega_set))
  losses <- losses %>%
    arrange(loss)
  elite_list <- losses[1:10,]
  taboo_condition <- losses[nrow(losses),1]
  S[['loss']] <- losses[1,'loss']
  S[['t_c']] <- losses[1,'t_c']
  S[['m']] <- losses[1,'m']
  S[['omega']] <- losses[1,'omega']
  if (min(losses, na.rm = TRUE) < 200){ 
    # Partitioning and setting parameters for the number of randomly drawn cells and points within them
    partitions <- c(6,6,6)
    n_c <- 2
    n_s <- 6
    t_c.partitions <- seq(from = max(base_data$t), to = max(base_data$t) + 4, length.out = partitions[1] + 1) # we add 1 to create 6 cells, which requires 7 borders
    m.partitions <- seq(from = 0.1, to = 0.9, length.out = partitions[2] + 1)
    omega.partitions <- seq(from = 6, to = 13, length.out = partitions[3] + 1)
    partitions_matrix <- rbind(t_c.partitions, m.partitions, omega.partitions)
    
    # Searching procedure
    while (iter_without_improvement < 100){
      # Drawing n_c * n_s points for looking for new solutions
      chosen_cells <- t(sapply(partitions, function(x) sample(1:x, size = n_c, replace = FALSE)))
      drawn_points <- sapply(1:nrow(partitions_matrix), function(row) sapply(chosen_cells[row,], 
                                                                             function(x) as.vector(sapply(x,function(y) 
                                                                               runif(n = n_s, min = partitions_matrix[row,y], max = partitions_matrix[row,y + 1])))))
      colnames(drawn_points) <- c('t_c', 'm', 'omega')
      
      # Computing the value of loss function for new points, dropping points returning losses in a taboo region
      losses <- apply(drawn_points, 1, function(x) L(A = A, B = B, C = C, t_c = x[1], m = x[2], omega = x[3], phi = phi, price = base_data$price, t = base_data$t))
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
    filter(a == A, b == B, c == C, Phi == phi) %>%
    mutate(loss = elite_list[1,'loss'], t_c = elite_list[1,'t_c'], m = elite_list[1,'m'], omega = elite_list[1,'omega'])
  print(elite_list)
  return(grid)
}

# Grids -------------------------------------------------------------------
  # Taboo search grid
grid <- list(a = seq(8.85, 9.2, length.out = 5),
             b = seq(-0.6, -0.1, by = 0.1),
             c = seq(0.01, 0.05, by = 0.01),
             Phi = seq(0, 2*pi, length.out = 8)) %>%
  cross_df()
grid$loss <- rep(as.numeric(NA), nrow(grid))
grid$t_c <- rep(as.numeric(NA), nrow(grid))
grid$m <- rep(as.numeric(NA), nrow(grid))
grid$omega <- rep(as.numeric(NA), nrow(grid))

# Body - 2007-2008 crash --------------------------------------------------------------------
  # Executing GridSearch, Taboo Algoruthm and thus searching a global minima
new_grid <- apply(grid, 1, function(x) TabuSearch(A = x[1], B = x[2], C = x[3], phi = x[4]))
data <- data.frame(matrix(unlist(new_grid), nrow=length(new_grid), byrow=T))
colnames(data) <- c('A', 'B', 'C', 'phi', 'loss', 't_c', 'm', 'omega')
data <- data %>%
  arrange(loss)
data
write_csv(data, 'Taboo_results.csv')
data <- read.csv('Taboo_results.csv')

  # Filtering good fits
new_data <- data %>%
  filter(loss < min(loss) + 0.3 * min(loss))
new_data
hist(new_data$t_c)

  # Fitting OLS to obtain linear parameters
SSR_lm <- rep(NA, nrow(new_data))
SSR_pls <- rep(NA, nrow(new_data))
for (i in 1:nrow(new_data)){
  t_c <- new_data$t_c[i]
  m <- new_data$m[i]
  omega <- new_data$omega[i]
  base_data$first_term <- (abs(t_c - base_data$t))^m
  base_data$second_term <- ((abs(t_c - base_data$t))^m) * cos(omega * log(abs(t_c - base_data$t)))
  base_data$third_term <- ((abs(t_c - base_data$t))^m) * sin(omega * log(abs(t_c - base_data$t)))
  
  lmodel <- lm(log(price) ~ first_term + second_term + third_term, data = base_data)
  partial_mode <- plsr(log(price) ~ first_term + second_term + third_term, data = base_data)
  SSR_pls[i] <- (sum(log(base_data$price) - partial_mode$fitted.values[,,3])^2)
  SSR_lm[i] <- sum((lmodel$residuals)^2)
}
SSR_pls

i <- 15
t_c <- new_data$t_c[i]
m <- new_data$m[i]
omega <- new_data$omega[i]
base_data$first_term <- (abs(t_c - base_data$t))^m
base_data$second_term <- ((abs(t_c - base_data$t))^m) * cos(omega * log(abs(t_c - base_data$t)))
base_data$third_term <- ((abs(t_c - base_data$t))^m) * sin(omega * log(abs(t_c - base_data$t)))
lmodel <- lm(log(price) ~ first_term + second_term + third_term, data = base_data)
lmodel$coefficients[1]
partial_mode <- plsr(log(price) ~ first_term + second_term + third_term, data = base_data)

  # Building new model
new_model <- TabuSearch(A = lmodel$coefficients[1], B = lmodel$coefficients[2], C = lmodel$coefficients[3] / 2.6927937, phi = 2.6927937)
t_c <- 2009.083
m <- 0.8172588
omega <- 11.145187
base_data$first_term <- (abs(t_c - base_data$t))^m
base_data$second_term <- ((abs(t_c - base_data$t))^m) * cos(omega * log(abs(t_c - base_data$t)))
base_data$third_term <- ((abs(t_c - base_data$t))^m) * sin(omega * log(abs(t_c - base_data$t)))
LPPL <- lm(log(price) ~ first_term + second_term + third_term, data = base_data)
summary(LPPL)

  # Predicting and plotting
getSymbols('^FTSE', env = .GlobalEnv, from = '2003-03-15', to = '2009-12-30', periodicity = 'daily')
dataset <- FTSE$FTSE.Adjusted
dataset <- dataset[complete.cases(dataset),]
dataset$t <- decimal_date(time(dataset))
head(dataset)

colnames(dataset) <- c('price', 't')
dataset$first_term <- (abs(t_c - dataset$t))^m
dataset$second_term <- ((abs(t_c - dataset$t))^m) * cos(omega * log(abs(t_c - dataset$t)))
dataset$third_term <- ((abs(t_c - dataset$t))^m) * sin(omega * log(abs(t_c - dataset$t)))
start <- nrow(base_data) + 1
fitted <- LPPL$fitted.values
fitted <- append(fitted, predict(LPPL, newdata = dataset[start:nrow(dataset),]))

fitted_pls <- partial_mode$fitted.values[,,3]
y_pred <- as.vector(predict(partial_mode, ncomp = 3, newdata = dataset[start:nrow(dataset),3:5]))
fitted_pls <- append(fitted_pls, y_pred)
dataset$fitted <- rep(NA, nrow(dataset))
dataset$fitted <- fitted_pls

ggplot(data = dataset) +
  geom_line(aes(x = t, y = log(price)), size = 1) +
  geom_line(aes(x = t, y = fitted), colour = 'blue', size = 1) +
  geom_vline(xintercept = 2007.202, linetype = 3, color = 'darkgreen', size = 0.8) +
  geom_vline(xintercept = t_c, linetype = 'dashed', color = 'red', size = 1)


# Data, Grids and Body for October 87 Crash -------------------------------------
getSymbols('DJI', env = .GlobalEnv, from = '1983-01-01', to = '1987-07-15', periodicity = 'daily')
base_data <- DJI$DJI.Adjusted
base_data <- base_data[complete.cases(base_data),]
plot(log(base_data))

# Extracting time in decimal years
base_data$t <- decimal_date(time(base_data))
head(base_data)

colnames(base_data) <- c('price', 't')
head(base_data)

grid <- list(a = seq(7.7, 8.2, length.out = 5),
             b = seq(-0.9, -0.1, by = 0.16),
             c = seq(0.01, 0.05, by = 0.01),
             Phi = seq(0, 2*pi, length.out = 8)) %>%
  cross_df()
grid$loss <- rep(as.numeric(NA), nrow(grid))
grid$t_c <- rep(as.numeric(NA), nrow(grid))
grid$m <- rep(as.numeric(NA), nrow(grid))
grid$omega <- rep(as.numeric(NA), nrow(grid))


new_grid <- apply(grid, 1, function(x) TabuSearch(A = x[1], B = x[2], C = x[3], phi = x[4]))
data <- data.frame(matrix(unlist(new_grid), nrow=length(new_grid), byrow=T))
colnames(data) <- c('A', 'B', 'C', 'phi', 'loss', 't_c', 'm', 'omega')
data <- data %>%
  arrange(loss)
data
write_csv(data, 'Taboo_results1.csv')

# Filtering good fits
new_data <- data %>%
  filter(loss < min(loss) + 0.3 * min(loss))
new_data
hist(new_data$t_c)

# Fitting OLS to obtain linear parameters
SSR <- rep(NA, nrow(new_data))
for (i in 1:nrow(new_data)){
  t_c <- new_data$t_c[i]
  m <- new_data$m[i]
  omega <- new_data$omega[i]
  base_data$first_term <- (abs(t_c - base_data$t))^m
  base_data$second_term <- ((abs(t_c - base_data$t))^m) * cos(omega * log(abs(t_c - base_data$t)))
  base_data$third_term <- ((abs(t_c - base_data$t))^m) * sin(omega * log(abs(t_c - base_data$t)))
  
  lmodel <- lm(log(price) ~ first_term + second_term + third_term, data = base_data)
  SSR[i] <- sum((lmodel$residuals)^2)
}
SSR

i <- 1
t_c <- new_data$t_c[i]
m <- new_data$m[i]
omega <- new_data$omega[i]
base_data$first_term <- (abs(t_c - base_data$t))^m
base_data$second_term <- ((abs(t_c - base_data$t))^m) * cos(omega * log(abs(t_c - base_data$t)))
base_data$third_term <- ((abs(t_c - base_data$t))^m) * sin(omega * log(abs(t_c - base_data$t)))
lmodel <- lm(log(price) ~ first_term + second_term + third_term, data = base_data)
lmodel$coefficients[1]

# Building new model
new_model <- TabuSearch(A = lmodel$coefficients[1], B = lmodel$coefficients[2], C = lmodel$coefficients[3] / 2.6927937, phi = 2.6927937)
t_c <- 2009.083
m <- 0.8172588
omega <- 11.145187
base_data$first_term <- (abs(t_c - base_data$t))^m
base_data$second_term <- ((abs(t_c - base_data$t))^m) * cos(omega * log(abs(t_c - base_data$t)))
base_data$third_term <- ((abs(t_c - base_data$t))^m) * sin(omega * log(abs(t_c - base_data$t)))
LPPL <- lm(log(price) ~ first_term + second_term + third_term, data = base_data)
summary(LPPL)

# Predicting and plotting
getSymbols('^FTSE', env = .GlobalEnv, from = '2003-03-15', to = '2009-12-30', periodicity = 'daily')
dataset <- FTSE$FTSE.Adjusted
dataset <- dataset[complete.cases(dataset),]
dataset$t <- decimal_date(time(dataset))
head(dataset)

colnames(dataset) <- c('price', 't')
dataset$first_term <- (abs(t_c - dataset$t))^m
dataset$second_term <- ((abs(t_c - dataset$t))^m) * cos(omega * log(abs(t_c - dataset$t)))
dataset$third_term <- ((abs(t_c - dataset$t))^m) * sin(omega * log(abs(t_c - dataset$t)))
start <- nrow(base_data) + 1
fitted <- LPPL$fitted.values
fitted <- append(fitted, predict(LPPL, newdata = dataset[start:nrow(dataset),]))
dataset$fitted <- rep(NA, nrow(dataset))
dataset$fitted <- fitted

ggplot(data = dataset) +
  geom_line(aes(x = t, y = log(price)), size = 1) +
  geom_line(aes(x = t, y = fitted), colour = 'blue', size = 1) +
  geom_vline(xintercept = 2007.202, linetype = 3, color = 'darkgreen', size = 0.8) +
  geom_vline(xintercept = t_c, linetype = 'dashed', color = 'red', size = 1)

