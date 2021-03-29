# Loading librariees
library(quantmod)
library(lubridate)
library(hash)
library(ggplot2)
library(tidyverse)

# Downloading Financial Data
getSymbols('^GSPC', env = .GlobalEnv, from = '1994-12-30', to = '1997-09-30', periodicity = 'daily')
GSPC <- GSPC$GSPC.Adjusted
plot(GSPC)

# Extracting time in decimal years
GSPC$t <- decimal_date(time(GSPC))
head(GSPC)

colnames(GSPC) <- c('price', 't')
head(GSPC)

# Generating linear parameters and phi, and creating Grid
grid <- list(a = seq(7.5, 9.5, by = 0.5),
             b = seq(-2.5, -0.5, by = 0.5),
             c = seq(0.01, 0.05, by = 0.01),
             Phi = seq(0, 2*pi, length.out = 4)) %>%
  cross_df()
grid$loss <- rep(as.numeric(NA), nrow(grid))
grid$t_c <- rep(as.numeric(NA), nrow(grid))
grid$m <- rep(as.numeric(NA), nrow(grid))
grid$omega <- rep(as.numeric(NA), nrow(grid))

# Defining Loss Functions
L = function(A, B, C, t_c, m, omega, phi, price, t){
    C_1 <- C * cos(phi)
    C_2 <- C * sin(phi)
    loss <- sum((log(price) - (A + B*abs(t_c - t)^m) + (C_1*abs(t_c - t)^m) * cos(omega * log(abs(t_c - t))) + (C_2*abs(t_c - t)^m) * sin(omega * log(abs(t_c - t))))^2)
}

# Formulating Simplified Version of Tabu Search Algorithm - simplified as there is only one taboo condition out of two
TabuSearch <- function(A, B, C, phi){
  S <- hash() # S represents current solution
  iter_without_improvement <- 0
  
  # Generating a set of initial 10 * 3 parameters
  t_c_set <- runif(n = 30, min = max(GSPC$t), max = max(GSPC$t) + 2.5) # adding 2.5 years represent time horizon within the crash is expected
  m_set <- runif(n = 30, min = 0.1, max = 0.9)
  omega_set <- runif(n = 30, min = 6, max = 13)
  
  # Looking for the 10 elite solutions out of 30 initial points, the best one is then our starting point. Furthermore, we choose the worst solution to set the taboo condition.
  elite_list <- vector()
  random_solutions <- cbind(t_c_set, m_set, omega_set)
  losses <- apply(random_solutions, 1, function(x) L(A = A, B = B, C = C, t_c = x[1], m = x[2], omega = x[3], phi = phi, price = GSPC$price, t = GSPC$t))
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
    t_c.partitions <- seq(from = max(GSPC$t), to = max(GSPC$t) + 4, length.out = partitions[1] + 1) # we add 1 to create 6 cells, which requires 7 borders
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
      
      # Computing the alue of loss function for new points, dropping points returning losses in a taboo region
      losses <- apply(drawn_points, 1, function(x) L(A = A, B = B, C = C, t_c = x[1], m = x[2], omega = x[3], phi = phi, price = GSPC$price, t = GSPC$t))
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

# Executing GridSearch, Taboo Algoruthm and thus searching a global minima
new_grid <- apply(grid, 1, function(x) TabuSearch(A = x[1], B = x[2], C = x[3], phi = x[4]))
data <- data.frame(matrix(unlist(new_grid), nrow=length(new_grid), byrow=T))
colnames(data) <- c('A', 'B', 'C', 'phi', 'loss', 't_c', 'm', 'omega')
data <- data %>%
  arrange(loss)
data
write_csv(data, 'Taboo_results.csv')

# Preparing dataset for OLS, executing OLS and choosing the best parameters for out model
getSymbols('^GSPC', env = .GlobalEnv, from = '1994-12-30', to = '2001-12-30', periodicity = 'daily')
dataset <- GSPC

dataset$t <- decimal_date(time(GSPC))
head(dataset)
tail(dataset)
colnames(dataset) <- c('price', 't')

SSR <- rep(NA, nrow(data))
for (i in 1:nrow(data)){
  t_c <- data$t_c[i]
  m <- data$m[i]
  omega <- data$omega[i]
  dataset$first_term <- (abs(t_c - dataset$t))^m
  dataset$second_term <- ((abs(t_c - dataset$t))^m) * cos(omega * log(abs(t_c - dataset$t)))
  dataset$third_terd <- ((abs(t_c - dataset$t))^m) * sin(omega * log(abs(t_c - dataset$t)))

  lmodel <- lm(log(price) ~ first_term + second_term + third_terd, data = dataset)
  SSR[i] <- sum((lmodel$residuals)^2)
}
# Choosing optimal parameters for our LPPL model
optimum <- which.min(SSR)
t_c <- data$t_c[optimum]
m <- data$m[optimum]
omega <- data$omega[optimum]
dataset$first_term <- (abs(t_c - dataset$t))^m
dataset$second_term <- ((abs(t_c - dataset$t))^m) * cos(omega * log(abs(t_c - dataset$t)))
dataset$third_terd <- ((abs(t_c - dataset$t))^m) * sin(omega * log(abs(t_c - dataset$t)))

LPPL <- lm(log(price) ~ first_term + second_term + third_terd, data = dataset)
summary(LPPL)
fitted <- LPPL$fitted.values
fitted <- append(fitted, predict(LPPL, newdata = dataset))
dataset$fitted <- rep(NA, nrow(dataset))
dataset$fitted <- fitted


ggplot(data = dataset) +
  geom_line(aes(x = t, y = log(price)), size = 1) +
  geom_line(aes(x = t, y = fitted), colour = 'blue', size = 1) +
  geom_vline(xintercept = 1997.75, linetype = 'dashed', color = 'red', size = 0.8)







##### Another Example on October 87 Crash
getSymbols('DJI', env = .GlobalEnv, from = '1983-01-01', to = '1987-08-15', periodicity = 'daily')
GSPC <- DJI$DJI.Adjusted
GSPC <- GSPC[complete.cases(GSPC),]
plot(log(GSPC))

# Extracting time in decimal years
GSPC$t <- decimal_date(time(GSPC))
head(GSPC)

colnames(GSPC) <- c('price', 't')
head(GSPC)

# Generating linear parameters and phi, and creating Grid
grid <- list(a = seq(7.5, 8.5, by = 0.5),
             b = seq(-2.5, -0.5, by = 0.5),
             c = seq(0.02, 0.1, by = 0.02),
             Phi = seq(0, 2*pi, length.out = 4)) %>%
  cross_df()
grid$loss <- rep(as.numeric(NA), nrow(grid))
grid$t_c <- rep(as.numeric(NA), nrow(grid))
grid$m <- rep(as.numeric(NA), nrow(grid))
grid$omega <- rep(as.numeric(NA), nrow(grid))

# Executing GridSearch, Taboo Algoruthm and thus searching a global minima
new_grid <- apply(grid, 1, function(x) TabuSearch(A = x[1], B = x[2], C = x[3], phi = x[4]))
data <- data.frame(matrix(unlist(new_grid), nrow=length(new_grid), byrow=T))
colnames(data) <- c('A', 'B', 'C', 'phi', 'loss', 't_c', 'm', 'omega')
data <- data %>%
  arrange(loss)
data
write_csv(data, 'Taboo_results.csv')

# Preparing dataset for OLS, executing OLS and choosing the best parameters for out model
getSymbols('DJI', env = .GlobalEnv, from = '1983-01-01', to = '1987-12-31', periodicity = 'daily')
dataset <- DJI$DJI.Adjusted
dataset <- dataset[complete.cases(dataset),]

dataset$t <- decimal_date(time(dataset))
head(dataset)
tail(dataset)
colnames(dataset) <- c('price', 't')

SSR <- rep(NA, nrow(data))
for (i in 1:nrow(data)){
  t_c <- data$t_c[i]
  m <- data$m[i]
  omega <- data$omega[i]
  dataset$first_term <- (abs(t_c - dataset$t))^m
  dataset$second_term <- ((abs(t_c - dataset$t))^m) * cos(omega * log(abs(t_c - dataset$t)))
  dataset$third_terd <- ((abs(t_c - dataset$t))^m) * sin(omega * log(abs(t_c - dataset$t)))
  
  lmodel <- lm(log(price) ~ first_term + second_term + third_terd, data = dataset[1:nrow(GSPC),])
  SSR[i] <- sum((lmodel$residuals)^2)
}
# Choosing optimal parameters for our LPPL model
optimum <- which.min(SSR)
t_c <- data$t_c[optimum]
m <- data$m[optimum]
omega <- data$omega[optimum]
dataset$first_term <- (abs(t_c - dataset$t))^m
dataset$second_term <- ((abs(t_c - dataset$t))^m) * cos(omega * log(abs(t_c - dataset$t)))
dataset$third_terd <- ((abs(t_c - dataset$t))^m) * sin(omega * log(abs(t_c - dataset$t)))

LPPL <- lm(log(price) ~ first_term + second_term + third_terd, data = dataset[1:nrow(GSPC),])
summary(LPPL)
fitted <- LPPL$fitted.values
fitted <- append(fitted, predict(LPPL, newdata = dataset[start:nrow(dataset),]))
dataset$fitted <- rep(NA, nrow(dataset))
dataset$fitted <- fitted

ggplot(data = dataset) +
  geom_line(aes(x = t, y = log(price)), size = 1) +
  geom_line(aes(x = t, y = fitted), colour = 'blue', size = 1) +
  geom_vline(xintercept = 1987.616, linetype = 3, color = 'darkgreen', size = 0.8) +
  geom_vline(xintercept = t_c, linetype = 'dashed', color = 'red', size = 1)







##### Another Example on 2007-2008 crash (FTSE)
getSymbols('^FTSE', env = .GlobalEnv, from = '2003-03-15', to = '2007-03-15', periodicity = 'daily')
GSPC <- FTSE$FTSE.Adjusted
GSPC <- GSPC[complete.cases(GSPC),]
plot(log(GSPC))

# Extracting time in decimal years
GSPC$t <- decimal_date(time(GSPC))
head(GSPC)

colnames(GSPC) <- c('price', 't')
head(GSPC)

# Generating linear parameters and phi, and creating Grid
grid <- list(a = seq(8.5, 9.1, by = 0.1),
             b = seq(-2, -0.2, by = 0.4),
             c = seq(0.01, 0.1, by = 0.1),
             Phi = seq(0, 2*pi, length.out = 6)) %>%
  cross_df()
grid$loss <- rep(as.numeric(NA), nrow(grid))
grid$t_c <- rep(as.numeric(NA), nrow(grid))
grid$m <- rep(as.numeric(NA), nrow(grid))
grid$omega <- rep(as.numeric(NA), nrow(grid))

# Executing GridSearch, Taboo Algoruthm and thus searching a global minima
new_grid <- apply(grid, 1, function(x) TabuSearch(A = x[1], B = x[2], C = x[3], phi = x[4]))
data <- data.frame(matrix(unlist(new_grid), nrow=length(new_grid), byrow=T))
colnames(data) <- c('A', 'B', 'C', 'phi', 'loss', 't_c', 'm', 'omega')
data <- data %>%
  arrange(loss)
data
write_csv(data, 'Taboo_results.csv')

# Preparing dataset for OLS, executing OLS and choosing the best parameters for out model
getSymbols('^FTSE', env = .GlobalEnv, from = '2003-03-15', to = '2009-06-30', periodicity = 'daily')
dataset <- FTSE$FTSE.Adjusted
dataset <- dataset[complete.cases(dataset),]

dataset$t <- decimal_date(time(dataset))
head(dataset)
tail(dataset)
colnames(dataset) <- c('price', 't')

SSR <- rep(NA, nrow(data))
for (i in 1:nrow(data)){
  t_c <- data$t_c[i]
  m <- data$m[i]
  omega <- data$omega[i]
  dataset$first_term <- (abs(t_c - dataset$t))^m
  dataset$second_term <- ((abs(t_c - dataset$t))^m) * cos(omega * log(abs(t_c - dataset$t)))
  dataset$third_terd <- ((abs(t_c - dataset$t))^m) * sin(omega * log(abs(t_c - dataset$t)))
  
  lmodel <- lm(log(price) ~ first_term + second_term + third_terd, data = dataset[1:nrow(GSPC),])
  SSR[i] <- sum((lmodel$residuals)^2)
}
# Choosing optimal parameters for our LPPL model
optimum <- which.min(SSR)
t_c <- data$t_c[optimum]
m <- data$m[optimum]
omega <- data$omega[optimum]
dataset$first_term <- (abs(t_c - dataset$t))^m
dataset$second_term <- ((abs(t_c - dataset$t))^m) * cos(omega * log(abs(t_c - dataset$t)))
dataset$third_terd <- ((abs(t_c - dataset$t))^m) * sin(omega * log(abs(t_c - dataset$t)))

LPPL <- lm(log(price) ~ first_term + second_term + third_terd, data = dataset[1:nrow(GSPC),])
summary(LPPL)

start <- nrow(GSPC) + 1
fitted <- LPPL$fitted.values
fitted <- append(fitted, predict(LPPL, newdata = dataset[start:nrow(dataset),]))
dataset$fitted <- rep(NA, nrow(dataset))
dataset$fitted <- fitted

ggplot(data = dataset) +
  geom_line(aes(x = t, y = log(price)), size = 1) +
  geom_line(aes(x = t, y = fitted), colour = 'blue', size = 1) +
  geom_vline(xintercept = 2007.202, linetype = 3, color = 'darkgreen', size = 0.8) +
  geom_vline(xintercept = t_c, linetype = 'dashed', color = 'red', size = 1)








##### The last Bitcoin Bubble
getSymbols('^FTSE', env = .GlobalEnv, from = '2003-03-15', to = '2007-03-15', periodicity = 'daily')
GSPC <- FTSE$FTSE.Adjusted
GSPC <- GSPC[complete.cases(GSPC),]
plot(log(GSPC))

# Extracting time in decimal years
GSPC$t <- decimal_date(time(GSPC))
head(GSPC)

colnames(GSPC) <- c('price', 't')
head(GSPC)

# Generating linear parameters and phi, and creating Grid
grid <- list(a = seq(8, 10, by = 0.5),
             b = seq(-3.5, -0.5, by = 0.5),
             c = seq(0.02, 0.1, by = 0.02),
             Phi = seq(0, 2*pi, length.out = 4)) %>%
  cross_df()
grid$loss <- rep(as.numeric(NA), nrow(grid))
grid$t_c <- rep(as.numeric(NA), nrow(grid))
grid$m <- rep(as.numeric(NA), nrow(grid))
grid$omega <- rep(as.numeric(NA), nrow(grid))

# Executing GridSearch, Taboo Algoruthm and thus searching a global minima
new_grid <- apply(grid, 1, function(x) TabuSearch(A = x[1], B = x[2], C = x[3], phi = x[4]))
data <- data.frame(matrix(unlist(new_grid), nrow=length(new_grid), byrow=T))
colnames(data) <- c('A', 'B', 'C', 'phi', 'loss', 't_c', 'm', 'omega')
data <- data %>%
  arrange(loss)
data
write_csv(data, 'Taboo_results.csv')

# Preparing dataset for OLS, executing OLS and choosing the best parameters for out model
getSymbols('^FTSE', env = .GlobalEnv, from = '2003-03-15', to = '2009-06-30', periodicity = 'daily')
dataset <- FTSE$FTSE.Adjusted
dataset <- dataset[complete.cases(dataset),]

dataset$t <- decimal_date(time(dataset))
head(dataset)
tail(dataset)
colnames(dataset) <- c('price', 't')

SSR <- rep(NA, nrow(data))
for (i in 1:nrow(data)){
  t_c <- data$t_c[i]
  m <- data$m[i]
  omega <- data$omega[i]
  dataset$first_term <- (abs(t_c - dataset$t))^m
  dataset$second_term <- ((abs(t_c - dataset$t))^m) * cos(omega * log(abs(t_c - dataset$t)))
  dataset$third_terd <- ((abs(t_c - dataset$t))^m) * sin(omega * log(abs(t_c - dataset$t)))
  
  lmodel <- lm(log(price) ~ first_term + second_term + third_terd, data = dataset[1:nrow(GSPC),])
  SSR[i] <- sum((lmodel$residuals)^2)
}
# Choosing optimal parameters for our LPPL model
optimum <- which.min(SSR)
t_c <- data$t_c[optimum]
m <- data$m[optimum]
omega <- data$omega[optimum]
dataset$first_term <- (abs(t_c - dataset$t))^m
dataset$second_term <- ((abs(t_c - dataset$t))^m) * cos(omega * log(abs(t_c - dataset$t)))
dataset$third_terd <- ((abs(t_c - dataset$t))^m) * sin(omega * log(abs(t_c - dataset$t)))

LPPL <- lm(log(price) ~ first_term + second_term + third_terd, data = dataset[1:nrow(GSPC),])
summary(LPPL)
fitted <- LPPL$fitted.values
fitted <- append(fitted, predict(LPPL, newdata = dataset[1013:nrow(dataset),]))
dataset$fitted <- rep(NA, nrow(dataset))
dataset$fitted <- fitted

ggplot(data = dataset) +
  geom_line(aes(x = t, y = log(price)), size = 1) +
  geom_line(aes(x = t, y = fitted), colour = 'blue', size = 1) +
  geom_vline(xintercept = 2007.202, linetype = 3, color = 'darkgreen', size = 0.8) +
  geom_vline(xintercept = t_c, linetype = 'dashed', color = 'red', size = 1)


