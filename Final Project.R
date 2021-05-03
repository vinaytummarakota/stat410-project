setwd('C:/Users/Srivinay Tummarakota/Documents/Vinay\'s Docs/Rice Sophomore/STAT 410/Final Project/')
data <- read.csv('cleaned_data.csv', header=TRUE)

# Model 1 (Simpson's DI)
sdi_fit <- lm(Pct5Plus ~ Bachelor+Poverty+SimpsonsDI+VacantHousing+OwnerOccupied+OldHousing, data=data)
summary(sdi_fit)
plot(sdi_fit)

# Model 2 (% Black)

pctblack_fit <- lm(Pct5Plus ~ Bachelor+Poverty+Black+VacantHousing+OwnerOccupied+OldHousing, data=data)
summary(pctblack_fit)
plot(pctblack_fit)

# Model 3 (Race-Poverty Interaction)

racepov_fit <- lm(Pct5Plus ~ Bachelor+Poverty+Black+Black*Poverty+VacantHousing+OwnerOccupied+OldHousing, data=data)
summary(racepov_fit)
plot(racepov_fit)

# Model 4 (Bootstrapped Simpson's DI)

B = 1000 # Number of resamples
beta_b = matrix(0, nrow = B, ncol = 7)
n = length(data$Pct5Plus)
for(b in 1:B){
  # Resample the data:
  boot = sample(1:n, n, replace = TRUE) 
  data_b = data[boot, ]
  fit <- lm(Pct5Plus ~ Bachelor+Poverty+SimpsonsDI+VacantHousing+OwnerOccupied+OldHousing, data=data_b)
  beta_b[b, ] = coef(fit)
}
covariates_sdi <- c('Bachelor', 'Poverty', 'SimpsonsDI', 'VacantHousing', 'OwnerOccupied', 'OldHousing')
beta_sdi <- as.vector(apply(beta_b, 2, mean)) # calculate coefficients
conf_sdi <- apply(beta_b, 2, function(x) quantile(x, c(0.025, 0.975))) # calculate CI's
x_sdi <- as.matrix(cbind(rep(1, n), data[covariates_sdi])) # subset data
y_hat_sdi <- x_sdi%*%beta_sdi # calculate fitted values
res_sdi <- data$Pct5Plus - y_hat_sdi # calculate residuals
sdres_sdi <- scale(res_sdi) # calculate standardized residuals

plot(x=y_hat_sdi, y=res_sdi, xlab="Fitted Values", ylab="Residuals") # Residuals vs Fitted Values
qqnorm(sdres_sdi) # Normality Plot
qqline(sdres_sdi)


# Model 5 (Bootstrapped % Black)

B = 1000 # Number of resamples
beta_b = matrix(0, nrow = B, ncol = 7)
n = length(data$Pct5Plus)
for(b in 1:B){
  # Resample the data:
  boot = sample(1:n, n, replace = TRUE) 
  data_b = data[boot, ]
  fit <- lm(Pct5Plus ~ Bachelor+Poverty+Black+VacantHousing+OwnerOccupied+OldHousing, data=data_b)
  beta_b[b, ] = coef(fit)
}
covariates_blk <- c('Bachelor', 'Poverty', 'Black', 'VacantHousing', 'OwnerOccupied', 'OldHousing')
beta_blk <- as.vector(apply(beta_b, 2, mean)) # calculate coefficients
conf_blk <- apply(beta_b, 2, function(x) quantile(x, c(0.025, 0.975))) # calculate CI's
x_blk <- as.matrix(cbind(rep(1, n), data[covariates_blk])) # subset data
y_hat_blk <- x_blk%*%beta_blk # calculate fitted values
res_blk <- data$Pct5Plus - y_hat_blk # calculate residuals
sdres_blk <- scale(res_blk) # calculate standardized residuals

plot(x=y_hat_blk, y=res_blk, xlab="Fitted Values", ylab="Residuals") # Residuals vs Fitted Values
qqnorm(sdres_blk) # Normality Plot
qqline(sdres_blk)

# Model 6 (Bootstrapped Race-Poverty Interaction)

B = 1000 # Number of resamples
beta_b = matrix(0, nrow = B, ncol = 8)
n = length(data$Pct5Plus)
for(b in 1:B){
  # Resample the data:
  boot = sample(1:n, n, replace = TRUE) 
  data_b = data[boot, ]
  fit <- lm(Pct5Plus ~ Bachelor+Poverty+Black+Black*Poverty+VacantHousing+OwnerOccupied+OldHousing, data=data_b)
  beta_b[b, ] = coef(fit)
}
covariates_pov <- c('Bachelor', 'Poverty', 'Black', 'VacantHousing', 'OwnerOccupied', 'OldHousing')
beta_pov <- as.vector(apply(beta_b, 2, mean)) # calculate coefficients
conf_pov <- apply(beta_b, 2, function(x) quantile(x, c(0.025, 0.975))) # calculate CI's
x_pov <- as.matrix(cbind(rep(1, n), data[covariates_pov], data['Black']*data['Poverty'])) # subset data
y_hat_pov <- x_pov%*%beta_pov # calculate fitted values
res_pov <- data$Pct5Plus - y_hat_pov # calculate residuals
sdres_pov <- scale(res_pov) # calculate standardized residuals

plot(x=y_hat_pov, y=res_pov, xlab="Fitted Values", ylab="Residuals") # Residuals vs Fitted Values
qqnorm(sdres_pov) # Normality Plot
qqline(sdres_pov)

# Model 7 (Log-Transformed Simpson's DI)

logsdi_fit <- lm(log(Pct5Plus+1) ~ Bachelor+Poverty+SimpsonsDI+VacantHousing+OwnerOccupied+OldHousing, data=data)
summary(logsdi_fit)
plot(logsdi_fit)

# Model 8 (Log-Transformed Percent Black)

logblk_fit <- lm(log(Pct5Plus+1) ~ Bachelor+Poverty+Black+VacantHousing+OwnerOccupied+OldHousing, data=data)
summary(logblk_fit)
plot(logblk_fit)

# Model 9 (Log-Transformed Race-Poverty Interaction)

logpov_fit <- lm(log(Pct5Plus+1) ~ Bachelor+Poverty+Black+Black*Poverty+VacantHousing+OwnerOccupied+OldHousing, data=data)
summary(logpov_fit)
plot(logpov_fit)


