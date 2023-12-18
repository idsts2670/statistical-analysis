##########################################
#
# Ordinary Linear Regression Data Analysis
#
##########################################

advertising_data = read.csv(file="Advertising.csv", header=TRUE)
advertising_data = advertising_data[,2:5]

plot(advertising_data)


# Following Gelman's advice, let's consider the log of Sales
# Also, let's transform the predictor variables,
# so that they are centered at zero and have unit variance

advertising_data$Sales = log(advertising_data$Sales)
advertising_data$TV = (advertising_data$TV - mean(advertising_data$TV))/sd(advertising_data$TV)
advertising_data$Radio = (advertising_data$Radio - mean(advertising_data$Radio))/sd(advertising_data$Radio)
advertising_data$Newspaper = (advertising_data$Newspaper - mean(advertising_data$Newspaper))/sd(advertising_data$Newspaper)

plot(advertising_data)


# Initial main effects model and posterior distributions

X = cbind(rep(1, nrow(advertising_data)), advertising_data$TV, advertising_data$Radio, advertising_data$Newspaper)

beta_hat = solve(t(X)%*%X)%*%t(X)%*%advertising_data$Sales
beta_cov = solve(t(X)%*%X)

n = nrow(advertising_data)
k = length(beta_hat)
sigma_sq_df = n-k
s_sq = sum((advertising_data$Sales-X%*%beta_hat)^2)/sigma_sq_df

number_draws = 10^4

posterior_draws_sigma_sq = sigma_sq_df*s_sq/rchisq(number_draws, df=sigma_sq_df)

posterior_draws_beta = matrix(NA, nrow=length(beta_hat), ncol=number_draws)
for(i in 1:number_draws)
{
  posterior_draws_beta[,i] = beta_hat + sqrt(posterior_draws_sigma_sq[i])*t(chol(beta_cov))%*%matrix(rnorm(length(beta_hat)), ncol=1)
}


par(mfrow=c(2,2))
hist(posterior_draws_beta[1,], xlab=expression(beta[0]), main=expression(paste("Posterior of "*beta[0]*" for Initial Main Effects Model")), freq=FALSE, breaks="fd")
abline(v=quantile(posterior_draws_beta[1,], probs=c(0.025, 0.5, 0.975)), lty=2)
hist(posterior_draws_beta[2,], xlab=expression(beta[TV]), main=expression(paste("Posterior of "*beta[TV])), freq=FALSE, breaks="fd")
abline(v=quantile(posterior_draws_beta[2,], probs=c(0.025, 0.5, 0.975)), lty=2)
hist(posterior_draws_beta[3,], xlab=expression(beta[Radio]), main=expression(paste("Posterior of "*beta[Radio])), freq=FALSE, breaks="fd")
abline(v=quantile(posterior_draws_beta[3,], probs=c(0.025, 0.5, 0.975)), lty=2)
hist(posterior_draws_beta[4,], xlab=expression(beta[Newspaper]), main=expression(paste("Posterior of "*beta[Newspaper])), freq=FALSE, breaks="fd")
abline(v=quantile(posterior_draws_beta[4,], probs=c(0.025, 0.5, 0.975)), lty=2)

par(mfrow=c(1,1))
hist(posterior_draws_sigma_sq, xlab=expression(sigma^2), main=expression(paste("Posterior of "*sigma^2*" for Initial Main Effects Model")), freq=FALSE, breaks="fd")
abline(v=quantile(posterior_draws_sigma_sq, probs=c(0.025, 0.5, 0.975)), lty=2)


# Checking with classical linear regression

# main_effects_model = lm(Sales~TV+Radio+Newspaper, data=advertising_data)
# summary(main_effects_model)
# beta_hat = matrix(as.vector(main_effects_model$coeff), ncol=1)

summary(lm(Sales~TV+Radio+Newspaper, data=advertising_data))
sd(posterior_draws_beta[1,])
sd(posterior_draws_beta[2,])
sd(posterior_draws_beta[3,])
sd(posterior_draws_beta[4,])

# Performing QR factorization

QR_factorization_X = qr(X)
Q = qr.Q(QR_factorization_X)
R = qr.R(QR_factorization_X)
max(abs(X - Q%*%R))
max(abs(t(R)%*%R - t(X)%*%X))


# Second main effects model, removing Newspaper from consideration

X = cbind(rep(1, nrow(advertising_data)), advertising_data$TV, advertising_data$Radio)

beta_hat = solve(t(X)%*%X)%*%t(X)%*%advertising_data$Sales
beta_cov = solve(t(X)%*%X)

n = nrow(advertising_data)
k = length(beta_hat)
sigma_sq_df = n-k
s_sq = sum((advertising_data$Sales-X%*%beta_hat)^2)/sigma_sq_df

number_draws = 10^4

posterior_draws_sigma_sq = sigma_sq_df*s_sq/rchisq(number_draws, df=sigma_sq_df)

posterior_draws_beta = matrix(NA, nrow=length(beta_hat), ncol=number_draws)
PPD_predictions = matrix(NA, nrow=nrow(X), ncol=number_draws)
PPD_residuals = matrix(NA, nrow=nrow(X), ncol=number_draws)
for(i in 1:number_draws)
{
  posterior_draws_beta[,i] = beta_hat + sqrt(posterior_draws_sigma_sq[i])*t(chol(beta_cov))%*%matrix(rnorm(length(beta_hat)), ncol=1)
  PPD_predictions[,i] = X%*%posterior_draws_beta[,i] + sqrt(posterior_draws_sigma_sq[i])*matrix(rnorm(nrow(X)), ncol=1)
  PPD_residuals[,i] = (advertising_data$Sales - PPD_predictions[,i])/sqrt(posterior_draws_sigma_sq[i])
}

posterior_expectation = X%*%beta_hat

PPD_predictions_mean = rowMeans(PPD_predictions)
PPD_residuals_mean = rowMeans(PPD_residuals)

PPD_predictions_quantiles = matrix(NA, nrow=nrow(X), ncol=2)
PPD_residuals_quantiles = matrix(NA, nrow=nrow(X), ncol=2)
for(i in 1:nrow(X))
{
  PPD_predictions_quantiles[i,] = quantile(PPD_predictions[i,], probs=c(0.025, 0.975))
  PPD_residuals_quantiles[i,] = quantile(PPD_residuals[i,], probs=c(0.025, 0.975))
}


par(mfrow=c(2,2))
hist(posterior_draws_beta[1,], xlab=expression(beta[0]), main=expression(paste("Posterior of "*beta[0]*" for Main Effects Model")), freq=FALSE, breaks="fd")
abline(v=quantile(posterior_draws_beta[1,], probs=c(0.025, 0.5, 0.975)), lty=2)
hist(posterior_draws_beta[2,], xlab=expression(beta[TV]), main=expression(paste("Posterior of "*beta[TV])), freq=FALSE, breaks="fd")
abline(v=quantile(posterior_draws_beta[2,], probs=c(0.025, 0.5, 0.975)), lty=2)
hist(posterior_draws_beta[3,], xlab=expression(beta[Radio]), main=expression(paste("Posterior of "*beta[Radio])), freq=FALSE, breaks="fd")
abline(v=quantile(posterior_draws_beta[3,], probs=c(0.025, 0.5, 0.975)), lty=2)
hist(posterior_draws_sigma_sq, xlab=expression(sigma^2), main=expression(paste("Posterior of "*sigma^2)), freq=FALSE, breaks="fd")
abline(v=quantile(posterior_draws_sigma_sq, probs=c(0.025, 0.5, 0.975)), lty=2)

par(mfrow=c(1,1))
plot(posterior_expectation[order(posterior_expectation)], PPD_predictions_mean[order(posterior_expectation)], type="l", 
     main="Observed and Posterior Predictive Distributions of log(Sales)", xlab="Weighted Combination of Explanatory Variables", ylab="log(Sales)")
points(posterior_expectation[order(posterior_expectation)], advertising_data$Sales[order(posterior_expectation)])
points(posterior_expectation[order(posterior_expectation)], PPD_predictions_quantiles[order(posterior_expectation), 1], type="l", lty=2)
points(posterior_expectation[order(posterior_expectation)], PPD_predictions_quantiles[order(posterior_expectation), 2], type="l", lty=2)

par(mfrow=c(1,1))
plot(posterior_expectation[order(posterior_expectation)], PPD_residuals_mean[order(posterior_expectation)], pch=20,
     main="Posterior Predictive Distributions of Standardized Residuals", 
     xlab="Weighted Combination of Explanatory Variables", ylab="Standardized Residual",
     ylim=c(min(PPD_residuals_quantiles[order(posterior_expectation), 1]), max(PPD_residuals_quantiles[order(posterior_expectation), 2])))
#abline(h=3*sqrt(s_sq), lty=2)
#abline(h=-3*sqrt(s_sq), lty=2)
abline(h=0)
points(posterior_expectation[order(posterior_expectation)], PPD_residuals_quantiles[order(posterior_expectation), 1], pch="-", cex=1.5)
points(posterior_expectation[order(posterior_expectation)], PPD_residuals_quantiles[order(posterior_expectation), 2], pch="-", cex=1.5)


# Next model, considering interaction of TV and Radio

X = cbind(rep(1, nrow(advertising_data)), advertising_data$TV, advertising_data$Radio, advertising_data$TV*advertising_data$Radio)

beta_hat = solve(t(X)%*%X)%*%t(X)%*%advertising_data$Sales
beta_cov = solve(t(X)%*%X)

n = nrow(advertising_data)
k = length(beta_hat)
sigma_sq_df = n-k
s_sq = sum((advertising_data$Sales-X%*%beta_hat)^2)/sigma_sq_df

number_draws = 10^4

posterior_draws_sigma_sq = sigma_sq_df*s_sq/rchisq(number_draws, df=sigma_sq_df)

posterior_draws_beta = matrix(NA, nrow=length(beta_hat), ncol=number_draws)
PPD_predictions = matrix(NA, nrow=nrow(X), ncol=number_draws)
PPD_residuals = matrix(NA, nrow=nrow(X), ncol=number_draws)
for(i in 1:number_draws)
{
  posterior_draws_beta[,i] = beta_hat + sqrt(posterior_draws_sigma_sq[i])*t(chol(beta_cov))%*%matrix(rnorm(length(beta_hat)), ncol=1)
  PPD_predictions[,i] = X%*%posterior_draws_beta[,i] + sqrt(posterior_draws_sigma_sq[i])*matrix(rnorm(nrow(X)), ncol=1)
  PPD_residuals[,i] = (advertising_data$Sales - PPD_predictions[,i])/sqrt(posterior_draws_sigma_sq[i])
}

posterior_expectation = X%*%beta_hat

PPD_predictions_mean = rowMeans(PPD_predictions)
PPD_residuals_mean = rowMeans(PPD_residuals)

PPD_predictions_quantiles = matrix(NA, nrow=nrow(X), ncol=2)
PPD_residuals_quantiles = matrix(NA, nrow=nrow(X), ncol=2)
for(i in 1:nrow(X))
{
  PPD_predictions_quantiles[i,] = quantile(PPD_predictions[i,], probs=c(0.025, 0.975))
  PPD_residuals_quantiles[i,] = quantile(PPD_residuals[i,], probs=c(0.025, 0.975))
}


par(mfrow=c(2,2))
hist(posterior_draws_beta[1,], xlab=expression(beta[0]), main=expression(paste("Posterior of "*beta[0]*" for Model with Interaction")), freq=FALSE, breaks="fd")
abline(v=quantile(posterior_draws_beta[1,], probs=c(0.025, 0.5, 0.975)), lty=2)
hist(posterior_draws_beta[2,], xlab=expression(beta[TV]), main=expression(paste("Posterior of "*beta[TV])), freq=FALSE, breaks="fd")
abline(v=quantile(posterior_draws_beta[2,], probs=c(0.025, 0.5, 0.975)), lty=2)
hist(posterior_draws_beta[3,], xlab=expression(beta[Radio]), main=expression(paste("Posterior of "*beta[Radio])), freq=FALSE, breaks="fd")
abline(v=quantile(posterior_draws_beta[3,], probs=c(0.025, 0.5, 0.975)), lty=2)
hist(posterior_draws_beta[4,], xlab=expression(beta[TV*"x"*Radio]), main=expression(paste("Posterior of "*beta[TV*"x"*Radio])), freq=FALSE, breaks="fd")
abline(v=quantile(posterior_draws_beta[4,], probs=c(0.025, 0.5, 0.975)), lty=2)

par(mfrow=c(1,1))
hist(posterior_draws_sigma_sq, xlab=expression(sigma^2), main=expression(paste("Posterior of "*sigma^2)), freq=FALSE, breaks="fd")
abline(v=quantile(posterior_draws_sigma_sq, probs=c(0.025, 0.5, 0.975)), lty=2)

par(mfrow=c(1,1))
plot(posterior_expectation[order(posterior_expectation)], PPD_predictions_mean[order(posterior_expectation)], type="l",
     main="Observed and Posterior Predictive Distributions of log(Sales)", xlab="Weighted Combination of Explanatory Variables", ylab="log(Sales)")
points(posterior_expectation[order(posterior_expectation)], advertising_data$Sales[order(posterior_expectation)])
points(posterior_expectation[order(posterior_expectation)], PPD_predictions_quantiles[order(posterior_expectation), 1], type="l", lty=2)
points(posterior_expectation[order(posterior_expectation)], PPD_predictions_quantiles[order(posterior_expectation), 2], type="l", lty=2)

par(mfrow=c(1,1))
plot(posterior_expectation[order(posterior_expectation)], PPD_residuals_mean[order(posterior_expectation)], pch=20,
     main="Posterior Predictive Distributions of Standardized Residuals", 
     xlab="Weighted Combination of Explanatory Variables", ylab="Standardized Residual",
     ylim=c(min(PPD_residuals_quantiles[order(posterior_expectation), 1]), max(PPD_residuals_quantiles[order(posterior_expectation), 2])))
#abline(h=3*sqrt(s_sq), lty=2)
#abline(h=-3*sqrt(s_sq), lty=2)
abline(h=0, lty=2)
points(posterior_expectation[order(posterior_expectation)], PPD_residuals_quantiles[order(posterior_expectation), 1], pch="-", cex=1.5)
points(posterior_expectation[order(posterior_expectation)], PPD_residuals_quantiles[order(posterior_expectation), 2], pch="-", cex=1.5)


# Final model, considering interaction of TV and Radio, and Sales in the original scale

advertising_data$Sales = exp(advertising_data$Sales)

X = cbind(rep(1, nrow(advertising_data)), advertising_data$TV, advertising_data$Radio, advertising_data$TV*advertising_data$Radio)

beta_hat = solve(t(X)%*%X)%*%t(X)%*%advertising_data$Sales
beta_cov = solve(t(X)%*%X)

n = nrow(advertising_data)
k = length(beta_hat)
sigma_sq_df = n-k
s_sq = sum((advertising_data$Sales-X%*%beta_hat)^2)/sigma_sq_df

number_draws = 10^4

posterior_draws_sigma_sq = sigma_sq_df*s_sq/rchisq(number_draws, df=sigma_sq_df)

posterior_draws_beta = matrix(NA, nrow=length(beta_hat), ncol=number_draws)
PPD_predictions = matrix(NA, nrow=nrow(X), ncol=number_draws)
PPD_residuals = matrix(NA, nrow=nrow(X), ncol=number_draws)
for(i in 1:number_draws)
{
  posterior_draws_beta[,i] = beta_hat + sqrt(posterior_draws_sigma_sq[i])*t(chol(beta_cov))%*%matrix(rnorm(length(beta_hat)), ncol=1)
  PPD_predictions[,i] = X%*%posterior_draws_beta[,i] + sqrt(posterior_draws_sigma_sq[i])*matrix(rnorm(nrow(X)), ncol=1)
  PPD_residuals[,i] = (advertising_data$Sales - PPD_predictions[,i])/posterior_draws_sigma_sq[i]
}

posterior_expectation = X%*%beta_hat

PPD_predictions_mean = rowMeans(PPD_predictions)
PPD_residuals_mean = rowMeans(PPD_residuals)

PPD_predictions_quantiles = matrix(NA, nrow=nrow(X), ncol=2)
PPD_residuals_quantiles = matrix(NA, nrow=nrow(X), ncol=2)
for(i in 1:nrow(X))
{
  PPD_predictions_quantiles[i,] = quantile(PPD_predictions[i,], probs=c(0.025, 0.975))
  PPD_residuals_quantiles[i,] = quantile(PPD_residuals[i,], probs=c(0.025, 0.975))
}


par(mfrow=c(2,2))
hist(posterior_draws_beta[1,], xlab=expression(beta[0]), main=expression(paste("Posterior of "*beta[0]*" for Model with Interaction")), freq=FALSE, breaks="fd")
abline(v=quantile(posterior_draws_beta[1,], probs=c(0.025, 0.5, 0.975)), lty=2)
hist(posterior_draws_beta[2,], xlab=expression(beta[TV]), main=expression(paste("Posterior of "*beta[TV])), freq=FALSE, breaks="fd")
abline(v=quantile(posterior_draws_beta[2,], probs=c(0.025, 0.5, 0.975)), lty=2)
hist(posterior_draws_beta[3,], xlab=expression(beta[Radio]), main=expression(paste("Posterior of "*beta[Radio])), freq=FALSE, breaks="fd")
abline(v=quantile(posterior_draws_beta[3,], probs=c(0.025, 0.5, 0.975)), lty=2)
hist(posterior_draws_beta[4,], xlab=expression(beta[TV*"x"*Radio]), main=expression(paste("Posterior of "*beta[TV*"x"*Radio])), freq=FALSE, breaks="fd")
abline(v=quantile(posterior_draws_beta[4,], probs=c(0.025, 0.5, 0.975)), lty=2)

par(mfrow=c(1,1))
hist(posterior_draws_sigma_sq, xlab=expression(sigma^2), main=expression(paste("Posterior of "*sigma^2)), freq=FALSE, breaks="fd")
abline(v=quantile(posterior_draws_sigma_sq, probs=c(0.025, 0.5, 0.975)), lty=2)

par(mfrow=c(1,1))
plot(posterior_expectation[order(posterior_expectation)], PPD_predictions_mean[order(posterior_expectation)], type="l",
     main="Observed and Posterior Predictive Distributions of Sales", xlab="Weighted Combination of Explanatory Variables", ylab="Sales")
points(posterior_expectation[order(posterior_expectation)], advertising_data$Sales[order(posterior_expectation)])
points(posterior_expectation[order(posterior_expectation)], PPD_predictions_quantiles[order(posterior_expectation), 1], type="l", lty=2)
points(posterior_expectation[order(posterior_expectation)], PPD_predictions_quantiles[order(posterior_expectation), 2], type="l", lty=2)

par(mfrow=c(1,1))
plot(posterior_expectation[order(posterior_expectation)], PPD_residuals_mean[order(posterior_expectation)], pch=20,
     main="Posterior Predictive Distributions of Standardized Residuals", 
     xlab="Weighted Combination of Explanatory Variables", ylab="Standardized Residual", 
     ylim=c(min(PPD_residuals_quantiles[order(posterior_expectation), 1]), max(PPD_residuals_quantiles[order(posterior_expectation), 2])))
#abline(h=3*sqrt(s_sq), lty=2)
#abline(h=-3*sqrt(s_sq), lty=2)
abline(h=0, lty=2)
points(posterior_expectation[order(posterior_expectation)], PPD_residuals_quantiles[order(posterior_expectation), 1], pch="-", cex=1.5)
points(posterior_expectation[order(posterior_expectation)], PPD_residuals_quantiles[order(posterior_expectation), 2], pch="-", cex=1.5)


# Checking lm output

lm_original_scale = lm(Sales~TV*Radio, data=advertising_data)
summary(lm_original_scale)
summary(lm_original_scale)$r.squared


advertising_data$Sales = log(advertising_data$Sales)
lm_log_scale = lm(Sales~TV*Radio, data=advertising_data)
summary(lm_log_scale)
summary(lm_log_scale)$r.squared

# It appears that keeping Sales on the original scale is better for prediction!