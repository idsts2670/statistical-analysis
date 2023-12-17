######################
#
# Normal Data Analysis
#
######################

#install.packages("ISLR")
#install.packages("e1071")

library(ISLR)
library(e1071)

# Preparing the data
wage_data = Wage$wage


# Visualize the data 

hist(wage_data, breaks="fd",
     main="Wages for Males from the Central Atlantic Region of the USA",
     xlab="Thousands of Dollars")

hist(log(wage_data), breaks="fd",
     main="Log Wages for Males from the Central Atlantic Region of the USA",
     xlab="Log of Thousands of Dollars")

hist(wage_data[wage_data<200], breaks="fd",
     main="Wages for Selected Males from the Central Atlantic Region of the USA",
     xlab="Thousands of Dollars")

# We work with the above subset of wage data for the remaining analyses

wage_data = wage_data[wage_data<200]


# Proceeding with the Bayesian analysis    


# mu unknown, sigma known

mu_0 = 60
tau_0 = 5
sigma = 30


logprior = function(mu)
{  
  return(-(0.5/tau_0^2)*(mu-mu_0)^2)
}

loglikelihood = function(mu)
{
  y_bar = mean(wage_data)
  
  return(-(0.5*length(wage_data)/sigma^2)*(mu-y_bar)^2)
}

logposterior_unnormalized = function(mu)
{
  return(logprior(mu)+loglikelihood(mu))
}


mu_sequence = seq(40,140,0.01)

logprior_values = logprior(mu_sequence)
loglikelihood_values = loglikelihood(mu_sequence)
logposterior_values = logprior_values + loglikelihood_values

# Scale the unnormalized posterior to be similar to the likelihood
logposterior_values = logposterior_values - max(logposterior_values) + max(loglikelihood_values)

plot(mu_sequence, exp(logprior_values), ylim=c(0,exp(max(c(logprior_values, loglikelihood_values, logposterior_values)))),
     main="Unnormalized Prior, Likelihood, and Posterior for Wage Data", xlab=expression(mu), ylab="Density", type="l", col="red")
points(mu_sequence, exp(loglikelihood_values), type="l")
points(mu_sequence, exp(logposterior_values), type="l", col="blue") # This is the unnormalized posterior 


legend("topright",c("Prior", "Likelihood", "Posterior"), fill=c("red", "black", "blue"), horiz=FALSE)


# Zooming in on the posterior distribution

mu_sequence = seq(103,109,0.01)
logposterior_values = logposterior_unnormalized(mu_sequence)

# To make exponentiation more stable
logposterior_values = logposterior_values - max(logposterior_values)

plot(mu_sequence, exp(logposterior_values),
     main="Unnormalized Posterior for Wage Data", xlab=expression(mu), ylab="Unnormalized Density", type="l", col="blue")


# Using the formulas on slides,

60+(5^2/(5^2+(30^2/2901)))*(mean(wage_data)-60)   # Posterior mean
5^2 - (5^4)/(5^2+(30^2/2901))                     # Posterior variance
sqrt(5^2 - (5^4)/(5^2+(30^2/2901)))               # Posterior standard deviation


mu_sequence = seq(103,109,0.01)

posterior_values = dnorm(mu_sequence, 105.991, 0.5535658)

plot(mu_sequence, posterior_values,
     main=expression(paste("Posterior for Wage Data: N(105.991, "*0.55^2*")")), xlab=expression(mu), ylab="Density", type="l", col="blue")
abline(v=105.991)
abline(v=(105.991-0.5535658), lty=2)
abline(v=(105.991+0.5535658), lty=2)

# Posterior predictive checks of Normal model for wage data

y_bar = mean(wage_data)
s_sq = var(wage_data)

number_of_replicates = 20

replicated_data = matrix(NA, nrow=number_of_replicates, ncol=length(wage_data))
for(i in 1:nrow(replicated_data))
{
  posterior_sigma_sq_draw = (length(wage_data)-1)*s_sq/rchisq(1, (length(wage_data)-1))
  posterior_mu_draw = y_bar + sqrt(posterior_sigma_sq_draw/length(wage_data))*rnorm(1)
  replicated_data[i,] = posterior_mu_draw + sqrt(posterior_sigma_sq_draw)*rnorm(length(replicated_data[i,]))
}

par(mfrow=c(4,4))
par(mar = c(1,1,1,1))

for(i in 1:16)
{
  hist(replicated_data[i,], main="", xlab="", yaxt="n", ylab="", xlim=c(min(replicated_data), max(replicated_data)))
}

number_of_replicates = 1000

posterior_predictive_check_distribution = rep(NA, number_of_replicates)
for(i in 1:number_of_replicates)
{
  posterior_sigma_sq_draw = (length(wage_data)-1)*s_sq/rchisq(1, (length(wage_data)-1))
  posterior_mu_draw = y_bar + sqrt(posterior_sigma_sq_draw/length(wage_data))*rnorm(1)
  posterior_predictive_check_distribution[i] = skewness(posterior_mu_draw + sqrt(posterior_sigma_sq_draw)*rnorm(length(wage_data)))
}

par(mfrow=c(1,1))
hist(posterior_predictive_check_distribution, breaks="fd",
     main="Skewnesses from Posterior Predictive Simulated Wage Datasets", xlab="Skewness", freq=FALSE,
     xlim=c(min(c(posterior_predictive_check_distribution, skewness(wage_data))),
            max(c(posterior_predictive_check_distribution, skewness(wage_data)))))
abline(v=skewness(wage_data), lwd=3)
text(0.23, 8, "Observed skewness = 0.32")


# Another example: 
# Speed of light posterior predictive checks

speed_of_light_data = c(28, 26, 33, 24, 34, -44, 27, 16, 40, -2,
                        29, 22, 24, 21, 25, 30, 23, 29, 31, 19,
                        24, 20, 36, 32, 36, 28, 25, 21, 28, 29,
                        37, 25, 28, 26, 30, 32, 36, 26, 30, 22,
                        36, 23, 27, 27, 28, 27, 31, 27, 26, 33,
                        26, 32, 32, 24, 39, 28, 24, 25, 32, 25,
                        29, 27, 28, 29, 16, 23)

hist(speed_of_light_data, breaks=30,
     main="Speed of Light Measurements", xlab="Observed Nanoseconds - 24800 Nanoseconds", freq=FALSE)

y_bar = mean(speed_of_light_data)
s_sq = var(speed_of_light_data)

number_of_replicates = 20

replicated_data = matrix(NA, nrow=number_of_replicates, ncol=length(speed_of_light_data))
for(i in 1:nrow(replicated_data))
{
  posterior_sigma_sq_draw = (length(speed_of_light_data)-1)*s_sq/rchisq(1, (length(speed_of_light_data)-1))
  posterior_mu_draw = y_bar + sqrt(posterior_sigma_sq_draw/length(speed_of_light_data))*rnorm(1)
  replicated_data[i,] = posterior_mu_draw + sqrt(posterior_sigma_sq_draw)*rnorm(length(replicated_data[i,]))
}


par(mfrow=c(4,4))
for(i in 1:16)
{
  hist(replicated_data[i,], main="", xlab="", yaxt="n", ylab="", xlim=c(min(replicated_data), max(replicated_data)))
}

number_of_replicates = 1000

posterior_predictive_check_distribution = rep(NA, number_of_replicates)
for(i in 1:number_of_replicates)
{
  posterior_sigma_sq_draw = (length(speed_of_light_data)-1)*s_sq/rchisq(1, (length(speed_of_light_data)-1))
  posterior_mu_draw = y_bar + sqrt(posterior_sigma_sq_draw/length(speed_of_light_data))*rnorm(1)
  posterior_predictive_check_distribution[i] = min(posterior_mu_draw + sqrt(posterior_sigma_sq_draw)*rnorm(length(speed_of_light_data)))
}

par(mfrow=c(1,1))
hist(posterior_predictive_check_distribution, breaks="fd",
     main="Smallest Observations from Posterior Predictive Simulated Datasets", xlab="Observed Nanoseconds - 24800 Nanoseconds", freq=FALSE,
     xlim=c(min(speed_of_light_data), max(posterior_predictive_check_distribution)))
abline(v=min(speed_of_light_data), lwd=3)
text(-30, 0.04, "Minimum observed value = -44")


# mu and sigma unknown
# Reference prior on mu, sigma^2

log_posterior_unnormalized = function(mu,sigma_sq) {
  return(-log(sigma_sq) - (length(wage_data)/2)*log(sigma_sq) -(0.5/sigma_sq)*sum((wage_data-mu)^2))
}

mu_sequence = seq(104,109,0.1)
sigma_sq_sequence = seq(900,1030,0.1)

log_posterior_values = matrix(NA, nrow=length(mu_sequence), ncol=length(sigma_sq_sequence))
  
for(i in 1:length(mu_sequence)) {
  log_posterior_values[i,] = log_posterior_unnormalized(mu_sequence[i], sigma_sq_sequence)
}

posterior_values = exp(log_posterior_values - max(log_posterior_values))
posterior_values = posterior_values/(sum(posterior_values)*0.1*0.1) # This is the empirical normalized joint posterior

contour(mu_sequence, sigma_sq_sequence, posterior_values, 
        main=expression(paste("Joint Posterior of "*mu*" and "*sigma^2*" for Wage Data")), xlab=expression(mu), ylab=expression(sigma^2), col="blue")

# Can directly specify distribution on mu as a t-distribution
posterior_mu_values = rep(NA, length(mu_sequence))
for(i in 1:length(posterior_mu_values))
{
  posterior_mu_values[i] = dt(((mu_sequence[i]-mean(wage_data))/(sd(wage_data)/sqrt(length(wage_data)))), (length(wage_data)-1))/(sd(wage_data)/sqrt(length(wage_data)))
}

plot(mu_sequence, posterior_mu_values,
     main=expression(paste("Posterior of "*mu*" for Wage Data: "*t[2900]*"(106.56, "*0.58^2*")")), xlab=expression(mu), ylab="Density", 
     type="l", col="blue")

# An alternative, more empirical calculation of the marginal posterior folows below

posterior_mu_values_empirical = rowSums(posterior_values)*0.1

posterior_mu_draws = 106.56 + 0.58*rt(10^5, df=2900)

hist(posterior_mu_draws, breaks="fd", freq=FALSE,
     main=expression(paste("Posterior of "*mu*" for Wage Data: "*t[2900]*"(106.56, "*0.58^2*")")), xlab=expression(mu), ylab="Density")
points(mu_sequence, posterior_mu_values_empirical, type="l", col="blue")





######################
#
# Poisson Data Analysis
#
######################


galton_temp = read.csv(file="Galton.csv", header=TRUE)
family_index = unique(galton_temp$Family)
number_kids = 0

for(i in 1:length(family_index))
{
  number_kids = c(number_kids, 
                  (sum(galton_temp[galton_temp[,1]==family_index[i],6])/sum(galton_temp[,1]==family_index[i])))
}

number_kids = number_kids[-1]

# From a Google search of "average number of kids in uk during 1880", we believe that, on average, there were
# 6 kids per family in the UK during the time period of Galton
# This motivates our conjugate prior specification accordingly

logprior = function(lambda)
{  
  -lambda + 5*log(lambda)  # Ignore terms without lambda
}

loglikelihood = function(lambda)
{
  (sum(number_kids)*log(lambda) - length(number_kids)*lambda)
}

posterior_unnormalized = function(lambda)
{
  return(logprior(lambda) + loglikelihood(lambda))
}

lambda_sequence = seq(0.1,20,0.1)

logprior_values = logprior(lambda_sequence)
logprior_values = logprior_values - max(logprior_values)
loglikelihood_values = loglikelihood(lambda_sequence)
loglikelihood_values = loglikelihood_values - max(loglikelihood_values)
logposterior_values  = logprior_values + loglikelihood_values 

# This constant was chosen just so that the likelihood and posterior are more easily compared
logposterior_values = logposterior_values - max(logposterior_values) +
                                          max(loglikelihood_values)

plot(lambda_sequence, exp(logprior_values), ylim=c(0,exp(max(c(logprior_values, loglikelihood_values, logposterior_values)))),
     main="Unnormalized Prior, Likelihood, and Posterior for Count Data", xlab=expression(lambda), ylab="Density", type="l", col="red")
points(lambda_sequence, exp(loglikelihood_values), type="l")
points(lambda_sequence, exp(logposterior_values), type="l", col="blue")

legend("topright",c("Prior", "Likelihood", "Posterior"), fill=c("red", "black", "blue"), horiz=FALSE)


lambda_sequence = seq(4,5.15,0.01)
logposterior_values = logposterior_unnormalized(lambda_sequence)
logposterior_values = logposterior_values - max(logposterior_values)

plot(lambda_sequence, posterior_values,
     main="Unnormalized Posterior for Count Data", xlab=expression(lambda), ylab="Unnormalized Density", type="l", col="blue")


lambda_sequence = seq(4,5.15,0.01)
posterior_values = dgamma(lambda_sequence, shape=905, rate=198)

