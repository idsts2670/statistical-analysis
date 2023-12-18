###################################
#
# Multivariate Normal Data Analysis
#
###################################

# mu unknown, Sigma known

galton_temp = read.csv(file="Galton_height.csv", header=TRUE)

son_height = galton_temp[galton_temp[,4]=="M", 5]
father_height = galton_temp[galton_temp[,4]=="M", 2]
plot(son_height, father_height, xlab="Son Height", ylab="Father Height", main="Galton Measurements of Son and Father Heights")

# In all that follows, we assume the first entry in the mu vector corresponds to the son height, and the second entry corresponds to the father height

n = length(son_height)
mu_0 = matrix(c(65,65), ncol=1)    # This was obtained from the Google search "average height in uk during 1880"
Lambda_0 = matrix(c(2.5^2,3.125,3.125,2.5^2), nrow=2, ncol=2) # This was specified according to 65 +- 5 and rho = 0.5
Sigma = matrix(c(6, 2, 2, 5), nrow=2, ncol=2)   # For the purposes of this exercise, assume this is the actual value of Sigma. In fact, this number is the estimate from the observed data.

y_bar = matrix(c(mean(son_height), mean(father_height)), ncol=1)

prior = function(mu)
{  
  mu = matrix(mu, ncol=1)
  
  return(exp(-0.5*t(mu-mu_0)%*%solve(Lambda_0)%*%(mu-mu_0)))
}

likelihood = function(mu)
{
  mu = matrix(mu, ncol=1)

  return(exp(-0.5*n*t(mu-y_bar)%*%solve(Sigma)%*%(mu-y_bar)))
}

mu_sequence_1 = seq(59,80,0.05)
mu_sequence_2 = seq(59,80,0.05)

prior_values = matrix(NA, nrow=length(mu_sequence_1), ncol=length(mu_sequence_2))
likelihood_values = matrix(NA, nrow=length(mu_sequence_1), ncol=length(mu_sequence_2))

for(i in 1:length(mu_sequence_1))
{
  for(j in 1:length(mu_sequence_2))
  {
    prior_values[i,j] = prior(c(mu_sequence_1[i], mu_sequence_2[j]))
    likelihood_values[i,j] = likelihood(c(mu_sequence_1[i], mu_sequence_2[j]))
  }
}

likelihood_values = likelihood_values/max(likelihood_values)

contour(mu_sequence_1, mu_sequence_2, prior_values,     
        main="Unnormalized Prior and Likelihood for Height Data", xlab=expression(mu[1]), ylab=expression(mu[2]), 
        col="red", xlim=c(60, 71), ylim=c(60, 71))

legend("topright",c("Prior", "Likelihood"), fill=c("red", "black"), horiz=FALSE)


contour(mu_sequence_1, mu_sequence_2, prior_values,     
        main="Unnormalized Prior and Likelihood for Height Data", xlab=expression(mu[1]), ylab=expression(mu[2]), 
        col="red", xlim=c(60, 71), ylim=c(60, 71))

contour(mu_sequence_1, mu_sequence_2, likelihood_values,     
        main="Unnormalized Prior, Likelihood, and Posterior for Height Data", xlab=expression(mu[1]), ylab=expression(mu[2]), 
        col="black", add=TRUE)

legend("topright",c("Prior", "Likelihood"), fill=c("red", "black"), horiz=FALSE)


contour(mu_sequence_1, mu_sequence_2, likelihood_values,     
        main="Likelihood for Height Data", xlab=expression(mu[1]), ylab=expression(mu[2]), 
        col="black", xlim=c(68.7, 69.6), ylim=c(68.7, 69.6))


# Getting parameters for Monte Carlo approximation to the posterior distribution under conjugate prior

posterior_mean = mu_0 + Lambda_0%*%solve(Lambda_0 + Sigma/length(son_height))%*%(y_bar - mu_0)
posterior_cov = Lambda_0 - Lambda_0%*%solve(Lambda_0 + Sigma/length(son_height))%*%Lambda_0

number_draws = 10^5

posterior_mu_draws = matrix(posterior_mean, nrow=2, ncol=number_draws) + t(chol(posterior_cov))%*%matrix(rnorm((2*number_draws)), nrow=2, ncol=number_draws)

plot(posterior_mu_draws[1,], posterior_mu_draws[2,], pch=".", col="blue",
     xlab=expression(mu[1]), ylab=expression(mu[2]), main=expression(paste(10^5*" Posterior Draws of "*mu)))


# Getting parameters for Monte Carlo approximation to the posterior distribution under reference prior

posterior_mean = y_bar
posterior_cov = Sigma/length(son_height)

number_draws = 10^5

posterior_mu_draws = matrix(posterior_mean, nrow=2, ncol=number_draws) + t(chol(posterior_cov))%*%matrix(rnorm((2*number_draws)), nrow=2, ncol=number_draws)

plot(posterior_mu_draws[1,], posterior_mu_draws[2,], pch=".", col="blue",
     xlab=expression(mu[1]), ylab=expression(mu[2]), main=expression(paste(10^5*" Posterior Draws of "*mu)))


# mu unknown, Sigma unknown

posterior_mean = y_bar

S = cov(cbind(son_height, father_height))*(n-1)

number_draws = 10^5

posterior_mu_draws = matrix(NA, nrow=number_draws, ncol=2)

checking_draws = matrix(0, nrow=2, ncol=2)

progress_bar = txtProgressBar(min=1, max=number_draws, style = 3)
for(i in 1:number_draws)
{
  Wishart_draw = matrix(rWishart(1, (n-1), diag(2)), nrow=2, ncol=2)
    
  Sigma_draw = t(chol(S))%*%solve(Wishart_draw)%*%chol(S)
  
  checking_draws = checking_draws + Sigma_draw
  
  posterior_mu_draws[i,] = y_bar + t(chol(Sigma_draw/n))%*%matrix(rnorm(2), ncol=1)
  setTxtProgressBar(progress_bar, i)
}
close(progress_bar)

checking_draws/number_draws # Empirical posterior mean of Sigma
S/(n-4)                     # True posterior mean of Sigma


plot(posterior_mu_draws[,1], posterior_mu_draws[,2], pch=".", col="blue",
     xlab=expression(mu[1]), ylab=expression(mu[2]), main=expression(paste(10^5*" Posterior Draws of "*mu)))