NBA_data = read.csv("NBA_data.csv")
NBA_data = NBA_data[,-1]

# We first prepare the data in a general format

n_j = NBA_data[,1]
y_bar = NBA_data[,2]
sigma_j_sq = NBA_data[,3]^2/n_j

J = length(y_bar)

# total_n = n_j + NBA_data[,4]
# final_average = (n_j*y_bar + NBA_data[,4]*NBA_data[,5])/total_n


# For reference, the data for the eight schools example in BDA follow below

# y_bar = c(28,8,-3,7,-1,1,18,12)
# sigma_j_sq = c(15^2, 10^2, 16^2, 11^2, 9^2, 11^2, 10^2, 18^2)


# We now define the functions that will be used to obtain posterior draws of the parameters

log_posterior_tau_sq = function(tau_sq)
{
  # Note that this is the log posterior under the prior p(tau_sq) \propto 1/tau
  
  mu_hat = sum(y_bar/(tau_sq+sigma_j_sq))/sum(1/(tau_sq+sigma_j_sq))
  
  tau = sqrt(tau_sq)
  
  log_prior_term = -log(tau)
  
  first_term = -0.5*sum(log(tau_sq + sigma_j_sq)) - 0.5*log(sum(1/(tau_sq+sigma_j_sq)))
  second_term = -0.5*sum((y_bar-mu_hat)^2/(tau_sq+sigma_j_sq))
  
  return(log_prior_term + first_term + second_term)
}


log_posterior_tau = function(tau)
{
  # Note that this is the log posterior under the prior p(tau_sq) \propto 1/tau

  log_jacobian_term = log(tau)
  
  tau_sq = tau^2
  
  return(log_posterior_tau_sq(tau_sq) + log_jacobian_term)
}


tau_sq_seq = seq(0.001,50,0.001)

log_posterior_tau_sq_evaluations = sapply(tau_sq_seq, log_posterior_tau_sq) - max(sapply(tau_sq_seq, log_posterior_tau_sq))
posterior_tau_sq_evaluations = exp(log_posterior_tau_sq_evaluations)/(sum(exp(log_posterior_tau_sq_evaluations))*0.001)

plot(tau_sq_seq, posterior_tau_sq_evaluations, type="l",
     xlab=expression(tau^2), ylab="Density", main=expression(paste("Marginal Posterior of "*tau^2)))

tau_seq = seq(0.001,10,0.001)

log_posterior_tau_evaluations = sapply(tau_seq, log_posterior_tau) - max(sapply(tau_seq, log_posterior_tau))
posterior_tau_evaluations = exp(log_posterior_tau_evaluations)/(sum(exp(log_posterior_tau_evaluations))*0.001)

plot(tau_seq, posterior_tau_evaluations, type="l",
     xlab=expression(tau), ylab="Density", main=expression(paste("Marginal Posterior of "*tau)))


conditional_mean_theta = function(tau, j)
{
  tau_sq = tau^2
  
  posterior_mean_mu = sum(y_bar/(tau_sq+sigma_j_sq))/sum(1/(tau_sq+sigma_j_sq))
  
  first_term = sigma_j_sq[j]^(-1)*y_bar[j]/(tau_sq^(-1)+sigma_j_sq[j]^(-1))
  second_term = tau_sq^(-1)*posterior_mean_mu/(tau_sq^(-1)+sigma_j_sq[j]^(-1))
  
  return(first_term + second_term)
}

tau_seq = seq(0.01,8,0.01)

conditional_mean_theta_evaluations = matrix(NA, nrow=length(y_bar), ncol=length(tau_seq))

for(j in 1:nrow(conditional_mean_theta_evaluations))
{
  for(i in 1:ncol(conditional_mean_theta_evaluations))
  {
    conditional_mean_theta_evaluations[j,i] = conditional_mean_theta(tau_seq[i], j)
  }
}

plot(tau_seq, conditional_mean_theta_evaluations[1,], type="l", 
     ylim=c(min(conditional_mean_theta_evaluations),max(conditional_mean_theta_evaluations)),
     xlab=expression(tau), ylab="Points",
     main=expression(paste("Posterior Expectations of "*theta[j]*" Conditional on "*tau)))
for(j in 1:length(y_bar))
{
  points(tau_seq, conditional_mean_theta_evaluations[j,], type="l")
}

NBA_data[order(NBA_data[,2], decreasing=TRUE),]    # To identify lines in plot with actual teams


conditional_sd_theta = function(tau, j)
{
  tau_sq = tau^2
  
  posterior_var_mu = 1/sum(1/(tau_sq+sigma_j_sq))
  
  first_term = tau_sq^(-2)*posterior_var_mu/((tau_sq^(-1)+sigma_j_sq[j]^(-1))^2)
  second_term = 1/(tau_sq^(-1)+sigma_j_sq[j]^(-1))
  
  return(sqrt(first_term + second_term))
}

tau_seq = seq(0.01,8,0.01)

conditional_sd_theta_evaluations = matrix(NA, nrow=length(y_bar), ncol=length(tau_seq))

for(j in 1:nrow(conditional_sd_theta_evaluations))
{
  for(i in 1:ncol(conditional_sd_theta_evaluations))
  {
    conditional_sd_theta_evaluations[j,i] = conditional_sd_theta(tau_seq[i], j)
  }
}

plot(tau_seq, conditional_sd_theta_evaluations[1,], type="l", 
     ylim=c(min(conditional_sd_theta_evaluations),max(conditional_sd_theta_evaluations)),
     xlab=expression(tau), ylab="Points",
     main=expression(paste("Posterior Standard Deviations of "*theta[j]*" Conditional on "*tau)))
for(j in 2:length(y_bar))
{
  points(tau_seq, conditional_sd_theta_evaluations[j,], type="l")
}

NBA_data[order((NBA_data[,3]/NBA_data[,1]), decreasing=TRUE),]

standard_error = NBA_data[,3]/sqrt(NBA_data[,1])
standard_error[order(standard_error, decreasing=TRUE)]


tau_seq = seq(0.01,50,0.01)

conditional_sd_theta_evaluations = matrix(NA, nrow=length(y_bar), ncol=length(tau_seq))

for(j in 1:nrow(conditional_sd_theta_evaluations))
{
  for(i in 1:ncol(conditional_sd_theta_evaluations))
  {
    conditional_sd_theta_evaluations[j,i] = conditional_sd_theta(tau_seq[i], j)
  }
}

plot(tau_seq, conditional_sd_theta_evaluations[1,], type="l", 
     ylim=c(min(conditional_sd_theta_evaluations),max(conditional_sd_theta_evaluations)),
     xlab=expression(tau), ylab="Points",
     main=expression(paste("Posterior Standard Deviations of "*theta[j]*" Conditional on "*tau)))
for(j in 2:length(y_bar))
{
  points(tau_seq, conditional_sd_theta_evaluations[j,], type="l")
}

NBA_data[order((NBA_data[,3]/NBA_data[,1]), decreasing=TRUE),]

standard_error = NBA_data[,3]/sqrt(NBA_data[,1])
standard_error_ordered = standard_error[order(standard_error, decreasing=TRUE)]
abline(h=standard_error_ordered[1], lty=2)
abline(h=standard_error_ordered[2], lty=2)
abline(h=standard_error_ordered[3], lty=2)


# Getting posterior draws of parameters

number_draws = 10^4

posterior_draws_tau = rep(NA, number_draws)
posterior_draws_mu = rep(NA, number_draws)
posterior_draws_theta = matrix(NA, nrow=number_draws, ncol=length(y_bar))

tau_seq = seq(0.001,15,0.001)

log_posterior_tau_evaluations = sapply(tau_seq, log_posterior_tau) - max(sapply(tau_seq, log_posterior_tau))
posterior_tau_evaluations = exp(log_posterior_tau_evaluations)/(sum(exp(log_posterior_tau_evaluations))*0.001)

posterior_draws_tau = sample(tau_seq, number_draws, replace=TRUE, prob=posterior_tau_evaluations)

hist(posterior_draws_tau, freq=FALSE, breaks="fd",
     xlab=expression(tau), ylab="Density", main=expression(paste("Marginal Posterior of "*tau)))
points(tau_seq, posterior_tau_evaluations, type="l")

for(i in 1:number_draws)
{
  posterior_draws_mu[i] = sum((posterior_draws_tau[i]^2 + sigma_j_sq)^(-1)*y_bar)/sum((posterior_draws_tau[i]^2 + sigma_j_sq)^(-1)) + sqrt(1/sum(1/(posterior_draws_tau[i]^2+sigma_j_sq)))*rnorm(1)
  
  posterior_draws_theta[i,] = (posterior_draws_tau[i]^(-2)*posterior_draws_mu[i] + sigma_j_sq^(-1)*y_bar)/(posterior_draws_tau[i]^(-2) + sigma_j_sq^(-1)) + sqrt(1/(posterior_draws_tau[i]^(-2) + sigma_j_sq^(-1)))*rnorm(length(posterior_draws_theta[i,]))
}

posterior_means_theta = colMeans(posterior_draws_theta)

plot(posterior_means_theta, rep(0,J), 
     main="Shrinkage Plot for Sample Averages", 
     ylim=c(0,15), 
     xlim=c(min(c(posterior_means_theta, y_bar)), max(c(posterior_means_theta, y_bar))), 
     ylab="", xlab="Average Score", yaxt="n")
axis(at=c(0,10), labels=c("Posterior", "Sample"), side=2, tick=FALSE)
points(y_bar, rep(10,J))
points((sum(n_j*y_bar)/sum(n_j)), 0, pch=3, lwd=5)
for(i in 1:J)
{
  segments(posterior_means_theta[i], 0, (y_bar[i]), 10)
  segments(y_bar[i],10,y_bar[i],(10 + 0.3*(sigma_j_sq[i])^0.5), lwd=3)
}


# Timothy Keaton's approach

pdf("shrinkage_plot_tim.pdf")
plot(posterior_means_theta, rep(0,J), pch=16,
     main="Shrinkage Plot for Sample Averages: Credit Due to Tim Keaton", 
     ylim=c(0,15), 
     xlim=c(84,115),
     #     xlim=c(min(c(posterior_means_theta, y_bar)), max(c(posterior_means_theta, y_bar))), 
     ylab="", xlab="Average Score", yaxt="n")
axis(at=c(0,10), labels=c("Posterior", "Sample"), side=2, tick=FALSE)
points(y_bar, rep(10,J),pch=16)
points((sum(n_j*y_bar)/sum(n_j)), 0, pch=3, lwd=5)
rad=pi/4
maxs=max(sqrt(sigma_j_sq[i]))
segments(85,0,110,0)
segments(85,10,110,10)
segments(85,0,85,10)
segments(110,0,110,10)
segments(85,10,85+maxs*sin(rad),10+maxs*sin(rad))
segments(110,10,110+maxs*sin(rad),10+maxs*sin(rad))
segments(110,0,110+maxs*sin(rad),0+maxs*sin(rad))
segments(85+maxs*sin(rad),10+maxs*sin(rad),110+maxs*sin(rad),10+maxs*sin(rad))
segments(110+maxs*sin(rad),0+maxs*sin(rad),110+maxs*sin(rad),10+maxs*sin(rad))
text(84,14,pos=4,'Standard\nDeviaton')
for(i in 1:J)
{
  segments(posterior_means_theta[i], 0, (y_bar[i]), 10)
  #  segments(y_bar[i],10,y_bar[i],(10 + 0.3*(sigma_j_sq[i])^0.5), lwd=3)
  segments(y_bar[i],10,y_bar[i]+sqrt(sigma_j_sq[i])*sin(rad),(10 + sqrt(sigma_j_sq[i])*cos(rad)), lwd=3)
}
dev.off()


# Final evaluation

sum((y_bar-NBA_data[,5])^2)
sum((posterior_means_theta-NBA_data[,5])^2)