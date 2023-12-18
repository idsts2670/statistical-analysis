#install.packages('coda')
library('coda')
library('ggplot2')

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


tau_sq_seq = seq(0.001,50,0.001)

log_posterior_tau_sq_evaluations = sapply(tau_sq_seq, log_posterior_tau_sq) - max(sapply(tau_sq_seq, log_posterior_tau_sq))
posterior_tau_sq_evaluations = exp(log_posterior_tau_sq_evaluations)/(sum(exp(log_posterior_tau_sq_evaluations))*0.001)

plot(tau_sq_seq, posterior_tau_sq_evaluations, type="l",
     xlab=expression(tau^2), ylab="Density", main=expression(paste("Marginal Posterior of "*tau^2)))



mh_tau_sq <- function(tau_init=1, num_iter=10000, prop_sd = 1)
{
  tau_sq_smpls    <- rep(0, num_iter)
  tau_sq_smpls[1] <- tau_init
  log_p_curr   <- log_posterior_tau_sq(tau_init)
  acc <- 0

  for(ii in 2:num_iter) {
    tau_sq_smpls[ii] <- tau_sq_smpls[ii-1]
    tau_sq_new       <- tau_sq_smpls[ii-1] + 
                        prop_sd * rnorm(1)
    log_p_new     <- log_posterior_tau_sq(abs(tau_sq_new))
    if(log(runif(1)) < (log_p_new - log_p_curr)) {
        log_p_curr    <- log_p_new
        tau_sq_smpls[ii] <- tau_sq_new
        acc <- acc + 1
      }
  }
  print(paste(c(acc,"out of",number_draws,"proposals accepted"), collapse=' '))
  return(list(smpls = abs(tau_sq_smpls), acc=acc))
}

number_draws = 10^4
#posterior_draws_tau = rep(NA, number_draws)

posterior_draws_tau_sq = sample(tau_sq_seq, number_draws, replace=TRUE, prob=posterior_tau_sq_evaluations)

hist(posterior_draws_tau_sq, freq=FALSE, breaks="fd",
     xlab=expression(tau_sq), ylab="Density", main=expression(paste("Marginal Posterior of "*tau)))
points(tau_sq_seq, posterior_tau_sq_evaluations, type="l")


mh_smpls <- mh_tau_sq(num_iter = number_draws, prop_sd = 10)
my_df    <- data.frame(indx = 1:number_draws, samples = mh_smpls$smpls)

hist(mh_smpls$smpls, freq=FALSE, breaks="fd",
     xlab=expression(tau_sq), ylab="Density", main=expression(paste("Marginal Posterior of "*tau)))
points(tau_sq_seq, posterior_tau_sq_evaluations, type="l")


plot(x=my_df$indx, y=my_df$samples, type='l',xlab='MCMC iteration',ylab='tau squared')
plot(acf(my_df$samples))
effectiveSize(my_df$samples)

optim(1, log_posterior_tau_sq, method='BFGS', hessian=T)
