# Vectorized code to sample cluster assignments
assign_cls <- function(X,lambda,theta,K,nobs) {
  rsp <- matrix(log(lambda),nrow=K, ncol = nobs) # K rows
  for(kk in 1:K) {
    rsp[kk,] <- rsp[kk,] + dpois(X,theta[kk],log=T)
  }
  rsp <- exp(t(rsp) - apply(rsp,2,max)) # K cols
  rsp <- apply(rsp,1,cumsum)
  rsp <- t(rsp) > (runif(nobs)*rsp[K,])
  max.col(rsp,"first")
}


# Poisson mixture model for RNA sequence data
ccr <- read.table('./rna_seq.txt')
nobs <- nrow(ccr)
X   <- ccr$V4
plot(density(X))
points(X,rnorm(nobs,0,.0001),pch=4)

K   <- 10
alp <- 1
a_0 <- 1; a_1 <- .01

n_iter <- 2000

zz   <- matrix(1L, n_iter,nobs)
theta <- matrix(0, n_iter,K)
lmbd   <- matrix(0, n_iter,K)


# To sample from a Dirichlet distribution, sample a bunch of
#  Gamma variables and normalize them
pi_0   <- rgamma(K,alp,1)
lmbd[1,] <- pi_0/sum(pi_0)


theta_0 <- rgamma(K,a_0,a_1)
theta[1,] <- theta_0; # seq(50,by=50,len=5)


for(ii in 2:n_iter) {
  zz[ii,] <- assign_cls(X,lmbd[ii-1,],theta[ii-1,],K,nobs) 
  for(cls in 1:K)  {
    curr <- X[zz[ii,]==cls]
    theta[ii,cls] <- rgamma(1,sum(curr)+a_0, length(curr)+a_1)
    lmbd[ii,cls]   <- rgamma(1,length(curr)+alp)
  }
  lmbd[ii,] <- lmbd[ii,]/sum(lmbd)
}

plot_dens <- function(grd,lmbd,theta) {
  K <- length(lmbd) 
  prb <- rep(0,length(grd))
  for(kk in 1:K) {
    prb <- prb + lmbd[kk]*dpois(grd,theta[kk])
  }
  return(prb)
}

plot_comps <- function(grd,lmbd,theta) {
  K <- length(lmbd) 
  prb <- matrix(0,K*length(grd),3)
  for(kk in 1:K) {
    ix <- (kk-1)*length(grd)+(1:length(grd))
    prb[ix,1] <- grd
    prb[ix,2] <- lmbd[kk]*dpois(grd,theta[kk])
    prb[ix,3] <- kk
  }
  return(prb)
}


grd <- 0:600
prb <- matrix(0,n_iter,length(grd))
for(ii in 1:n_iter) {
  prb[ii,] <- plot_dens(grd,lmbd[ii,],theta[ii,])
}
tmp <- data.frame(t(apply(prb, 2,quantile,c(.1,.5,.9))))
tmp$X <- grd
ggplot(data=tmp, aes(x=X)) + geom_line(aes(y=X50.)) +
              geom_ribbon(aes(ymin=X10., ymax=X90.),fill='blue', alpha=0.3) +
              geom_point(data=data.frame(X),aes(x=X,y=0))
