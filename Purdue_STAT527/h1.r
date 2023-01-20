t = 0:10
n = 1000*(1+0.05)^t
plot(t,n,type = "l")

plot(x <- sort(runif(1000))*2*pi, sin(x))
