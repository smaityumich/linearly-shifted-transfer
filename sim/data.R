library(Matrix)
library(MASS)
library(mvtnorm)


gen.y <- function(n = 100)
{
  y = rbinom(n, size = 1, p = 0.5)
  return(y)
}


gen.x <- function(y, mu1, mu0, Sigma1, Sigma0)
{
  n = length(y)
  n1 = sum(y)
  n0 = n - n1
  d = length(mu1)
  x = matrix(0, n, d)
  x[which(y == 1), ] = mvrnorm(n = n1, mu1, Sigma1)
  x[which(y == 0), ] = mvrnorm(n = n0, mu0, Sigma0)
  return(x)
}





posterior.prob2 <- function(x, beta, curvature = 1, link = 'logit')
{
  d = length(beta)
  beta.quad = rep(c(1, -1), floor(d/2))
  if(2 * floor(d/2)<d)
    beta.quad = c(beta.quad, 1)
  
  beta.combined = c(beta, beta.quad * curvature)
  x.combined = cbind(x, x^2)
  r = x.combined %*% beta.combined
  link = binomial(link = link)
  return(link$linkinv((0.25 * r)))
}








gen.xy <- function(n, mu, sigma, link = 'logit')
{
  # For generating x, y 
  d = length(mu)
  x = matrix(rnorm(n*d, 0, 2), n, d)

  p = posterior.prob2(x, mu, sigma, link = link)
  y = rbinom(n, 1, p)
  data = as.data.frame(x)
  colnames(data) = paste('x', 1:length(mu), sep = '')
  data$y = y
  
 
  return(data)
}





gen.transfer.train <- function(m, n, mu, sigma = 1, link = 'logit')
{
  
  data.source = gen.xy(n = m, mu = - mu, sigma = sigma, link = link)
  data.target = gen.xy(n = n, mu = mu, sigma = sigma, link = link)
  
  return(list(data.source = data.source, data.target = data.target))
}
