source('~/projects/posterior-drift/sim/data.R')
source('~/projects/posterior-drift/sim/utils.R')







iteration.single <- function(m = 14142, n = 50, d = 5,
                              delta = 2, sigma = 1, link = 'logit', 
                             seed = 1)
{
  print('Running for parameters...')
  print(list(m = m, n = n, d = d, 
             delta = delta, sigma = sigma, link = link,
             seed = seed))
  
  set.seed(seed = seed)
  beta.quad = rep(c(1, -1), floor(d/2))
  if(2 * floor(d/2)<d)
    beta.quad = c(beta.quad, 1)
  
  mu = beta.quad
  
  
  
  
  
  train.data = gen.transfer.train(m = m, n = n, mu = mu * delta, sigma = sigma)
  test.data = gen.transfer.train(m = 10000, n = 10000, mu = mu * delta, sigma = sigma, link = link)
  
  
  val.data = gen.transfer.train(m = 100, n = 100000, mu = mu * delta, sigma = sigma, link = link)
  
  
  
  ### poly(deg = 2) model on source
  
  data = train.data
  data.source = data$data.source
  data.target = data$data.target
  
  x.data = poly.features(data.source, degree = 2)
  source.full = glm.fit(x.data, data.source$y, family = binomial())
  source.full.accuracy = train.test.accuracy(source.full, test.data, accuracy.glm.fit, degree = 2)
  
   
  
  
  ### Model with main effects on source
  
  data = train.data
  data.source = data$data.source
  data.target = data$data.target
  
  source.main = glm(y~., family = binomial(), data = data.source)
  source.main.accuracy = train.test.accuracy(source.main, test.data, accuracy.glm, degree = 2)
  
  
  ### poly(deg = 2) model on target
  
  data = train.data
  data.source = data$data.source
  data.target = data$data.target
  
  
  x.data = poly.features(data.target, degree = 2)
  target.full = glm.fit(x.data, data.target$y, family = binomial())
  target.full.accuracy = train.test.accuracy(target.full, test.data, accuracy.glm.fit, degree = 2)
  
  
  ### poly(deg = 2) model on target to calculate validation accuracy
  
  data = val.data
  data.source = data$data.source
  data.target = data$data.target
  
  
  x.data = poly.features(data.target, degree = 2)
  val.full = glm.fit(x.data, data.target$y, family = binomial())
  val.full.accuracy = train.test.accuracy(val.full, test.data, accuracy.glm.fit, degree = 2)
  
  
  ### Model with main effects on target
  data = train.data
  data.source = data$data.source
  data.target = data$data.target
  
  
  
  target.main = glm(y~., family = binomial(), data = data.target)
  target.main.accurcay = train.test.accuracy(target.main, test.data, accuracy.glm, degree = 2)
  
  accuracy.list = rbind(val.full.accuracy, 
                        source.full.accuracy, source.main.accuracy,
                        target.full.accuracy, target.main.accurcay)
  
  accuracy.list = as.data.frame(accuracy.list)
  colnames(accuracy.list) = c('source', 'target')
  rownames(accuracy.list) = c('val.full',
                              'source.full', 'source.main',
                              'target.full', 'target.main')
  
  
  
  
  ### Transfer model
  
  
  
  data = train.data
  data.source = data$data.source
  data.target = data$data.target
  
  x.data = poly.features(data.target, degree = 2)
  offset.target = as.matrix(x.data) %*% source.full$coefficients
  
  
  linear.adjust = glm(y~. + offset(offset.target), data = data.target,
                      family = binomial())
  
  transfer.accuracy = accuracy.transfer.function(linear.adjust, source.full, data = test.data, degree = 2)
  
  accuracy.list = rbind(accuracy.list, transfer.accuracy)
  rownames(accuracy.list)[nrow(accuracy.list)] = 'transfer'
  
  
  
  return(accuracy.list)
}




par.list = list()


par.list[['m']] = list(default = 2000, vec = 2^(0:6) * 50)
par.list[['n']] = list(default = 100, vec = 2^(0:6) * 20)
par.list[['d']] = list(default = 5, vec = c(2, 4, 6, 8, 10))
par.list[['delta']] = list(default = 2, vec = 0.25 * (1.5)^(0:5))
par.list[['sigma']] = list(default = 1, vec = 0.125 * (1.5)^(0:5))


pars = parameter.grid2(par.list)

args = commandArgs(trailingOnly=TRUE)
iter = as.integer(args[1])



for(link in c('logit', 'probit', 'cauchit', 'cloglog'))
{
    for(index in 1:nrow(pars))
    {
      
      m = as.integer(pars[index, 1])
      n = as.integer(pars[index, 2])
      d = as.integer(pars[index, 3])
      delta = as.numeric(pars[index, 4])
      sigma = as.numeric(pars[index, 5])
      
      accuracy.list = iteration.single(m = m, n = n, d = d, delta = delta,
                                       sigma = sigma, link = link, seed = iter)
      print(accuracy.list)
      
      ret.list = list()
      ret.list[['accuracy']] = list(name = row.names(accuracy.list),
                                    source = accuracy.list[, 'source'],
                                    target = accuracy.list[, 'target'])
      ret.list[['par']] = list(m = m, n = n, d = d, 
                               delta = delta, sigma = sigma, link = link,
                               iter = iter)
      
      
      write(rjson::toJSON(ret.list), file = paste('summaries_part/acc_',link, '_', ((iter) * nrow(pars) + index-1), '.json', sep = ''))
      
    }

}
