source('~/projects/posterior-drift/sim/data.R')
source('~/projects/posterior-drift/sim/utils.R')


library(ggplot2)





iteraction.single <- function(m = 10000, n = 50, d = 5,
                              delta = 2, sigma = 1, seed = 0)
{
  set.seed(seed)
  beta.quad = rep(c(1, -1), floor(d/2))
  if(2 * floor(d/2)<d)
    beta.quad = c(beta.quad, 1)

  mu = beta.quad

  train.data = gen.transfer.train(m = m, n = n, mu = mu * delta, sigma = sigma)
  test.data = gen.transfer.train(m = 10000, n = 10000, mu = mu * delta, sigma = sigma)

  data = test.data
  data.source = data$data.source
  data.target = data$data.target

  p1 = ggplot(data.source, aes(x=x1, y=x2, color = as.factor(y))) + geom_point()
  p2 = ggplot(data.target, aes(x=x1, y=x2, color = as.factor(y))) + geom_point()

  gridExtra::grid.arrange(p1, p2)



  ### poly(deg = 2) model on source

  data = train.data
  data.source = data$data.source
  data.target = data$data.target

  x.data = poly.features(data.source, degree = 2)
  source.full = glm.fit(x.data, data.source$y, family = binomial())
  source.full.accuracy = train.test.accuracy(source.full, test.data, accuracy.glm.fit, degree = 2)




  ### Model with main effects on source
  source.main = glm(y~., family = binomial(), data = data.source)
  source.main.accuracy = train.test.accuracy(source.main, test.data, accuracy.glm, degree = 2)


  ### poly(deg = 2) model on target
  x.data = poly.features(data.target, degree = 2)
  target.full = glm.fit(x.data, data.target$y, family = binomial())
  target.full.accuracy = train.test.accuracy(target.full, test.data, accuracy.glm.fit, degree = 2)

  target.main = glm(y~., family = binomial(), data = data.target)
  target.main.accurcay = train.test.accuracy(target.main, test.data, accuracy.glm, degree = 2)

  accuracy.list = rbind(source.full.accuracy, source.main.accuracy,
                      target.full.accuracy, target.main.accurcay)

  accuracy.list = as.data.frame(accuracy.list)
  colnames(accuracy.list) = c('source', 'target')
  rownames(accuracy.list) = c('source.full', 'source.main',
                            'target.full', 'target.main')

  ### transfer model
  x.data = poly.features(data.target, degree = 2)
  offset.target = as.matrix(x.data) %*% source.full$coefficients
  
  
  linear.adjust = glm(y~. + offset(offset.target), data = data.target,
                      family = binomial())

  transfer.accuracy = accuracy.transfer.function(linear.adjust, source.full, data = test.data, degree = 2)
  
  accuracy.list = rbind(accuracy.list, transfer.accuracy)
  rownames(accuracy.list)[nrow(accuracy.list)] = 'transfer'

  return(accuracy.list)
}


iteraction.single()