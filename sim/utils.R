
poly.features <- function(data, degree = 2)
{
  x.data = as.matrix(data[, colnames(data) != 'y'])
  x = model.matrix(y~.^2, data = data)
  cname = colnames(x)
  cname = c(cname, paste(colnames(x.data), '.2', sep = ''))
  x = cbind(x, x.data^2)
  colnames(x) = cname
  return(x)
}


accuracy.glm <- function(model, data, degree = 2)
{
  accuracy = 1 - mean(predict(model, newdata = data) * (2 * data$y - 1) < 0)
  return(accuracy)
}

accuracy.glm.fit <- function(model, data, degree = 2)
{
  x = poly.features(data, degree = 2)
  link = as.matrix(x) %*% model$coefficients
  accuracy = 1 - mean(link * (2 * data$y - 1) < 0)
  return(accuracy)
}

accuracy.glmnet <- function(model, data, degree = 2)
{
  x = poly.features(data, degree = degree)
  accuracy = 1 - mean(predict(model, newx = x, s = "lambda.1se") * (2 * data$y - 1)<0)
  return(accuracy)
}


accuracy.transfer.function <- function(model, offset.model, data, degree = 2)
{
  source.data = data$data.source
  target.data = data$data.target
  
  source.offset = poly.features(source.data, degree = degree) %*% offset.model$coefficients
  target.offset = poly.features(target.data, degree = degree) %*% offset.model$coefficients

  link.source = model.matrix(y~., data = source.data) %*% model$coefficient + source.offset
  link.target = model.matrix(y~., data = target.data) %*% model$coefficient + target.offset


  accuracy.source = 1 - mean(link.source * (2 * source.data$y - 1) < 0)
  accuracy.target = 1 - mean(link.target * (2 * target.data$y - 1) < 0)
  
  return(c(accuracy.source, accuracy.target))
}



train.test.accuracy <- function(model, test.data, accuracy.function, degree = 2)
{
  data = test.data$data.source
  source.accuracy = accuracy.function(model, data, degree = 2)
  
  data = test.data$data.target
  target.accuracy = accuracy.function(model, data, degree = 2)
  
  return(c(source.accuracy, target.accuracy))
}



parameter.grid <- function(par.list)
{
  n = 0
  for(name in names(par.list))
    n = n + length(par.list[[name]][['vec']])
  
  pars = matrix(0, n, length(par.list))
  pars = as.data.frame(pars)
  colnames(pars) = names(par.list)
  
  index.start = 1
  index.end = 1
  for(i in 1:length(par.list))
  {
    name = names(par.list)[i]
    pars[, i] = rep(par.list[[name]][['default']], n)
    index.end = index.start + length(par.list[[name]][['vec']]) - 1
    if(index.end - index.start >= 0)
      pars[index.start:index.end, i] = par.list[[name]][['vec']]
    index.start = index.end + 1
  }
  return(pars)
}



parameter.grid2 <- function(par.list)
{
  n = 0
  for(name in names(par.list))
    n = n + length(par.list[[name]][['vec']])
  
  v.list = list()
  
  index.start = 1
  index.end = 1
  for(i in 1:length(par.list))
  {
    p.name = names(par.list)[i]
    p.list = par.list[[p.name]]
    v = rep(p.list[['default']], n)
    index.end = index.start + length(p.list[['vec']]) - 1
    if(index.end - index.start >= 0)
      v[index.start:index.end] = p.list[['vec']]
    index.start = index.end + 1
    
    v.list[[p.name]] = v
  }
  
  pars = as.data.frame(v.list)
  return(pars)
}



