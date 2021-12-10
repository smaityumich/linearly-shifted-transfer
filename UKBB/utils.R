predict.accuracy <- function(y.true, y.pred)
{
  accuracy = mean(y.true == y.pred, na.rm = T)
  TPR = mean(y.pred[y.true == 1], na.rm = T)
  TNR = 1 - mean(y.pred[y.true == 0], na.rm = T)
  print(paste('Accuracy: ', accuracy, '; TPR: ', TPR, '; TNR: ', TNR, sep = ''))
  return(c(accuracy, TPR, TNR))
}


get.weights = function(y)
{
  mean.y = mean(y)
  weights = (1/mean.y) * y + (1/(1-mean.y)) * (1-y)
  return(weights)
}




sampling.index <- function(y, seed = 0, train.prop = 0.8)
{
  set.seed(seed)
  y.unique = unique(y)
  if(length(y.unique) < 11)
  {
    ind = c()
    for(y.i in y.unique)
    {
      inds.y.i = which(y == y.i)
      train.inds.y.i <- sample(inds.y.i, size = floor(train.prop * length(inds.y.i)))
      ind = c(ind, train.inds.y.i)
    }
  }
  else
  {
    m = length(y)
    ind = sample(1:m, size = floor(train.prop * m))
  }
  return(ind)
}






