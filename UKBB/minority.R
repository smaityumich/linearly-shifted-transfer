library("glmnet")

#########################################################################
############# minority models and evaluations function ##################


minority.evaluations <- function(ethnicity = "Asian", bd.minority)
{
  group = ethnicity
  
  ##################################################################
  ############## train/test split ##################################
  
  train_ind = sampling.index(bd.minority$mortality)
  train.bd.minority <- bd.minority[train_ind, ]
  test.bd.minority <- bd.minority[-train_ind, ]
  
  
  ###################################################################
  ################# evaluation of glm.white #########################
  
  x.test.minority = model.matrix(`mortality` ~ .^2, data = test.bd.minority)
  
  logit = predict(glm.white, x.test.minority,
                  s = 'lambda.min', type = 'response')
  y.pred = as.numeric(logit>0.5)
  y.true = test.bd.minority$`mortality`
  print(paste('Prediction summaries for', group, 'with model fitted on majority:'))
  pred.majority = predict.accuracy(y.true, y.pred)
  auc.majority = assess.glmnet(predict(glm.white, x.test.minority, s= "lambda.min"), newy = y.true, family = "binomial")$auc
  
  train.major.full.test.minor = c(pred.majority, auc.majority)
  
  
  ####################################################################
  ################# offset model: full offset ########################
  print("Offset from full model")
  weights = get.weights(train.bd.minority$`mortality`)
  
  
  
  x.train.minority = model.matrix(`mortality` ~ .^2,
                                  data = train.bd.minority)
  
  
  beta.offset = coef(glm.white, s = "lambda.min")
  offset.minority = x.train.minority %*% beta.offset[-1] + beta.offset[1]
  offset.minority = as.numeric(offset.minority)
  
  x.train.minority = model.matrix(`mortality` ~ .,
                                  data = train.bd.minority)
  y.train.minority = train.bd.minority$mortality
  
  
  nfold <- 5
  set.seed(12121)
  # assign folds evenly using the modulus operator
  fold0 <- sample.int(sum(y.train.minority==0)) %% nfold
  fold1 <- sample.int(sum(y.train.minority==1)) %% nfold
  foldid <- numeric(length(y.train.minority))
  foldid[y.train.minority==0] <- fold0
  foldid[y.train.minority==1] <- fold1
  foldid <- foldid + 1
  
  
  
  
  glm.minority.offset.full = cv.glmnet(x.train.minority, y.train.minority, weights = weights, foldid = foldid, trace.it = T, nfolds = 5, type.measure = measure, family = 'binomial', parallel = TRUE, offset = offset.minority, keep = TRUE)
  rm('offset.minority')
  
  
  x.test.minority = model.matrix(`mortality` ~ .^2,
                                 data = test.bd.minority)
  
  
  
  offset.minority = x.test.minority %*% beta.offset[-1] + beta.offset[1]
  offset.minority = as.numeric(offset.minority)
  
  x.test.minority = model.matrix(`mortality` ~ .,
                                 data = test.bd.minority)
  
  
  
  logit = predict(glm.minority.offset.full, newx = x.test.minority,
                  s = "lambda.min", type = "response", 
                  newoffset = offset.minority)
  y.pred = as.numeric(logit>0.5)
  y.true = test.bd.minority$`mortality`
  print(paste('Prediction summaries for', group, 'with interactions as offset from White:'))
  pred.offset = predict.accuracy(y.true, y.pred)
  auc.offset = assess.glmnet(logit, newy = y.true, family = "binomial")$auc
  
  
  
  train.major.offset.full.test.minor = c(pred.offset, auc.offset)
  
  
  #######################################################################
  ################# offset model: offset main ###########################
  print("Offset only from main effects")
  
  weights = get.weights(train.bd.minority$`mortality`)
  
  
  x.train.minority = model.matrix(`mortality` ~ .,
                                  data = train.bd.minority)
  
  
  beta.offset = coef(glm.white.main, s = "lambda.min")
  
  offset.minority = x.train.minority %*% beta.offset[-1] + beta.offset[1]
  offset.minority = as.numeric(offset.minority)
  
  x.train.minority = model.matrix(`mortality` ~ .,
                                  data = train.bd.minority)
  y.train.minority = train.bd.minority$mortality
  
  
  nfold <- 5
  set.seed(12121)
  # assign folds evenly using the modulus operator
  fold0 <- sample.int(sum(y.train.minority==0)) %% nfold
  fold1 <- sample.int(sum(y.train.minority==1)) %% nfold
  foldid <- numeric(length(y.train.minority))
  foldid[y.train.minority==0] <- fold0
  foldid[y.train.minority==1] <- fold1
  foldid <- foldid + 1
  
  
  
  
  glm.minority.offset.main = cv.glmnet(x.train.minority, y.train.minority, weights = weights, foldid = foldid, trace.it = T, nfolds = 5, type.measure = measure, family = 'binomial', parallel = TRUE, offset = offset.minority, keep = TRUE)
  rm('offset.minority')
  
  
  x.test.minority = model.matrix(`mortality` ~ .,
                                 data = test.bd.minority)
  
  
  
  offset.minority = x.test.minority %*% beta.offset[-1] + beta.offset[1]
  offset.minority = as.numeric(offset.minority)
  
  
  x.test.minority = model.matrix(`mortality` ~ .,
                                 data = test.bd.minority)
  
  
  
  logit = predict(glm.minority.offset.main, newx = x.test.minority,
                  s = "lambda.min", type = "response", 
                  newoffset = offset.minority)
  y.pred = as.numeric(logit>0.5)
  y.true = test.bd.minority$`mortality`
  print(paste('Prediction summaries for', group, 'with interactions as offset from White:'))
  pred.offset = predict.accuracy(y.true, y.pred)
  auc.offset = assess.glmnet(logit, newy = y.true, family = "binomial")$auc
  
  train.major.offset.main.test.minor = c(pred.offset, auc.offset)
  
  #########################################################################
  ######################### offset model: offset interaction ##############
  print("offset from interaction effects")
  
  weights = get.weights(train.bd.minority$`mortality`)
  
  
  x.train.minority = model.matrix(`mortality` ~ .^2,
                                  data = train.bd.minority)
  
  
  beta.offset = coef(glm.white, s = "lambda.min")
  mains = !str_detect(row.names(beta.offset), ":")
  beta.offset[mains] = 0
  offset.minority = x.train.minority %*% beta.offset[-1]
  offset.minority = as.numeric(offset.minority)
  
  x.train.minority = model.matrix(`mortality` ~ .,
                                  data = train.bd.minority)
  y.train.minority = train.bd.minority$mortality
  
  
  nfold <- 5
  set.seed(12121)
  # assign folds evenly using the modulus operator
  fold0 <- sample.int(sum(y.train.minority==0)) %% nfold
  fold1 <- sample.int(sum(y.train.minority==1)) %% nfold
  foldid <- numeric(length(y.train.minority))
  foldid[y.train.minority==0] <- fold0
  foldid[y.train.minority==1] <- fold1
  foldid <- foldid + 1
  
  
  
  
  glm.minority.offset.interaction = cv.glmnet(x.train.minority, y.train.minority, weights = weights, foldid = foldid, trace.it = T, nfolds = 5, type.measure = measure, family = 'binomial', parallel = TRUE, offset = offset.minority, keep = TRUE)
  rm('offset.minority')
  
  
  x.test.minority = model.matrix(`mortality` ~ .^2,
                                 data = test.bd.minority)
  
  
  
  offset.minority = x.test.minority %*% beta.offset[-1]
  offset.minority = as.numeric(offset.minority)
  
  
  x.test.minority = model.matrix(`mortality` ~ .,
                                 data = test.bd.minority)
  
  
  
  logit = predict(glm.minority.offset.interaction, newx = x.test.minority,
                  s = "lambda.min", type = "response", 
                  newoffset = offset.minority)
  y.pred = as.numeric(logit>0.5)
  y.true = test.bd.minority$`mortality`
  print(paste('Prediction summaries for', group, 'with interactions as offset from White:'))
  pred.offset = predict.accuracy(y.true, y.pred)
  auc.offset = assess.glmnet(logit, newy = y.true, family = "binomial")$auc
  
  train.major.offset.interaction.test.minor = c(pred.offset, auc.offset)
  
  
  #############################################################################
  #################### minority full model ####################################
  
 
  print("full model on minority")
  
  x.train.minority = model.matrix(`mortality` ~ .^2,
                                  data = train.bd.minority)
  
  y.train.minority = train.bd.minority$mortality
  
  glm.minority = cv.glmnet(x.train.minority, y.train.minority, weights = weights, foldid = foldid, trace.it = T, nfolds = 5, type.measure = measure, family = 'binomial', parallel = TRUE, keep = TRUE)
  
  x.test.minority = model.matrix(`mortality` ~ .^2,
                                 data = test.bd.minority)
  logit = predict(glm.minority, x.test.minority,
                  s = "lambda.min", type = "response")
  y.pred = as.numeric(logit>0.5)
  y.true = test.bd.minority$`mortality`
  print(paste('Prediction summaries for', group, 'with full estimated on the same data:'))
  pred.original = predict.accuracy(y.true, y.pred)
  auc.original = assess.glmnet(logit, newy = y.true, family = "binomial")$auc
  
  
  train.minor.full.test.minor = c(pred.original, auc.original)
  
  
  #############################################################################
  #################### minority main model ####################################
  
  print("main model on minority")
  x.train.minority = model.matrix(`mortality` ~ .,
                                  data = train.bd.minority)
  
  y.train.minority = train.bd.minority$mortality
  
  glm.minority.main = cv.glmnet(x.train.minority, y.train.minority, weights = weights, foldid = foldid, trace.it = T, nfolds = 5, type.measure = measure, family = 'binomial', parallel = TRUE, keep = TRUE)
  
  x.test.minority = model.matrix(`mortality` ~ .,
                                 data = test.bd.minority)
  logit = predict(glm.minority.main, x.test.minority,
                  s = "lambda.min", type = "response")
  y.pred = as.numeric(logit>0.5)
  y.true = test.bd.minority$`mortality`
  print(paste('Prediction summaries for', group, 'with only main effects estimated on the minority data:'))
  pred.no.interaction = predict.accuracy(y.true, y.pred)
  auc.no.interaction = assess.glmnet(logit, newy = y.true, family = "binomial")$auc
  
  
  train.minor.main.test.minor = c(pred.no.interaction, auc.no.interaction)
  
  #################################################
  ############ majority main effect model ################
  #################################################
  
  
  
  logit = predict(glm.white.main, x.test.minority,
                  s = "lambda.min", type = "response")
  y.pred = as.numeric(logit>0.5)
  print(paste('Prediction summaries for', group, 'with only main effects estimated on the majority data:'))
  pred.main = predict.accuracy(y.true, y.pred)
  auc.main = assess.glmnet(logit, newy = y.true, family = "binomial")$auc
  
  train.major.main.test.minor = c(pred.main, auc.main)
  
  #############################################################
  #############################################################
  
  minority.summary = rbind(train.major.full.test.minor, train.major.main.test.minor,
                           train.minor.full.test.minor, train.minor.main.test.minor,
                           train.major.offset.full.test.minor,
                           train.major.offset.main.test.minor,
                           train.major.offset.interaction.test.minor)
  
  minority.summary = as.data.frame(minority.summary)
  major = "White"
  minor = group
  
  colnames(minority.summary) = c('accuracy', 'TPR', 'TNR', 'auc')
  
  
  
  minority.summary = data.frame(minority.summary)
  
  minority.summary$bal.acc = (minority.summary$TPR + minority.summary$TNR)/2
  minority.summary$test.data = group
  
  minority.summary[['model']] = c(paste(major, '.full', sep = ""),
                                  paste(major, '.main', sep =""),
                                  paste(minor, '.full', sep = ""),
                                  paste(minor, '.main', sep =""),
                                  paste(major, '.offset.full', sep = ""),
                                  paste(major, '.offset.main', sep = ""),
                                  paste(major, '.offset.interaction', sep = ""))
  
  order.col = c("model", "test.data", "accuracy",
                "TPR", "TNR", "auc", "bal.acc" )
  minority.summary = minority.summary[, order.col]
  row.names(minority.summary) = NULL
  
  return.list = list(minority.summary = minority.summary, 
                     offset.full = glm.minority.offset.full,
                     offset.main = glm.minority.offset.main,
                     offset.interaction = glm.minority.offset.interaction,
                     minority.full = glm.minority,
                     minority.main = glm.minority.main)
  
  
  return(return.list)
  
  
  
}




adv.sampling <- function(model, df, p)
{
  y = df$mortality
  x = model.matrix(`mortality` ~ ., data = df)
  probs = predict(model, newx = x, s = "lambda.min", type = "response")
  losses = - y * log(probs) - (1 - y) * log(1- probs)
  losses.0 = losses[y == 0]
  losses.1 = losses[y == 1]
  ind.0 = which(y == 0)
  ind.1 = which(y == 1)
  ind.0.adv = ind.0[which(losses.0 >= quantile(losses.0, p))]
  ind.1.adv = ind.1[which(losses.1 >= quantile(losses.1, p))]
  return(df[c(ind.0.adv, ind.1.adv), ])
}






minority.evaluation.cv <- function(ethnicity = "Asian", bd.minority, test_ind)
{
  group = ethnicity
  
  ##################################################################
  ############## train/test split ##################################
  
  train.bd.minority <- bd.minority[-test_ind, ]
  test.bd.minority <- bd.minority[test_ind, ]
  
  
  ###################################################################
  ################# evaluation of glm.white #########################
  
  x.test.minority = model.matrix(`mortality` ~ .^2, data = test.bd.minority)
  
  logit = predict(glm.white, x.test.minority,
                  s = 'lambda.min', type = 'response')
  y.pred = as.numeric(logit>0.5)
  y.true = test.bd.minority$`mortality`
  print(paste('Prediction summaries for', group, 'with model fitted on majority:'))
  pred.majority = predict.accuracy(y.true, y.pred)
  auc.majority = assess.glmnet(predict(glm.white, x.test.minority, s= "lambda.min"), newy = y.true, family = "binomial")$auc
  
  train.major.full.test.minor = c(pred.majority, auc.majority)
  
  
  ####################################################################
  ################# offset model: full offset ########################
  
  weights = get.weights(train.bd.minority$`mortality`)
  
  
  
  x.train.minority = model.matrix(`mortality` ~ .^2,
                                  data = train.bd.minority)
  
  
  beta.offset = coef(glm.white, s = "lambda.min")
  offset.minority = x.train.minority %*% beta.offset[-1] + beta.offset[1]
  offset.minority = as.numeric(offset.minority)
  
  x.train.minority = model.matrix(`mortality` ~ .,
                                  data = train.bd.minority)
  y.train.minority = train.bd.minority$mortality
  
  
  nfold <- 5
  set.seed(12121)
  # assign folds evenly using the modulus operator
  fold0 <- sample.int(sum(y.train.minority==0)) %% nfold
  fold1 <- sample.int(sum(y.train.minority==1)) %% nfold
  foldid <- numeric(length(y.train.minority))
  foldid[y.train.minority==0] <- fold0
  foldid[y.train.minority==1] <- fold1
  foldid <- foldid + 1
  
  
  
  
  glm.minority.offset.full = cv.glmnet(x.train.minority, y.train.minority, weights = weights, foldid = foldid, trace.it = T, nfolds = 5, type.measure = measure, family = 'binomial', parallel = TRUE, offset = offset.minority, keep = TRUE)
  rm('offset.minority')
  
  
  x.test.minority = model.matrix(`mortality` ~ .^2,
                                 data = test.bd.minority)
  
  
  
  offset.minority = x.test.minority %*% beta.offset[-1] + beta.offset[1]
  offset.minority = as.numeric(offset.minority)
  
  x.test.minority = model.matrix(`mortality` ~ .,
                                 data = test.bd.minority)
  
  
  
  logit = predict(glm.minority.offset.full, newx = x.test.minority,
                  s = "lambda.min", type = "response", 
                  newoffset = offset.minority)
  y.pred = as.numeric(logit>0.5)
  y.true = test.bd.minority$`mortality`
  print(paste('Prediction summaries for', group, 'with interactions as offset from White:'))
  pred.offset = predict.accuracy(y.true, y.pred)
  auc.offset = assess.glmnet(logit, newy = y.true, family = "binomial")$auc
  
  
  
  train.major.offset.full.test.minor = c(pred.offset, auc.offset)
  
  
  #######################################################################
  ################# offset model: offset main ###########################
  
  weights = get.weights(train.bd.minority$`mortality`)
  
  
  x.train.minority = model.matrix(`mortality` ~ .,
                                  data = train.bd.minority)
  
  
  beta.offset = coef(glm.white.main, s = "lambda.min")
  
  offset.minority = x.train.minority %*% beta.offset[-1] + beta.offset[1]
  offset.minority = as.numeric(offset.minority)
  
  x.train.minority = model.matrix(`mortality` ~ .,
                                  data = train.bd.minority)
  y.train.minority = train.bd.minority$mortality
  
  
  nfold <- 5
  set.seed(12121)
  # assign folds evenly using the modulus operator
  fold0 <- sample.int(sum(y.train.minority==0)) %% nfold
  fold1 <- sample.int(sum(y.train.minority==1)) %% nfold
  foldid <- numeric(length(y.train.minority))
  foldid[y.train.minority==0] <- fold0
  foldid[y.train.minority==1] <- fold1
  foldid <- foldid + 1
  
  
  
  
  glm.minority.offset.main = cv.glmnet(x.train.minority, y.train.minority, weights = weights, foldid = foldid, trace.it = T, nfolds = 5, type.measure = measure, family = 'binomial', parallel = TRUE, offset = offset.minority, keep = TRUE)
  rm('offset.minority')
  
  
  x.test.minority = model.matrix(`mortality` ~ .,
                                 data = test.bd.minority)
  
  
  
  offset.minority = x.test.minority %*% beta.offset[-1] + beta.offset[1]
  offset.minority = as.numeric(offset.minority)
  
  
  x.test.minority = model.matrix(`mortality` ~ .,
                                 data = test.bd.minority)
  
  
  
  logit = predict(glm.minority.offset.main, newx = x.test.minority,
                  s = "lambda.min", type = "response", 
                  newoffset = offset.minority)
  y.pred = as.numeric(logit>0.5)
  y.true = test.bd.minority$`mortality`
  print(paste('Prediction summaries for', group, 'with interactions as offset from White:'))
  pred.offset = predict.accuracy(y.true, y.pred)
  auc.offset = assess.glmnet(logit, newy = y.true, family = "binomial")$auc
  
  train.major.offset.main.test.minor = c(pred.offset, auc.offset)
  
  #########################################################################
  ######################### offset model: offset interaction ##############
  
  weights = get.weights(train.bd.minority$`mortality`)
  
  
  x.train.minority = model.matrix(`mortality` ~ .^2,
                                  data = train.bd.minority)
  
  
  beta.offset = coef(glm.white, s = "lambda.min")
  mains = !str_detect(row.names(beta.offset), ":")
  beta.offset[mains] = 0
  offset.minority = x.train.minority %*% beta.offset[-1]
  offset.minority = as.numeric(offset.minority)
  
  x.train.minority = model.matrix(`mortality` ~ .,
                                  data = train.bd.minority)
  y.train.minority = train.bd.minority$mortality
  
  
  nfold <- 5
  set.seed(12121)
  # assign folds evenly using the modulus operator
  fold0 <- sample.int(sum(y.train.minority==0)) %% nfold
  fold1 <- sample.int(sum(y.train.minority==1)) %% nfold
  foldid <- numeric(length(y.train.minority))
  foldid[y.train.minority==0] <- fold0
  foldid[y.train.minority==1] <- fold1
  foldid <- foldid + 1
  
  
  
  
  glm.minority.offset.interaction = cv.glmnet(x.train.minority, y.train.minority, weights = weights, foldid = foldid, trace.it = T, nfolds = 5, type.measure = measure, family = 'binomial', parallel = TRUE, offset = offset.minority, keep = TRUE)
  rm('offset.minority')
  
  
  x.test.minority = model.matrix(`mortality` ~ .^2,
                                 data = test.bd.minority)
  
  
  
  offset.minority = x.test.minority %*% beta.offset[-1]
  offset.minority = as.numeric(offset.minority)
  
  
  x.test.minority = model.matrix(`mortality` ~ .,
                                 data = test.bd.minority)
  
  
  
  logit = predict(glm.minority.offset.interaction, newx = x.test.minority,
                  s = "lambda.min", type = "response", 
                  newoffset = offset.minority)
  y.pred = as.numeric(logit>0.5)
  y.true = test.bd.minority$`mortality`
  print(paste('Prediction summaries for', group, 'with interactions as offset from White:'))
  pred.offset = predict.accuracy(y.true, y.pred)
  auc.offset = assess.glmnet(logit, newy = y.true, family = "binomial")$auc
  
  train.major.offset.interaction.test.minor = c(pred.offset, auc.offset)
  
  
  #############################################################################
  #################### minority full model ####################################
  
  
  
  
  x.train.minority = model.matrix(`mortality` ~ .^2,
                                  data = train.bd.minority)
  
  y.train.minority = train.bd.minority$mortality
  
  glm.minority = cv.glmnet(x.train.minority, y.train.minority, weights = weights, foldid = foldid, trace.it = T, nfolds = 5, type.measure = measure, family = 'binomial', parallel = TRUE, keep = TRUE)
  
  x.test.minority = model.matrix(`mortality` ~ .^2,
                                 data = test.bd.minority)
  logit = predict(glm.minority, x.test.minority,
                  s = "lambda.min", type = "response")
  y.pred = as.numeric(logit>0.5)
  y.true = test.bd.minority$`mortality`
  print(paste('Prediction summaries for', group, 'with full estimated on the same data:'))
  pred.original = predict.accuracy(y.true, y.pred)
  auc.original = assess.glmnet(logit, newy = y.true, family = "binomial")$auc
  
  
  train.minor.full.test.minor = c(pred.original, auc.original)
  
  
  #############################################################################
  #################### minority main model ####################################
  
  
  x.train.minority = model.matrix(`mortality` ~ .,
                                  data = train.bd.minority)
  
  y.train.minority = train.bd.minority$mortality
  
  glm.minority.main = cv.glmnet(x.train.minority, y.train.minority, weights = weights, foldid = foldid, trace.it = T, nfolds = 5, type.measure = measure, family = 'binomial', parallel = TRUE, keep = TRUE)
  
  x.test.minority = model.matrix(`mortality` ~ .,
                                 data = test.bd.minority)
  logit = predict(glm.minority.main, x.test.minority,
                  s = "lambda.min", type = "response")
  y.pred = as.numeric(logit>0.5)
  y.true = test.bd.minority$`mortality`
  print(paste('Prediction summaries for', group, 'with only main effects estimated on the minority data:'))
  pred.no.interaction = predict.accuracy(y.true, y.pred)
  auc.no.interaction = assess.glmnet(logit, newy = y.true, family = "binomial")$auc
  
  
  train.minor.main.test.minor = c(pred.no.interaction, auc.no.interaction)
  
  #################################################
  ############ majority main effect model ################
  #################################################
  
  
  
  logit = predict(glm.white.main, x.test.minority,
                  s = "lambda.min", type = "response")
  y.pred = as.numeric(logit>0.5)
  print(paste('Prediction summaries for', group, 'with only main effects estimated on the majority data:'))
  pred.main = predict.accuracy(y.true, y.pred)
  auc.main = assess.glmnet(logit, newy = y.true, family = "binomial")$auc
  
  train.major.main.test.minor = c(pred.main, auc.main)
  
  #############################################################
  #############################################################
  
  minority.summary = rbind(train.major.full.test.minor, train.major.main.test.minor,
                           train.minor.full.test.minor, train.minor.main.test.minor,
                           train.major.offset.full.test.minor,
                           train.major.offset.main.test.minor,
                           train.major.offset.interaction.test.minor)
  
  minority.summary = as.data.frame(minority.summary)
  major = "White"
  minor = group
  
  colnames(minority.summary) = c('accuracy', 'TPR', 'TNR', 'auc')
  
  
  
  minority.summary = data.frame(minority.summary)
  
  minority.summary$bal.acc = (minority.summary$TPR + minority.summary$TNR)/2
  minority.summary$test.data = group
  
  minority.summary[['model']] = c(paste(major, '.full', sep = ""),
                                  paste(major, '.main', sep =""),
                                  paste(minor, '.full', sep = ""),
                                  paste(minor, '.main', sep =""),
                                  paste(major, '.offset.full', sep = ""),
                                  paste(major, '.offset.main', sep = ""),
                                  paste(major, '.offset.interaction', sep = ""))
  
  order.col = c("model", "test.data", "accuracy",
                "TPR", "TNR", "auc", "bal.acc" )
  minority.summary = minority.summary[, order.col]
  row.names(minority.summary) = NULL
  
  
  return(minority.summary)
  
  
  
}



sampling.cv <- function(y, seed = 0, cv = 5)
{
  y.0 = which(y == 0)
  y.1 = which(y == 1)
  y.0 = sample(y.0, length(y.0))
  y.1 = sample(y.1, length(y.1))
  n.0 = floor(length(y.0)/cv)
  n.1 = floor(length(y.1)/cv)
  test.ind = list()
  for(i in 1:cv)
  {
    ind.0 = ((i-1)*n.0 + 1):(i*n.0)
    ind.1 = ((i-1)*n.1 + 1):(i*n.1)
    test.ind[[i]] = c(y.0[ind.0], y.1[ind.1])
  }
  
  return(test.ind)
}










minority.evaluation.all <- function(ethnicity = "Asian", bd.minority, cv = 5)
{
  test.ind = sampling.cv(bd.minority$mortality, cv = cv)
  test.summary = list()
  for(i in 1:length(test.ind))
  {
    print(paste('running cv', i, '...'))
    test.summary.temp = minority.evaluation.cv(ethnicity = ethnicity, 
                                               bd.minority = bd.minority,
                                               test_ind = test.ind[[i]])
    test.summary.temp[['cv']] = i
    test.summary[[i]] = test.summary.temp
  }
  ret.summary = test.summary[[1]]
  for(i in 2:length(test.summary))
    ret.summary = rbind(ret.summary, test.summary[[i]])
  return(ret.summary)
}
