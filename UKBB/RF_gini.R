# args = commandArgs(trailingOnly=TRUE)
# iter = as.integer(args[1])

path = '~/projects/posterior-drift/UKBB/'
load(file = paste(path, "rdata/processed.data.RData", sep =""))


iterations = 5000
ntree = 1000

options(warn = -1)

set.seed(0)

library(doMC)
numCores = 25
registerDoMC(numCores)
library(randomForest)
library(progress)
library(foreach)



train.bd.white$mortality = as.factor(train.bd.white$mortality)
train.bd.white = data.frame(train.bd.white)
train.df = train.bd.white

# y.train = train.df$mortality
# n0 = sum(y.train == 0)
# n1 = sum(y.train == 1)
# ind.0 = which(y.train == 0)
# ind.1 = which(y.train == 1)
# train.df = train.df[c(ind.0, rep(ind.1, floor(n0/n1))), ]



y.train = train.df$mortality
n1 = min(sum(y.train == 0), sum(y.train == 1))
# weights = (y == 0)/mean(y == 0) + (y == 1)/mean(y == 1)
classwt = c(1/mean(y.train == 0), 1/mean(y.train == 1))
x.train = subset(train.df, select = -c(`mortality`))



test.df = rbind(test.bd.white, bd.asian)
index.test = c(rep('white', nrow(test.bd.white)), rep('asian', nrow(test.bd.white)))

test.df$mortality = as.factor(test.df$mortality)
test.df = data.frame(test.df)

y.test = test.df$mortality
x.test = subset(test.df, select = -c(`mortality`))




pb = txtProgressBar(min = 0, max = iterations, style = 3)



rf.white <- foreach(i=1:iterations, .packages='randomForest') %dopar% {

                            set.seed(i)
                            print(paste('White: Running tree', i, '...'))
                            #setTxtProgressBar(pb, i)
                            rf = randomForest(x = x.train, y = y.train,
                                              xtest = x.test,
                                              ytest = y.test,
                                              sampsize=c(n1,n1),
                                              norm.votes = T,
                                              do.trace = F, ntree = ntree)
                            test.votes = rf$test$votes / apply(rf$test$votes, 1, sum)
                            test.votes = as.numeric(test.votes[, 2])
                            return(list(test.votes = test.votes, importance = rf$importance))
                            # rf.votes = rf$test$votes
                            # rf.votes = rf.votes/apply(rf.votes, 1, sum)
                            # rf.white.votes = rf.white.votes + rf.votes

}


rf.white.votes = rf.white[[1]]$test.votes
rf.white.importance = rf.white[[1]]$importance
for(i in 2:length(rf.white))
{
  rf.white.votes = rf.white.votes + rf.white[[i]]$test.votes
  rf.white.importance = rf.white.importance + rf.white[[i]]$importance
}
rf.white.votes = rf.white.votes/iterations
rf.white.importance = rf.white.importance/iterations
save(list = c('rf.white.votes', 'rf.white.importance'), file = paste(path, "rdata/rf.white",  ".RData", sep =""))

rm(rf.white)





#############################################################
#############################################################


train.bd.asian$mortality = as.factor(train.bd.asian$mortality)
train.bd.asian = data.frame(train.bd.asian)
train.df = train.bd.asian




y.train = train.df$mortality
n_min = min(sum(y.train == 0), sum(y.train == 1))
n_max = max(sum(y.train == 0), sum(y.train == 1))
# weights = (y == 0)/mean(y == 0) + (y == 1)/mean(y == 1)
classwt = c(1/mean(y.train == 0), 1/mean(y.train == 1))
cutoff = c(mean(y.train == 0), mean(y.train == 1))
x.train = subset(train.df, select = -c(`mortality`))



test.df = test.bd.asian

test.df$mortality = as.factor(test.df$mortality)
test.df = data.frame(test.df)

y.test = test.df$mortality
x.test = subset(test.df, select = -c(`mortality`))



pb = txtProgressBar(min = 0, max = iterations, style = 3)



library(foreach)






rf.asian <- foreach(i=1:iterations, .packages='randomForest') %dopar% {
                            
                            set.seed(i)
                            print(paste('Asian: Running tree', i, '...'))
                            
                            rf = randomForest(x = x.train, y = y.train, 
                                              xtest = x.test,
                                              ytest = y.test,
                                              #cutoff = cutoff,
                                              sampsize=c(n_min,  n_min),
                                              norm.votes = T, 
                                              do.trace = F, ntree = ntree)
                            
                            test.votes = rf$test$votes / apply(rf$test$votes, 1, sum)
                            test.votes = as.numeric(test.votes[, 2])
                            return(list(test.votes = test.votes, importance = rf$importance))

                          }


rf.asian.votes = rf.asian[[1]]$test.votes
rf.asian.importance = rf.asian[[1]]$importance
for(i in 2:length(rf.asian))
{
  rf.asian.votes = rf.asian.votes + rf.asian[[i]]$test.votes
  rf.asian.importance = rf.asian.importance + rf.asian[[i]]$importance
}
rf.asian.votes = rf.asian.votes/iterations
rf.asian.importance = rf.asian.importance/iterations


save(list = c('rf.asian.votes', 'rf.asian.importance'), file = paste(path, "rdata/rf.asian",  ".RData", sep =""))

rm(rf.asian)


