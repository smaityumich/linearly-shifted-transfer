

################################################################
################################################################


path = '~/projects/posterior-drift/UKBB/'
load(file = paste(path, "rdata/processed.data.RData", sep =""))
load(file = paste(path, "rdata/rf.white.RData", sep =""))

source(file = paste(path, 'RF_utils.R', sep = ''))
source(file = paste(path, 'utils.R', sep = ''))


######################
#### white prediction ####

library(randomForest)
library(caret)
library(pROC)


test.df = rbind(test.bd.white, bd.asian)
index.test = c(rep('white', nrow(test.bd.white)), rep('asian', nrow(bd.asian)))


y.test = test.df$mortality
x.test = subset(test.df, select = -c(`mortality`))


y.true = test.bd.white$mortality
y.pred = as.numeric(rf.white.votes[index.test == 'white'] > 0.5)

white.white.summary = predict.accuracy(y.true = y.true, y.pred = y.pred)




test.bd.white = data.frame(test.bd.white)
y_true = as.factor(test.bd.white$mortality)
y_pred = rf.white.votes[index.test == 'white'] > 0.5
y_pred = as.factor(as.numeric(y_pred))


cm = confusionMatrix(data = y_pred, reference = y_true)
white.white.bc = cm$byClass


white.white.roc = roc(response = y_true, predictor = rf.white.votes[index.test == 'white'])
white.white.accuracy = mean(y_true == y_pred)



############################################################
############################################################

ind.asian = which(index.test == 'asian')
ind.asian.train = ind.asian[train_ind]
ind.asian.test = ind.asian[-train_ind]


y_true = as.factor(test.bd.asian$mortality)
y_pred = rf.white.votes[ind.asian.test] > 0.5
y_pred = as.factor(as.numeric(y_pred))

cm = confusionMatrix(data = y_pred, reference = y_true)
white.asian.bc = cm$byClass


white.asian.roc = roc(response = y_true, predictor = rf.white.votes[ind.asian.test])
white.asian.accuracy = mean(y_true == y_pred)
white.asian.summary = predict.accuracy(test.bd.asian$mortality, as.numeric(rf.white.votes[ind.asian.test] > 0.5))


##############################################################
##############################################################

load(file = paste(path, "rdata/rf.asian.RData", sep =""))


y_true = as.factor(test.bd.asian$mortality)
y_pred = rf.asian.votes > 0.5
y_pred = as.factor(as.numeric(y_pred))

cm = confusionMatrix(data = y_pred, reference = y_true)
asian.asian.bc = cm$byClass

asian.asian.roc = roc(response = y_true, predictor = rf.asian.votes)
asian.asian.accuracy = mean(y_true == y_pred)
asian.asian.summary = predict.accuracy(test.bd.asian$mortality, as.numeric(rf.asian.votes>0.5))

##############################################################
##############################################################

p.asian.train = rf.white.votes[ind.asian.train]
offset.asian.train = log((p.asian.train/(1-p.asian.train)))

x.train.asian = model.matrix(`mortality` ~ .-1,
                                data = train.bd.asian)
y.train.asian = train.bd.asian$mortality
weights = get.weights(train.bd.asian$`mortality`)

nfold <- 5
set.seed(12121)
# assign folds evenly using the modulus operator
fold0 <- sample.int(sum(y.train.asian==0)) %% nfold
fold1 <- sample.int(sum(y.train.asian==1)) %% nfold
foldid <- numeric(length(y.train.asian))
foldid[y.train.asian==0] <- fold0
foldid[y.train.asian==1] <- fold1
foldid <- foldid + 1




glm.asian.offset = cv.glmnet(x.train.asian, y.train.asian, weights = weights,
                             foldid = foldid, trace.it = T, nfolds = 5,
                             type.measure = 'default', family = 'binomial',
                             parallel = TRUE,
                             offset = offset.asian.train,
                             keep = TRUE)



p.asian.test = rf.white.votes[ind.asian.test]
offset.asian.test = log((p.asian.test/(1-p.asian.test)))

x.test.asian = model.matrix(`mortality` ~ .-1,
                               data = test.bd.asian)



p = predict(glm.asian.offset, newx = x.test.asian,
                s = "lambda.min", 
                type = "response", 
                newoffset = offset.asian.test,
            )

y.pred = as.numeric(p>0.5)
y_pred = as.factor(y.pred)
y_true = as.factor(test.bd.asian$mortality)


cm = confusionMatrix(data = y_pred, reference = y_true)
transfer.bc = cm$byClass


transfer.roc = roc(response = y_true, predictor = as.numeric(p))
transfer.accuracy = mean(y_true == y_pred)
transfer.summary = predict.accuracy(test.bd.asian$mortality, as.numeric(p>0.5))



#############################################################
#############################################################

models = c('rf.White', 'rf.White', 'rf.Asian', 'rf.transfer')
train.data = c('White', 'White', 'Asian', 'White/Asian')
test.data = c('White', 'Asian', 'Asian', 'Asian' )

accuracy = c(white.white.summary[1], white.asian.summary[1],
        asian.asian.summary[1], transfer.summary[1])

TPR = c(white.white.summary[2], white.asian.summary[2],
        asian.asian.summary[2], transfer.summary[2])


TNR = c(white.white.summary[3], white.asian.summary[3],
        asian.asian.summary[3], transfer.summary[3])


AUC = c(white.white.roc$auc, white.asian.roc$auc,
        asian.asian.roc$auc, transfer.roc$auc)


summary.rf = data.frame(models = models, 
                        'train-data' = train.data,
                        test.data = test.data,
                        accuracy = as.numeric(accuracy),
                        TPR = as.numeric(TPR), 
                        TNR = as.numeric(TNR), 
                        AUC = as.numeric(AUC))
summary.rf$`bal-acc` = (summary.rf$TPR + summary.rf$TNR)/2
summary.rf


xtable::xtable(summary.rf, digits = 3)


save(glm.asian.offset, file = paste(path, "rdata/rf.transfer.RData", sep =""))



##########################
##########################
## test for mortality prop diff ####
p1 = mean(bd.white$mortality)
p2 = mean(bd.asian$mortality)
n1 = nrow(bd.white)
n2 = nrow(bd.asian)

T.stat = (p1 - p2)/sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
p.value = 2 * pnorm(abs(T.stat), lower.tail = F)