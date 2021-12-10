path = '~/projects/posterior-drift/UKBB/'

#### load and clean data
source(paste(path, "mortality.r", sep = ""))
bd = clean.data()


#### remove all entries with NA

bd = bd[complete.cases(bd), ]


#### load necessary functions and packages 

source(paste(path, "utils.R", sep = ""))


######## remove pcs ##########
ind.pc = stringr::str_detect(colnames(bd), "PC")
bd = bd[, !ind.pc]







#################################################
#################################################
######### train test for white ##################


train_ind <- sampling.index(bd.white$mortality)

train.bd.white <- bd.white[train_ind, ]
test.bd.white <- bd.white[-train_ind, ]

save.image(file = paste(path, "rdata/processed.data.RData", sep =""))


############################################
############# CHECKPOINT ###################
############################################

# path = '~/projects/posterior-drift/UKBB/'
# load(file = paste(path, "rdata/processed.data.RData", sep =""))


########################################################
####### load packages, set weights, x, y, folds ########

library(glmnet)
library(doParallel)
registerDoParallel(20)

weights = get.weights(train.bd.white$`mortality`)


y <- train.bd.white$mortality


nfold <- 5
set.seed(12122)
# assign folds evenly using the modulus operator
fold0 <- sample.int(sum(y==0)) %% nfold
fold1 <- sample.int(sum(y==1)) %% nfold
foldid <- numeric(length(y))
foldid[y==0] <- fold0
foldid[y==1] <- fold1
foldid <- foldid + 1


################################################
#### set x with interaction and fit glmnet ###


path = '~/projects/posterior-drift/UKBB/'
load(file = paste(path, "rdata/white.full.model.RData", sep =""))

measure = "class"

x <- model.matrix(`mortality` ~ .^2 , data = train.bd.white)

glm.white = cv.glmnet(x, y, weights = weights, foldid = foldid,
                    trace.it = T, nfolds = 5,
                    type.measure = measure,
                    family = 'binomial',
                    parallel = TRUE, keep = TRUE)


save('glm.white', file = paste(path, "rdata/white.full.model.RData", sep =""))



#####################################################
############### model fit for main effect ###########

x <- model.matrix(`mortality` ~ . , data = train.bd.white)

glm.white.main = cv.glmnet(x, y, weights = weights, foldid = foldid, 
                      trace.it = T, nfolds = 5, 
                      type.measure = measure, 
                      family = 'binomial', 
                      parallel = TRUE, keep = TRUE)




############################################
############# CHECKPOINT ###################
############################################


#########################################################
############ saving fitted white models #################
save.image(paste(path, "rdata/class.white.full.RData", sep =""))


##########################################################
############# load fitted white model ####################

path = "~/projects/posterior-drift/UKBB/"
load(paste(path, "rdata/class.white.full.RData", sep =""))


###########################################################
############# majority model evaluation ###################



library(stringr)
library(glmnet)
library(doParallel)
registerDoParallel(20)



white.summary = as.data.frame(matrix(0, 2, 4))
colnames(white.summary) = c('accuracy', 'TPR', 'TNR', 'auc')


x.test = model.matrix(`mortality` ~ .^2, data = test.bd.white)
y.pred = as.numeric(predict(glm.white, x.test, s = 'lambda.min', type = 'link')>0)
y.true = test.bd.white$`mortality`
pred = predict.accuracy(y.true, y.pred)
auc.white = assess.glmnet(predict(glm.white, x.test, s= "lambda.min"), newy = y.true, family = "binomial")$auc

white.summary[1, ] = c(pred, auc.white) 


x.test = model.matrix(`mortality` ~ ., data = test.bd.white)
y.pred = as.numeric(predict(glm.white.main, x.test, s = 'lambda.min', type = 'link')>0)
pred.main = predict.accuracy(y.true, y.pred)
auc.white.main = assess.glmnet(predict(glm.white.main, x.test, s= "lambda.min"), newy = y.true, family = "binomial")$auc

white.summary[2, ] = c(pred, auc.white.main) 

white.summary$bal.acc = (white.summary$TPR + white.summary$TNR)/2
white.summary$test.data = 'white'
white.summary$model = c("white.full", "white.main")

order.col = c("model", "test.data", "accuracy",
              "TPR", "TNR", "auc", "bal.acc" )
white.summary = white.summary[, order.col]

########################################################################
################### minority evaluations ###############################


source(paste(path, "minority.R", sep =""))



bd.asian = bd.sel[bd.sel$`Ethnic background` == "Asian or Asian British", ]
bd.asian = bd.asian[, colnames(bd.asian) != "Ethnic background"]

asian.return = minority.evaluations(ethnicity = "Asian", bd.asian)
asian.summary = asian.return$minority.summary

summary = rbind(white.summary, asian.summary)


betas = list()

betas[['white.full']] = coef(glm.white, s0 = "lambda.min")
betas[['white.main']] = coef(glm.white.main, s0 = "lambda.min")
betas[['asian.full']] = coef(asian.return$minority.full, s0 = "lambda.min")
betas[['asian.main']] = coef(asian.return$minority.main, s0 = "lambda.min")
betas[['offset.full']] = coef(asian.return$offset.full, s0 = "lambda.min")
betas[['offset.main']] = coef(asian.return$offset.main, s0 = "lambda.min")
betas[['offset.interaction']] = coef(asian.return$offset.interaction, s0 = "lambda.min")

full.length = dim(betas$white.full)[1]
main.length = dim(betas$white.main)[1]

beta.matrix = matrix(0, full.length-1, 7)
beta.matrix = as.data.frame(beta.matrix)
ROWS = row.names(betas$white.full)
row.names(beta.matrix) = ROWS[-2]
colnames(beta.matrix) = names(betas)

for(beta.name in names(betas))
{
  BETA = betas[[beta.name]]
  BETA.vars = rownames(BETA)
  ind.intercept = which(BETA.vars == "(Intercept)")
  if(length(ind.intercept)>2)
  {
    i = which(BETA[ind.intercept] == 0)
    to.delete = ind.intercept[i]
    BETA = BETA[-to.delete,]
  }
  beta.matrix[row.names(BETA), beta.name] = BETA
}

write.csv(beta.matrix, file = paste(path, "summary/beta.csv", sep =""))