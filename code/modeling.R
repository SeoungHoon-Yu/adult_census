pkgs <- c('dplyr','caret','data.table',
          'rlist')
sapply(pkgs,require,character.only = TRUE)

load('D:/github_desktop/adult_census/adult_census/data/adult.RData')
colnames(adult) <- colnames(adult) %>% tolower()

adult$income_condition <- (as.numeric(adult$income_condition)-1) %>% as.factor()

# custom function ####
# dummy
dum <- function(x,name = 'dummy'){
  uni <- length(unique(x))
  a <- matrix(nrow = length(x),ncol = uni)
  for(i in 1:uni){
    a[,i] <- ifelse(x == unique(x)[i],1,0)
  }
  return(a)
}

# 0 to 1
normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

# classification performance
perf <- function(pred,real){
  if(length(pred) != length(real)){
    warning('No match in length',
            '\n',paste0('pred_length : ',length(pred)),
            '\n',paste0('real_length : ',length(real)))
    break()}
  pred <- as.character(pred) %>% as.numeric()
  real <- as.character(real) %>% as.numeric()
  sensitivity <- sum(pred == 1 & real == 1) / sum(real == 1)
  specificity <- sum(pred == 0 & real == 0) / sum(real == 0)
  accuracy    <- mean(pred == real)
  mis_class <- sum(pred != real) / length(pred)
  (perform <- data.frame(sensitivity,specificity,accuracy,mis_class))
}

# data partition ####
set.seed(2018)
idx <- sample(c('train','valid','test'),
              size = nrow(adult),
              prob = c(6,2,2),replace = TRUE)
train <- adult[idx == 'train',]
valid <- adult[idx == 'valid',]
test  <- adult[idx == 'test' ,]
train.y <- adult$income_condition[idx == 'train']

# 1. knn ####
library(kknn)
knntr <- train.kknn(income_condition ~.,train,kmax = 15,
                    distance = 2,kernel = 'rectangular')

# knn best k = 14
md_knn <- kknn(income_condition~.,train,test,
               k = knntr$best.parameters$k,kernel = 'rectangular',
               distance = 2)
pr_knn <- md_knn$fitted.values

# test dataset performance
(perf_knn <- perf(pr_knn,test$income_condition))

# 2. decision tree ####
# min_criterion = 0.01 through caret package.
library(party)
trc <- trainControl(method = 'cv',number = 10,
                    verboseIter = TRUE)
tmd <- train(income_condition~.,data = train,
             trControl = trc,method = 'ctree')

md_tree <- ctree(income_condition ~., data = train,
                 controls = ctree_control(mincriterion = 0.01))
pr_tree <- predict(md_tree,test)
(perf_tree <- perf(pr_tree,test$income_condition))
plot(md_tree)

# ROC curve
library(ROCR)
res <- treeresponse(md_tree,test)
res <- 1-unlist(res, use.names=F)[seq(1,nrow(test)*2,2)]
res.rocr <- prediction(res,test$income_condition)
res.perf <- performance(res.rocr,'tpr','fpr')
plot(res.perf,col = 2,lwd = 3)
lines(x = c(0,1),y = c(0,1),lwd = 2)

# 3. naive bayes ####
library(e1071)
md_naive <- naiveBayes(income_condition ~., data = train)

pr_naive <- predict(md_naive,test,type = 'class')
(perf_naive <- perf(pr_naive,test$income_condition))

# 4. logistic regression ####
# full variable
md_glm_full <- glm(income_condition ~.,family = binomial,data = train)
summary(md_glm_full)
pr_glm_full <- predict(md_glm_full,test,type = 'response')
pr_glm_full <- ifelse(pr_glm_full > 0.5,1,0)
(perf_glm_full <- perf(pr_glm_full,test$income_condition))

# selected variable backward
md_glm_back <- step(md_glm_full,direction = 'backward')
summary(md_glm_back)
pr_glm_back <- predict(md_glm_back,test,type = 'response')
pr_glm_back <- ifelse(pr_glm_back >= 0.5,1,0)
(perf_glm_back <- perf(pr_glm_back,test$income_condition))

# 5. neuralnet ####
# find best parameter
library(nnet)
aa <- list()
for(i in 1:10){
  md <- nnet(income_condition ~.,
             data = train,size = i,decay = 5e-4, maxit = 500)
  pre <- predict(md,valid)
  pre <- ifelse(pre > 0.5,1,0)
  aa[[i]] <- perf(pre,valid$income_condition)
}
aa <- rbindlist(aa) %>% data.frame()


trc <- trainControl(method = 'cv',
                    number = '10')
md_neur_caret <- train(income_condition~.,data = train,method = 'nnet')
md_neur_nnet <- nnet(income_condition ~.,
                   data = train,size = 5,decay = 5e-4,maxit = 500)

pre_caret <- predict(md_neur_caret,test)
pre_nnet <- predict(md_neur_nnet,test,type = 'class')
