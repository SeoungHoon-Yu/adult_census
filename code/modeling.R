pkgs <- c('dplyr','caret','data.table')
sapply(pkgs,require,character.only = TRUE)

load('D:/github_desktop/adult_census/adult_census/data/adult.RData')
colnames(adult) <- colnames(adult) %>% tolower()

adult$income_condition <- (as.numeric(adult$income_condition)-1) %>% as.factor()
adult <- adult[,-12]
adult$group <- as.factor(adult$group)

# custom function ####
dum <- function(x,name = 'dummy'){
  uni <- length(unique(x))
  a <- matrix(nrow = length(x),ncol = uni)
  for(i in 1:uni){
    a[,i] <- ifelse(x == unique(x)[i],1,0)
  }
  assign(paste0('dummy_',name),a,envir = .GlobalEnv)
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
knntr <- train.kknn(income_condition ~.,train,kmax = 15,distance = 2,kernel = 'rectangular')

# knn best k = 12
md_knn <- kknn(income_condition~.,train,test,
               k = knntr$best.parameters$k,kernel = 'rectangular',distance = 2)
pr_knn <- md_knn$fitted.values

# test dataset performance
(perf_knn <- perf(pr_knn,test$income_condition))

# 2. decision tree ####
# min_criterion = 0.5 through caret package.
tra <- data.frame(
min_criterion = rep(0.5,12),
min_split = rep(c(10, 30, 50, 100),each = 3),
max_depth = rep(c(0, 10, 5),4))
tretr <- list()

for(i in 1:12){
    cat("CART Min criterion:", tra[i,1], ", Min split:", tra[i,2], 
        ", Max depth:", tra[i,3], "\n")
      
    tmp_control = ctree_control(mincriterion = tra[i,1], 
                                  minsplit = tra[i,2], maxdepth = tra[i,3])
    tmp_tree <- ctree(income_condition ~ ., data = train, controls = tmp_control)
    tmp_tree_pre <- predict(tmp_tree,valid)
    tretr[[i]] <- perf(tmp_tree_pre,valid$income_condition)
    }
tem <- rbindlist(tretr) %>% data.frame()

# best 
# 0.5, 10, 0
md_tree <- ctree(income_condition ~., data = train,controls = ctree_control(mincriterion = 0.5,
                                                                         minsplit = tra[1,2],
                                                                         maxdepth = tra[1,3]))
pr_tree <- predict(tree,test)
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
pr_glm_full <- predict(md_glm_full,valid,type = 'response')
pr_glm_full <- ifelse(pr_glm_full > 0.5,1,0)
(perf_glm_full <- perf(pr_glm_full,valid$income_condition))

# selected variable backward
md_glm_back <- step(md_glm_full,direction = 'backward')
summary(md_glm_back)
pr_glm_back <- predict(md_glm_back,valid[,names(md_glm_back$data)],type = 'response')
pr_glm_back <- ifelse(pr_glm_back > 0.5,1,0)
(perf_glm_back <- perf(pr_glm_back,valid$income_condition))

# selected variable both
md_glm_both <- step(md_glm_full,direction = 'both')
summary(md_glm_both)
pr_glm_both <- predict(md_glm_both,valid[,names(md_glm_both$data)],type = 'response')
pr_glm_both <- ifelse(pr_glm_both > 0.5,1,0)
(perf_glm_both <- perf(pr_glm_both,valid$income_condition))

# selected variable forward
md_glm_forw <- step(md_glm_full,direction = 'forward')
summary(md_glm_forw)
pr_glm_forw <- predict(md_glm_forw,valid[,names(md_glm_forw$data)],type = 'response')
pr_glm_forw <- ifelse(pr_glm_both > 0.5,1,0)
(perf_glm_forw <- perf(pr_glm_forw,valid$income_condition))

# 5. neuralnet ####



