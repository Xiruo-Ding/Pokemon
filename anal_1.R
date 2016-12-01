library(nnet)
library(glmnet)
library(caret)
library(data.table)
library(parallel)
library(doMC)
library(knncat)
library(cluster)
library(dummy)
library(foreach)
library(doParallel)
library(randomForest)
library(ranger)


#setwd("~/Dropbox/707/HW/TeamReport")

# data_anal.RData is the data for analysis
# the pre-processing step is in dingxir_TR0.R
#load('~/Documents/707_Data/data_anal.Rdata')

# setwd("P:/TeamReport")
load('P://TeamReport/707_Data_out/data_anal.RData')


# knncat
# data0 <- copy(data_anal)
# nm_less100 <- names(table(data_anal$class))[table(data_anal$class) < 100]
# data0 <- data0[class %in% nm_less100, class := 'other']

# idx <- sample(1:nrow(data0), 200000)
# idx2 <- setdiff(1:290000, idx)
# KNN_test <- knncat(data0[idx,-'class',with=FALSE], data0[idx2,-'class', with=FALSE], k = 2)
# 


# delete only 1 level
data0 <- copy(data_anal)
col <- lapply(data_anal, function(x) if(is.factor(x)){return(length(levels(x)) == 1)}else{return(FALSE)})
col <- unlist(col)
col2del <- names(data0)[col]

data0 <- as.data.table(data0)
data0 <- data0[,(col2del) := NULL]
data0 <- data0[,c('cellId_180m','cellId_370m','cellId_730m','cellId_1460m','cellId_2920m','cellId_5850m') :=NULL]

# save(data0, file = '707_Data_out/data_1.RData')
# load('707_Data_out/data_1.RData')


# k medoids
data0 <- as.data.frame(data0)
data0 <- na.omit(data0)
class <- data0$class

data0 <- model.matrix(class ~., data=data0)
data0 <- data0[,-1]
data0 <- as.data.frame(data0)
data0 <- cbind(class, data0)


kmedois <- clara(data0[,-1], 40, metric = 'hamming', keep.data = FALSE, medoids.x = FALSE)
# save(kmedois, file='707_Data_out/kmedois.RData')

k40_table <- table((kmedois$clustering), data0$class)
k40_table <- as.data.frame.matrix(k40_table)


class_to_cluster <- sapply((k40_table), function(x) return(which.max(x)))

data0$newclass <- sapply(data0$class, function(x){return(class_to_cluster[x])})

# save(class_to_cluster, file='class_2_new.RData')
# save(data0, file = '707_Data_out/data_2.RData')

data0 <- data0[,-1]

# sample 50000
set.seed(123)
dt1_idx <- sample(1:nrow(data0), 50000)
other_idx <- setdiff(1:nrow(data0), dt1_idx)
data1 <- data0[dt1_idx, ]

set.seed(1234)
idx <- sample(1:nrow(data1), 40000)
idx2 <- setdiff(1:nrow(data1), idx)


t1 <- proc.time()
set.seed(1234)
registerDoParallel(6)
rf <- foreach(ntree = rep(500), .combine = combine, .multicombine = TRUE, .packages = 'randomForest') %dopar% {
  randomForest(data0[idx, -337], y = as.factor(data0[idx, 337]), ntree = ntree)
  
}
proc.time() - t1

# save(rf, file = 'rf_null.RData')

data_test <- data0[other_idx,]
data_test_id2 <- data1[idx2,]

# sum(data1[idx,'newclass'] != rf$predicted)/40000

# ts <- predict(rf, newdata = data_test, type = 'class')
# sum(data_test[,'newclass'] != ts_idx2)/nrow(data_test)

######      PCA

data1_pca <- data1[,c('latitude', 'longitude', 'cellId_90m','appearedHour','appearedMinute','temperature','windSpeed','windBearing','pressure','sunriseMinutesMidnight','sunriseHour','sunriseMinute','sunriseMinutesSince','sunsetMinutesMidnight','sunsetHour','sunsetMinute','sunsetMinutesBefore','population_density', 'gymDistanceKm','pokestopDistanceKm' )]
pca_1 <- prcomp(data1_pca, center = TRUE, scale. = TRUE)

summary(pca_1)

pca_var <- pca_1$x[,1:17]

############## On New Set
data_add <- read.csv('P://TeamReport/707_Data_out/new.csv')

save(data_share, file = "P://TeamReport/707_Data_out/Data_Share_All.RData")


set.seed(1234)

rf <- ranger(class ~., data=data0[,-338])

save(rf, file = 'C://Users/xd21/Desktop/RF_Final.RData')

############ On whole data RF
load('data_2.RData')
data0 <- data0[, -1]

rf0 <- ranger(as.factor(newclass) ~., data=data0)

save(rf0, file = 'RF0_Final.RData')


tst_x <- data0[1:10,-337]
tst_y <- data0[1:10,'newclass']

pred <- predictions(predict(rf0, data=tst_x))
