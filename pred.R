pred_Poke <- function(dt){
  library(data.table)
  library(gtools)
  library(ranger)
  
  data0 <- read.csv(file = dt, header = TRUE, stringsAsFactors = FALSE)
  
  
  # pokemonId and class are identical, so delete pokemonId
  data0 <- subset(data0, select = -pokemonId)
  data0 <- subset(data0, select = -X_id)
  
  # change class into factor
  data0$class <- as.factor(data0$class)
  
  # urban, suburban, midurban, rural: combine them into 1 variable (urban_rural) with four categories
  data0$urban_rural <- ifelse(data0$urban, 'urban', 
                              ifelse(data0$suburban, 'suburban',
                                     ifelse(data0$midurban, 'midurban', 'rural')))
  data0 <- subset(data0, select = -c(urban, suburban, midurban, rural))
  
  
  data0 <- as.data.table(data0)
  
  
  
  
  # gym
  data0$gym <- ifelse(data0$gymIn100m, '0-100m', ifelse(data0$gymIn250m, '100-250m', ifelse(data0$gymIn500m, '250-500m', ifelse(data0$gymIn1000m,'500-1000m', ifelse(data0$gymIn2500m,'1000-2500m', ifelse(data0$gymIn5000m, '2500-5000m', '>5000m'))))))
  
  
  # poke Stop
  data0$pokestop <- ifelse(data0$pokestopIn100m, '0-100m', ifelse(data0$pokestopIn250m, '100-250m', ifelse(data0$pokestopIn500m, '250-500m', ifelse(data0$pokestopIn1000m,'500-1000m', ifelse(data0$pokestopIn2500m,'1000-2500m', ifelse(data0$pokestopIn5000m, '2500-5000m', '>5000m'))))))
  
  # dummy_day to Monday
  data0$appearedDayOfWeek <- ifelse(data0$appearedDayOfWeek == 'dummy_day', 'Monday',data0$appearedDayOfWeek)
  
  
  # weatherIcon
  data0$weatherIcon <- ifelse(data0$weatherIcon %in% c('clear-day', 'clear-night'), 'clear', ifelse(data0$weatherIcon %in% c('partly-cloudy-day','partly-cloudy-night'), 'partly-cloudy', data0$weatherIcon))
  
  
  col2del <- c('appearedLocalTime','appearedDay','appearedMonth','appearedYear','gymIn100m','gymIn250m','gymIn500m','gymIn1000m','gymIn2500m','gymIn5000m','pokestopIn100m','pokestopIn250m','pokestopIn500m','pokestopIn1000m','pokestopIn2500m','pokestopIn5000m','appearedTimeOfDay')
  data_anal <- copy(data0)
  data_anal <- data_anal[,(col2del) := NULL]
  
  
  
  # class into character
  data_anal$class <- as.character(data_anal$class)
  
  # terrainType to character
  data_anal$terrainType <- as.character(data_anal$terrainType)
  
  # log(population_density)
  data_anal$population_density <- log(data_anal$population_density + 1)
  
  
  
  
  # change pokestopDistanceKm and gymDistanceKm into numeric
  data_anal$gymDistanceKm <- as.numeric(data_anal$gymDistanceKm)
  data_anal$pokestopDistanceKm <- as.numeric(data_anal$pokestopDistanceKm)
  
  # reorder columns
  setcolorder(data_anal, c(185, 1:184, 186:188))
  
  # change character as factor
  data_anal <- as.data.frame(data_anal)
  nm_char <- lapply(data_anal, class) == 'character'
  
  data_anal[,nm_char] <- lapply(data_anal[,nm_char], as.factor)
  
  # delete more cols
  data0 <- copy(data_anal)
  col2del <- c('cooc_68',"cooc_132","cooc_144", "cooc_145", "cooc_146", "cooc_150",'cooc_151')
  
  data0 <- as.data.table(data0)
  data0 <- data0[,(col2del) := NULL]
  data0 <- data0[,c('cellId_180m','cellId_370m','cellId_730m','cellId_1460m','cellId_2920m','cellId_5850m') :=NULL]
  
  data_test <- copy(data0)
  
  
  load('data_1.RData')
  
  data0 <- na.omit(data0)
  dt_all <- smartbind(data0, data_test)
  
  
  
  # create dummy variables
  dt_all <- as.data.frame(dt_all)
  #dt_all <- na.omit(dt_all)
  class <- dt_all$class
  
  
  dt_all <- model.matrix(class ~., data=dt_all)
  dt_all <- dt_all[,-1]
  dt_all <- as.data.frame(dt_all)
  dt_all <- cbind(class, dt_all)
  
  dt_test <- dt_all[(dim(data0)[1] + 1): dim(dt_all)[1],]
  
  rm(dt_all,data_anal,data_test)
  # add in newclass
  load('class_2_new.RData')
  
  dt_test$newclass <- sapply(dt_test$class, function(x){return(class_to_cluster[x])})
  
  dt_test <- dt_test[, -1]
  
  tst_x <- dt_test[,-337]
  tst_y <- dt_test[,'newclass']
  
  # load RF
  load('RF0_Final.RData')
  
  pred <- predictions(predict(rf0, data=tst_x))
  
  Accuracy <- sum(pred == tst_y)/length(tst_y)
  
  return(list(Accuracy = Accuracy, Prediction = pred, y = tst_y, Table = table(Predicted = pred, Observed = tst_y)))
  
  
}

