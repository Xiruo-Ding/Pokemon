###########################
###  Team Report TR0    ###
###########################

library(ggplot2)
library(GGally)
library(reshape2)
library(cluster)
library(psych)
library(data.table)
library(parallel)

#setwd("~/Dropbox/707/HW/TeamReport")
setwd("P:/TeamReport")


# 208 variables in the original data
#data0 <- read.csv(file = '~/Documents/707_Data/train.csv', header = TRUE, stringsAsFactors = FALSE)
data0 <- read.csv(file = '707_Data_out/train.csv', header = TRUE, stringsAsFactors = FALSE)


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




#save(data0, file = '~/Documents/707_Data/data0.RData')
save(data0, file = '707_Data_out/data0.RData')


# delete variable
col2del <- c('appearedLocalTime','appearedDay','appearedMonth','appearedYear','gymIn100m','gymIn250m','gymIn500m','gymIn1000m','gymIn2500m','gymIn5000m','pokestopIn100m','pokestopIn250m','pokestopIn500m','pokestopIn1000m','pokestopIn2500m','pokestopIn5000m','appearedTimeOfDay')
data_anal <- copy(data0)
data_anal <- data_anal[,(col2del) := NULL]



# class into character
data_anal$class <- as.character(data_anal$class)

# terrainType to character
data_anal$terrainType <- as.character(data_anal$terrainType)

# log(population_density)
data_anal$population_density <- log(data_anal$population_density + 1)



# # terrainType
# data_anal$terrainType <- ifelse(data_anal$terrainType == '0', 'water', ifelse(data_anal$terrainType %in% c('1','2','3','4','5'), 'forest', ifelse(data_anal$terrainType %in% c('6','7'), 'shrublands', ifelse(data_anal$terrainType %in% c('8','9'), 'savannas', ifelse(data_anal$terrainType == '10', 'grasslands', data_anal$terrainType)))))
# data_anal <- data_anal[terrainType %in% c('12','14'), terrainType := 'Cropland/Natural vegetation mosaic']
# data_anal <- data_anal[terrainType == '13', terrainType := 'urban and built-up']
# data_anal <- data_anal[terrainType %in% c('11'), terrainType := 'permanent wetlands']
# data_anal <- data_anal[terrainType %in% c('16'), terrainType := 'Barren or sparsely vegetated']
# 




# # continent
# data_anal <- data_anal[continent %in% c('America/Kentucky', 'America','America/Indiana'), continent := 'America']
# data_anal <- data_anal[continent %in% c('Africa','Atlantic','Indian'), continent := 'other']


# change pokestopDistanceKm and gymDistanceKm into numeric
data_anal$gymDistanceKm <- as.numeric(data_anal$gymDistanceKm)
data_anal$pokestopDistanceKm <- as.numeric(data_anal$pokestopDistanceKm)

# reorder columns
setcolorder(data_anal, c(185, 1:184, 186:188))

# change character as factor
data_anal <- as.data.frame(data_anal)
nm_char <- lapply(data_anal, class) == 'character'

data_anal[,nm_char] <- lapply(data_anal[,nm_char], as.factor)

save(data_anal, file = '707_Data_out/data_anal.RData')



