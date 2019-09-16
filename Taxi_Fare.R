library(tidyverse)

taxi=read.csv('taxi.csv')
head(taxi)

colnames(taxi)[c(3,4)]=c('long','lat')
taxi=taxi[taxi$fare_amount>0,]
taxi=taxi[taxi$tip_amount>0,]
taxi$total=log(taxi$fare_amount+taxi$tip_amount)

taxi=taxi[between(taxi$lat,40.70,40.83) &
            between(taxi$long,-74.025,-73.93),]


install.packages('tree')
library(tree)

fitted_tree=tree(total~long+lat,taxi)
plot(fitted_tree)
text(fitted_tree)

library(lubridate)

taxi$hour=hour(taxi$pickup_datetime)
taxi$wday=wday(taxi$pickup_datetime,label=T)
taxi$month=month(taxi$pickup_datetime,label=T)

fitted_tree=tree(total~long+lat+hour+wday+month,taxi)
plot(fitted_tree)
text(fitted_tree)
summary(fitted_tree)

install.packages('randomForest')
library(randomForest)
fitted_tree=randomForest(total~long+lat+hour+wday+month,taxi,
                         ntree=80,sampsize=10000)
plot(fitted_tree)
summary(fitted_tree)

taxi$pred_total=fitted_tree$predicted
summary(taxi$pred_total)
