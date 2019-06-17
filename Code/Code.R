if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, ggplot2,corrplot,plotly,ggfortify, textclean,
               GGally, readr,caret,tidyr,reshape,rstudioapi,h2o,randomForest,DescTools)
h2o.init(nthreads = -1, max_mem_size = "14G")
zerovar <- function(x){
  removenames <- nearZeroVar(x,saveMetrics = FALSE,uniqueCut = 0.02,names = TRUE)
  waps <- colnames(x)[grep("WAP", colnames(x))]
  importants <- colnames(x)[which(!names(x) %in% waps)]
  removenames <- removenames[which(!removenames %in% importants)]
  x <- x[-which(colnames(x) %in% removenames)]
}
filtergroup <- function(x,varcolumn){
  list <- list()
  categories <- unique(x[varcolumn])
  for (i in categories[[1]]) {
    list[[i]] <- x %>%  filter(x[varcolumn] == i)
  } 
  return(list)
}
filtergrouplist <- function(x,varcolumn,listelement){
  list <- list()
  categories <- unique(x[[listelement]][[varcolumn]])
  for (i in categories) {
    list[[i]] <- x[[listelement]] %>%  filter(x[[listelement]][[varcolumn]] == i)
  } 
  return(list)
}
#GITHUB SETUP####
current_path <- getActiveDocumentContext()
setwd(dirname(dirname(current_path$path)))
rm(current_path)
#IMPORT DATASETS####
training <- read.csv("Datasets/trainingData.csv",colClasses = c(SPACEID = "numeric"))
validation <- read.csv("Datasets/validationData.csv",colClasses = c(SPACEID = "numeric"))

training$position <- as.factor(paste(training$BUILDINGID,"floor",
                                     training$FLOOR,"room",training$SPACEID))
validation$position <- as.factor(paste(validation$BUILDINGID,"floor",
                                       validation$FLOOR))

positiontrain <- split(training,training$position)
positionvalidation <- split(validation,validation$position)

combinedtrain <- lapply(positiontrain,function(x)sample_frac(tbl = x,size = 7/10,replace =  TRUE))
combinedtest <- lapply(positionvalidation,function(x)sample_frac(tbl = x,size = 10/10,replace =  TRUE))
combinedtrain <- bind_rows(combinedtrain)
combinedtest <- bind_rows(combinedtest)
combined <- rbind(combinedtrain,combinedtest)

combined[grep("WAP", colnames(combined))] <- 
  apply(combined[grep("WAP", colnames(combined))],2,
        function(x) ifelse(x == 100,yes =  0,no = 
                             ifelse(test = x < -92,yes =  1,no = 
                                      ifelse(test = x > -21,yes =  100,no = 
                                               (-0.0154*x*x)-(0.3794*x)+98.182))))
training[grep("WAP", colnames(training))] <- 
  apply(training[grep("WAP", colnames(training))],2,
        function(x) ifelse(x == 100,yes =  0,no = 
                             ifelse(test = x < -92,yes =  1,no = 
                                      ifelse(test = x > -21,yes =  100,no = 
                                               (-0.0154*x*x)-(0.3794*x)+98.182))))
validation[grep("WAP", colnames(validation))] <- 
  apply(validation[grep("WAP", colnames(validation))],2,
        function(x) ifelse(x == 100,yes =  0,no = 
                             ifelse(test = x < -92,yes =  1,no = 
                                      ifelse(test = x > -21,yes =  100,no = 
                                               (-0.0154*x*x)-(0.3794*x)+98.182))))
checking <- c()
for (i in grep("WAP", colnames(training))) {
  checking <- c(checking,training$BUILDINGID[Mode(which(training[i] == max(training[i])))[1]] - 
                  validation$BUILDINGID[Mode(which(validation[i] == max(validation[i])))[1]]) 
}

training <- training[-which(abs(checking) >= 2)]
validation <- validation[-which(abs(checking) >= 2)]
combined <- combined[-which(abs(checking) >= 2)]

training <- training[,c(520:529,1:519)]
validation <- validation[,c(520:529,1:519)]
combined <- combined[,c(520:529,1:519)]

#TRAINING
training <- training[-which(duplicated(training)),]
training$BUILDINGID <- mgsub(training$BUILDINGID,
                             pattern = c(0,1,2),
                             replacement = c("TI","TD","TC"))
training$BUILDINGID <- as.factor(training$BUILDINGID)
training$ID <- as.factor(paste(training$BUILDINGID,"floor",training$FLOOR))
training$FLOOR <- as.factor(training$FLOOR)
training <- training[,c(530,1:529)]
training <- zerovar(training)

#COMBINED
combined <- combined[-which(duplicated(combined)),]
combined$BUILDINGID <- mgsub(combined$BUILDINGID,
                             pattern = c(0,1,2),
                             replacement = c("TI","TD","TC"))
combined$BUILDINGID <- as.factor(combined$BUILDINGID)
combined$ID <- as.factor(paste(combined$BUILDINGID,"floor",combined$FLOOR))
combined$FLOOR <- as.factor(combined$FLOOR)
combined <- combined[,c(530,1:529)]
combined <- zerovar(combined)

#VALIDATION
validation$BUILDINGID <- mgsub(validation$BUILDINGID,
                               pattern = c(0,1,2),
                               replacement = c("TI","TD","TC"))
validation$BUILDINGIDreal <- as.factor(validation$BUILDINGID)
validation$ID <- as.factor(paste(validation$BUILDINGID,"floor",validation$FLOOR))
validation$FLOORreal <- as.factor(validation$FLOOR)
validation$FLOOR <- NULL
validation$BUILDINGID <- NULL
validation <- validation[,c(530,1:529)]
validation$LONGITUDEreal <- validation$LONGITUDE 
validation$LATITUDEreal <- validation$LATITUDE 
# validation$LATITUDE <- NULL
# validation$LONGITUDE <- NULL

#####Model Building#####

trainingh2o <- as.h2o(training)
validationh2o <- as.h2o(validation)
combinedh2o <- as.h2o(combined)

buildingsmodel <-
      h2o.randomForest(
            x = which(grepl("WAP", x = colnames(trainingh2o))),
            y = which(colnames(trainingh2o) == "BUILDINGID"),
            training_frame = trainingh2o,
            seed = 1,
            ntrees = 100,nfolds = 5)

# buildingsmodel <- readRDS("Models/buildingsmodel.rds")

#####Model Floor#####
trainingbuilding <- lapply(split(training,training$BUILDINGID),zerovar)
combinedbuilding <- lapply(split(combined,combined$BUILDINGID),zerovar)

testing <- validation
testingbuilding <- split(testing,testing$BUILDINGID)

trainingbuildingh2o <- lapply(trainingbuilding,as.h2o)
testingbuildingh2o <- lapply(testingbuilding,as.h2o)
combinedbuildingh2o <- lapply(combinedbuilding,as.h2o)

floorsmodel <-
    lapply(trainingbuildingh2o,function(x)
        h2o.randomForest(
            x = which(grepl("WAP", x = colnames(x))),
            y = which(colnames(x) == "FLOOR"),
            training_frame = x,
            seed = 1,
            ntrees = 100,nfolds = 5))
floorsmodel[[2]] <- 
        h2o.randomForest(
          x = which(grepl("WAP", x = colnames(combinedbuildingh2o[[2]]))),
          y = which(colnames(combinedbuildingh2o[[2]]) == "FLOOR"),
          training_frame = combinedbuildingh2o[[2]],
          seed = 1,
          ntrees = 100,nfolds = 5)
# floorsmodel <- readRDS("Models/floorsmodel.rds")

#####Model Latitude#####
trainingfloor <- lapply(trainingbuilding,function(x)
  split(x,x$FLOOR))

trainingfloor$TD$`4` <- NULL
trainingfloor$TI$`4` <- NULL

trainingfloor <- lapply(trainingfloor,function(x)
  lapply(x,zerovar))

trainingfloorh2o <- lapply(trainingfloor,function(x)
  lapply(x,as.h2o))

latitudemodel <-
  lapply(trainingfloorh2o,function(x)
    lapply(x,function(x)
      h2o.randomForest(
        x = which(grepl("WAP", x = colnames(x))),
        y = which(colnames(x) == "LATITUDE"),
        training_frame = x,
        seed = 1,
        ntrees = 100,nfolds = 5)))

# latitudemodel <- readRDS("Models/latitudemodel.rds")

#####Model Longitude####
longitudemodel <-
    lapply(trainingfloorh2o,function(x)
          lapply(x,function(x)
               h2o.randomForest(
                        x = which(grepl("WAP|LATITUDE", x = colnames(x))),
                        y = which(colnames(x) == "LONGITUDE"),
                        training_frame = x,
                        seed = 1,
                        ntrees = 100,nfolds = 5)))

# longitudemodel <- readRDS("Models/longitudemodelmarshmello.rds")

#####Predict Building####

buildingspredict <- as.data.frame(h2o.predict(object = buildingsmodel,as.h2o(testing)))
testing$BUILDINGID <- buildingspredict$predict
perfbuilding <- postResample(testing$BUILDINGID,testing$BUILDINGIDreal)

#####Predicting Floor#####

performancefloor <- c()

for (i in 1:length(testingbuildingh2o)) {
  floor <- as.data.frame(h2o.predict(object = floorsmodel[[i]],newdata = testingbuildingh2o[[i]]))
  testingbuilding[[i]]$FLOOR <- floor$predict
  performancefloor <- rbind(as.data.frame(performancefloor),postResample(testingbuilding[[i]]$FLOOR,testingbuilding[[i]]$FLOORreal))
}
colnames(performancefloor) <- c("Accuracy","Kappa")
rownames(performancefloor) <- names(testingbuilding)

#####Predicting Latitude#####

testingfloor <- lapply(testingbuilding,function(x) split(x,x$FLOOR))
testingfloor <- lapply(testingfloor,function(x)lapply(x,zerovar))
testingfloorh2o <- lapply(testingfloor,function(x)lapply(x,as.h2o))
testingfloor$TD$`4` <- NULL
testingfloor$TI$`4` <- NULL

testingfloorh2o <- lapply(testingfloor,function(x)lapply(x,as.h2o))


latitudepebuildingsmodel <- c()
for (j in 1:length(testingfloorh2o)) {
  for (i in 1:length(testingfloorh2o[[j]])) {
    latitudepred <- as.data.frame(h2o.predict(object = latitudemodel[[j]][[i]],
                                          newdata = testingfloorh2o[[j]][[i]]))
    
    testingfloor[[j]][[i]]$LATITUDE <- latitudepred$predict
    
    latitudepebuildingsmodel <- rbind(as.data.frame(latitudepebuildingsmodel),
                                   postResample(testingfloor[[j]][[i]]$LATITUDE,
                                                testingfloor[[j]][[i]]$LATITUDEreal))
  }
}

naming <- c()
for(i in 1:length(testingfloor)){
naming <- c(naming,paste(rep(names(testingfloor[i]),
                             times = length(names(testingfloor[[i]]))),names(testingfloor[[i]])))
}
colnames(latitudepebuildingsmodel) <- c("RMSE","Rsquared","MAE")
rownames(latitudepebuildingsmodel) <- naming

#####Predicting Longitude#####
longitudepebuildingsmodel <- c()
testinglatitude <- lapply(testingfloor,function(x)lapply(x,function(x)as.h2o(x)))
for (j in 1:length(testinglatitude)) {
  
  for (i in 1:length(testinglatitude[[j]])) {
    longitudepred <- as.data.frame(h2o.predict(object = longitudemodel[[j]][[i]],
                                              newdata = testinglatitude[[j]][[i]]))
    
    testingfloor[[j]][[i]]$LONGITUDE <- longitudepred$predict
    
    longitudepebuildingsmodel <- rbind(as.data.frame(longitudepebuildingsmodel),
                                   postResample(testingfloor[[j]][[i]]$LONGITUDE,
                                                testingfloor[[j]][[i]]$LONGITUDEreal))
  }
}

naming <- c()
for(i in 1:length(testingfloor)){
  naming <- c(naming,paste(rep(names(testingfloor[i]),times = length(names(testingfloor[[i]]))),names(testingfloor[[i]])))
}
colnames(longitudepebuildingsmodel) <- c("RMSE","Rsquared","MAE")
rownames(longitudepebuildingsmodel) <- naming

#####RESULTS#####
finaltrial <- lapply(testingfloor,bind_rows)
finaltrial <- bind_rows(finaltrial)
finaltrial <- as.data.frame(finaltrial[-which(grepl("WAP", x = colnames(finaltrial)))])
finaltrial$buildingerror <- finaltrial$BUILDINGID != finaltrial$BUILDINGIDreal
finaltrial$floorerror <- abs(abs(as.numeric(finaltrial$FLOOR)) - abs(as.numeric(finaltrial$FLOORreal)))
finaltrial$longitudeerror <- finaltrial$LONGITUDE - finaltrial$LONGITUDEreal
finaltrial$latitudeerror <- finaltrial$LATITUDE - finaltrial$LATITUDEreal
errors <- finaltrial[which(grepl("error", x = colnames(finaltrial)))]
errors$totalerror <- abs(errors$latitudeerror) + abs(errors$longitudeerror) + 4*abs(as.numeric(errors$floorerror)) + 50*finaltrial$buildingerror
bigerrors <- finaltrial[which(errors$totalerror>20),]
summary(errors)
perfbuilding
performancefloor
latitudepebuildingsmodel
longitudepebuildingsmodel
apply(latitudepebuildingsmodel,2,mean)
apply(longitudepebuildingsmodel,2,mean)
histogram(as.numeric(bigerrors$longitudeerror),breaks = 50)
histogram(as.numeric(bigerrors$latitudeerror),breaks = 50)
histogram(as.numeric(bigerrors$FLOOR))
histogram(as.numeric(bigerrors$BUILDINGID))

#####Models####
saveRDS(buildingsmodel,file = "Models/buildingsmodelsomecombined.rds")
saveRDS(longitudemodel,file = "Models/longitudemodelsomecombined.rds")
saveRDS(latitudemodel,file = "Models/latitudemodelsomecombined.rds")
saveRDS(floorsmodel,file = "Models/floorsmodelsomecombined.rds")