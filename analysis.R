rm(list = ls())

library(car)
library(caret)
# setwd("~/Documents/6300individual/")
# load data
enbdata <- read.table("ENBdata.txt", header = TRUE,sep = "\t",encoding = "utf-8")
enbnames <- c('RelativeCompactness','SurfaceArea','WallArea',
              'RoofArea','OverallHeight','Orientation','GlazingArea',
              'GlazingAreaDistribution','HeatingLoad','CoolingLoad')
# colnames(enbdata) <- enbnames
# Specifically: 
# X1	Relative Compactness 
# X2	Surface Area 
# X3	Wall Area 
# X4	Roof Area 
# X5	Overall Height 
# X6	Orientation 
# X7	Glazing Area 
# X8	Glazing Area Distribution 
# y1	Heating Load 
# y2	Cooling Load
summary(enbdata)
enbdata$X6 <- factor(enbdata$X6)
enbdata$X7 <- factor(enbdata$X7)
enbdata$X8 <- factor(enbdata$X8)

# data partition
intrain <- createDataPartition(y = enbdata$Y1, p = 0.7, list = F)

train.data <- enbdata[intrain, 1:8]
train.label <- enbdata[intrain, 'Y1']
train.total <- enbdata[intrain, 1:9]

test.data <- enbdata[-intrain, 1:8]
test.label <- enbdata[-intrain, 'Y1']

# EDA
pairs(enbdata)
cor(cbind(enbdata[1:5],enbdata[9:10]))
scatterplotMatrix(enbdata,spread=FALSE,lty.smooth=2,main="Scatter Plot Matrix")

# train and mse mae
model.fit <- function(train.total, test.data, test.label, method.fit){
  fit <- train(
    Y1 ~ ., train.total,
    method = method.fit,
    trControl = trainControl(
      method = "cv", number = 10,
      verboseIter = TRUE
    )
  )
  cat(str(fit$results))
  predictY <- predict(fit, test.data)
  mse <- sum((test.label-predictY)^2)/length(test.label)
  mae <- sum(abs(test.label-predictY))/length(test.label)
  cat(method.fit,".mse=",mse,"\n")
  cat(method.fit,".mae=",mae,"\n")
}


# simple linear regression
lm.slr <- lm(Y1~., data = train.total)
summary(lm.slr)
pre.slr <- predict(lm.slr, test.data)
slr.mse <- sum((test.label-pre.slr)^2)/length(test.label)
slr.mae <- sum(abs(test.label-pre.slr))/length(test.label)
cat("slr.mse=",slr.mse,"\n")
cat("slr.mae=",slr.mae,"\n")

# simple linear regression with 10-fold cv
model.fit(train.total, test.data, test.label, 'lm')

# min-max normalization
data.new <- enbdata
data.new$X1 <- (data.new$X1-min(data.new$X1))/(max(data.new$X1)-min(data.new$X1))
data.new$X2 <- (data.new$X2-min(data.new$X2))/(max(data.new$X2)-min(data.new$X2))
data.new$X3 <- (data.new$X3-min(data.new$X3))/(max(data.new$X3)-min(data.new$X3))
data.new$X4 <- (data.new$X4-min(data.new$X4))/(max(data.new$X4)-min(data.new$X4))
data.new$X5 <- (data.new$X5-min(data.new$X5))/(max(data.new$X5)-min(data.new$X5))

intrains <- createDataPartition(y = data.new$Y1, p = 0.7, list = F)

train.datas <- data.new[intrain, 1:8]
train.labels <- data.new[intrain, 'Y1']
train.totals <- data.new[intrain, 1:9]

test.datas <- data.new[-intrain, 1:8]
test.labels <- data.new[-intrain, 'Y1']

# after normalization
# simple linear regression
lm.slr <- lm(Y1~., data = train.totals)
summary(lm.slr)

pre.slr <- predict(lm.slr, test.datas)
slr.mse <- sum((test.labels-pre.slr)^2)/length(test.labels)
slr.mae <- sum(abs(test.labels-pre.slr))/length(test.labels)
cat("slr.mse=",slr.mse,"\n")
cat("slr.mae=",slr.mae,"\n")

# simple linear regression with 10-fold cv
model.fit(train.totals, test.datas, test.labels, 'lm')

# predictors selection
library(MASS)
temp <- stepAIC(lm.slr, direction = "backward")

# simple linear regression
lm.slr <- lm(Y1~X1+X2+X3+X5+X7, data = train.total)
summary(lm.slr)
pre.slr <- predict(lm.slr, test.data)
slr.mse <- sum((test.label-pre.slr)^2)/length(test.label)
slr.mae <- sum(abs(test.label-pre.slr))/length(test.label)
cat("slr.mse=",slr.mse,"\n")
cat("slr.mae=",slr.mae,"\n")

model.fit(train.total, test.data, test.label, 'lm')
model.fit(train.total, test.data, test.label, 'lasso')
model.fit(train.total, test.data, test.label, 'rf')
model.fit(train.total, test.data, test.label, 'svmLinear3')
