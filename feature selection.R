## the code is exploring different subset selection 
## and regularization techniques for predicting the "thalach" variable in the "heart.csv" dataset, 
## and the results are summarized and visualized for further analysis.

data <- read.csv("heart.csv")
df <- na.omit(data)
head(df, 10)

df$sex <- as.factor(df$sex)
df$cp <- as.factor(df$cp)
df$fbs <- as.factor(df$fbs)
df$restecg <- as.factor(df$restecg)
df$exang <- as.factor(df$exang)
df$slope <- as.factor(df$slope)
df$ca <- as.factor(df$ca)
df$thal <- as.factor(df$thal)
df$target <- as.factor(df$target)

# Subset selection

summary(df)
install.packages("leaps")
library(leaps)
sub.fit <- regsubsets(data = df, thalach ~.,nvmax = 23)

summary(sub.fit)

subreg.sum = summary(sub.fit)
plot(subreg.sum$bic, type = "b", ylab = "BIC", xlab = "Number of variables")
plot(sub.fit)


## Forward method
tc <- trainControl(method = "cv", number = 5)

h.fwd <- regsubsets(thalach ~ ., data=df, nvmax=19, method="forward")
summary(h.fwd)


## backward 

h.bwd <- regsubsets(thalach ~ ., data=df, nvmax=19, method="backward")
summary(h.bwd)


#lasso and ridge regression
library(elasticnet)

set.seed(195) #SEED
h.ridge <- train(thalach ~ ., data= df, method = "ridge", tuneLength=10, preProcess="scale",
                 trControl=ctrl)
h.ridge
plot(h.ridge)
getTrainPerf(h.ridge)

#lasso


set.seed(195) #SEED
h.lasso <- train(thalach ~ ., 
                 data= df, 
                 method = "lasso", tuneLength=100, trControl=ctrl, preProcess="scale")
h.lasso
plot(h.lasso)
getTrainPerf(h.lasso)

fraction.grid <- expand.grid(fraction=seq(.4,.5,by=.0001))
set.seed(195) #SEED
h.lasso.grid <- train(thalach ~ ., 
                      data= df, 
                      method = "lasso", tuneGrid=fraction.grid, 
                      trControl=ctrl)
h.lasso.grid
plot(h.lasso.grid)
getTrainPerf(h.lasso)
getTrainPerf(h.lasso.grid)


