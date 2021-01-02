library(data.table)
library(dplyr)
library("randomForest"); library("caret");library(ggplot2);library(corrplot)
library(nortest)

df <- fread("C:/Users/User/Documents/bigdata/mid_data/new_tw2.csv")
video = na.omit(df)

video = subset(video,select = c(-video_id))
video = subset(video,select = c(-categoryId))
# Leave-one-out CV RMSE for lm()
LOOCV_lm_rmse = function(f, d){
  numOfRec = nrow(d);
  rmse_vec = rep(0,numOfRec); # n RMSEs
  reponse_var = all.vars(f)[1]; # Get the name of reponse variable
  
  for(i in 1:numOfRec){
    m = lm(formula=f, data=d[- i,],na.action = na.omit);
    rmse_vec[i] = (d[[reponse_var]][i]) - predict(m,newdata=d[i,]);
  }
  return( paste("LOOCV RMSE for lm(", format(f),") =", 
                round(sqrt(mean(rmse_vec ^ 2)),4)) );
}
#MAE function
Mae = function(x,y){
  mean(abs(x-y))
}

#切資料
ran = sample(1:nrow(video) , 0.7 * nrow(video)) #隨機抽70%
train = video[ran,] 
test = video[setdiff(1:nrow(video) , ran),]

#p-value 非常小，所以拒絕虛無假設，表示 income不符合常態分佈
qplot(x=income , data=video , geom="density" , xlab="income")
ad.test(video$income)

Mae(video$income , mean(video$income))#1085.922 模型都還沒建，用平均值

#income
#linear model

summary(lm_video)
lm_video1 = lm(income ~ ., data = train)
MAE(train$income,predict(lm_video1))#441
lm_video2 = lm(income ~ (.)^2, data = train)
MAE(train$income,predict(lm_video2))#253
summary(lm_video1)


MAE(test$income,predict(lm_video1,test))#458
MAE(test$income,predict(lm_video2,test))#417

#挑選變數
new_lm_video = lm(income ~ likes + dislikes + comment_count+desc_length , data = train)
MAE(train$income,predict(new_lm_video))#395.1171

new_lm_video_1 = lm(income ~ likes + dislikes +desc_length , data = train)
MAE(train$income,predict(new_lm_video_1))#395.2809

sort(abs(video$coefficients)[-1], decreasing = T)
cbind(caret::varImp(lm(income ~ . , data = video)), importance(randomForest(income ~ ., data = video)))

#對income取log之後，接近常態
qplot(x=log(income) , data=video , geom="density" , xlab="income")

#取log，效果沒有比較好
mae_log = function(model,dataset){
  y = log(dataset$income)
  y_pred = predict(model, dataset)
  
  y = exp(y) - 1
  y_pred = exp(y_pred)-1
  
  return(mean(abs(y-y_pred)))
}

lm_video_log = lm( log(income) ~ . , train)
mae_log(lm_video_log,train)#18116
mae_log(lm_video_log,test)#43445

lm_video_log2 = lm( log(income) ~ (.)^2 , train)
mae_log(lm_video_log2,train)#1089
mae_log(lm_video_log2,test)#1752
#效果不好，推測是資料集太少

#隨機森林
library(ranger)

rg_model = ranger(income ~ . , data = train , num.trees = 450 , mtry = 5)
Mae(train$income , rg_model$predictions)#317.9452


#predict
rg_pre = predict(rg_model , test)
Mae(test$income , rg_pre$predictions )#436.0136

rg_model_test = ranger(log(income) ~ . , data = train , num.trees = 450 , mtry = 5)

mae_rf = function(model,video){
  y = log(video$income) ; y_pred = model$predictions
  
  y = exp(y) - 1 ; y_pred = exp(y_pred)-1
  
  return(mean(abs(y-y_pred)))
}

mae_rf(rg_model_test , train) #563

#like ratio
#linear model
lm_video1 = lm(likes_ratio ~ ., data = train)
MAE(train$likes_ratio,predict(lm_video1))#40
lm_video2 = lm(likes_ratio ~ (.)^2, data = train)
MAE(train$likes_ratio,predict(lm_video2))#37

summary(lm_video1)

MAE(test$likes_ratio,predict(lm_video1,test))#39
MAE(test$likes_ratio,predict(lm_video2,test))#156



#挑選變數
new_lm_video_likes = lm(likes_ratio ~ likes + dislikes + tag_count, data = train)
MAE(train$likes_ratio,predict(new_lm_video_likes))#41


new_lm_video_likes2 = lm(likes_ratio ~ likes + dislikes , data = train)
MAE(train$income,predict(new_lm_video_likes2))#703

sort(abs(video$coefficients)[-1], decreasing = T)
cbind(caret::varImp(lm(likes_ratio ~ . , data = video)), importance(randomForest(likes_ratio ~ ., data = video)))

new_lm_video_likes3 = lm(likes_ratio ~ likes + dislikes +comment_count + income , data = train)
MAE(train$likes_ratio,predict(new_lm_video_likes3))#41.15589
new_lm_video_likes3 = lm(likes_ratio ~ likes + dislikes +comment_count  , data = train)
MAE(train$likes_ratio,predict(new_lm_video_likes3))#41.13441

#對income取log之後，接近常態
qplot(x=log(likes_ratio) , data=video , geom="density" , xlab="likes_ratio")

#取log，效果沒有比較好
mae_log = function(model,dataset){
  y = log(dataset$likes_ratio)
  y_pred = predict(model, dataset)
  
  y = exp(y) - 1
  y_pred = exp(y_pred)-1
  
  return(mean(abs(y-y_pred)))
}

lm_video_log3 = lm( log(likes_ratio) ~ . , train)
mae_log(lm_video_log3,train)#716.6723
mae_log(lm_video_log3,test)#43068.38

lm_video_log4 = lm( log(likes_ratio) ~ (.)^2 , train)
mae_log(lm_video_log4,train)#712
mae_log(lm_video_log4,test)#868.0577

#隨機森林


rg_model = ranger(likes_ratio ~ . , data = train , num.trees = 400 , mtry = 4)
Mae(train$likes_ratio , rg_model$predictions)#14.3208
#predict
rg_pre = predict(rg_model , test)
Mae(test$income , rg_pre$predictions )#868.0655

rg_model_test = ranger(log(likes_ratio) ~ . , data = train , num.trees = 400 , mtry = 4)

mae_rf = function(model,dataset){
  y = log(dataset$likes_ratio) ; y_pred = model$predictions
  
  y = exp(y) - 1 ; y_pred = exp(y_pred)-1
  
  return(mean(abs(y-y_pred)))
}

mae_rf(rg_model_test , train) #14.01734
#可以看到表現確實比likes_ratio未取log來的好











