###Setting working directory####

setwd("F:/IE Lectures/02 Spring 2019/02 Predictive Modeling/Midterm")


###Loading the data###

midterm<- read.csv("midterm.csv",stringsAsFactors = FALSE)

#######One hot encoding for month
midterm$month1 <- ifelse(midterm$month==1,1,0)
midterm$month2 <- ifelse(midterm$month==2,1,0)
midterm$month3 <- ifelse(midterm$month==3,1,0)
midterm$month4 <- ifelse(midterm$month==4,1,0)
midterm$month5 <- ifelse(midterm$month==5,1,0)
midterm$month6 <- ifelse(midterm$month==6,1,0)
midterm$month7 <- ifelse(midterm$month==7,1,0)
midterm$month8 <- ifelse(midterm$month==8,1,0)
midterm$month9 <- ifelse(midterm$month==9,1,0)
midterm$month10 <- ifelse(midterm$month==10,1,0)
midterm$month11 <- ifelse(midterm$month==11,1,0)


####CLEANING AND PREPROCESSING
str(midterm)  #46 vars
midterm$DT00<- as.numeric(midterm$DT00)
midterm$year <- as.factor(midterm$year)
midterm$month <- as.factor(midterm$month)



#All the variables are in proper formats

####Checking NA values
sum(complete.cases(midterm))  #296 out of 300 are complete , so 4 rows need to be removed
midterm <- midterm[complete.cases(midterm),]
nrow(midterm) # 296 rows



##Building linear model

model1<-lm(res.sales.adj ~ .-res.price-com.price-com.sales.adj-year-month,data = midterm)
summary(model1)
library(car)
residualPlots(model1)
#####The x variables should be used in their linear form according to the residual plots


#####################CROSS VALIDATION ON OLS#####################################
kfolds <-5

ols_prediction = data.frame()
test_copy_ols = data.frame()

# add a column sampled from 1 to 5 which will be our fold
midterm$kfolds <- sample(x=1:kfolds,size=nrow(midterm),replace=T) # assign folds
for (i in 1:kfolds){
  
  
  
  midterm.train <- midterm[-which(midterm$kfolds==i),]
  midterm.test  <- midterm[which(midterm$kfolds==i),]
  
  model_ols <- lm(res.sales.adj ~ .-res.price-com.price-com.sales.adj-year-kfolds-month,data = midterm.train)
  
  model_ols_pred <- as.data.frame(predict(model_ols,newdata=midterm.test))
  
  ols_prediction <- rbind(ols_prediction, model_ols_pred)
  
  test_copy_ols= rbind(test_copy_ols,as.data.frame(midterm.test$res.sales.adj))
  
  
}

result_ols <- cbind(ols_prediction, test_copy_ols[, 1])
names(result_ols) <- c("Predicted", "Actual")
mse_ols<- mean((result_ols$Actual - result_ols$Predicted)^2)
rmse_ols <- mse_ols^0.5
rmse_ols   #### 470
#############################################OLS ENDS HERE####################################################


################################RIDGE REGRESSION CV#######################################
kfolds <-5

ridge_prediction = data.frame()
test_copy_ridge = data.frame()

midterm$kfolds <- sample(x=1:kfolds,size=nrow(midterm),replace=T) # assign folds

library(glmnet)
shrinkage_ridge <- c(110,125,137,150,165)
for (i in 1:kfolds){
  
  
  
  midterm.train <- midterm[-which(midterm$kfolds==i),]
  midterm.test  <- midterm[which(midterm$kfolds==i),]
  
  midterm.train.x <- midterm.train[,-c(1,2,3,4,5,6,47)]
  midterm.train.y <- midterm.train[,4]
  midterm.train.x <- as.matrix(midterm.train.x)
  midterm.test.x <- midterm.test[,-c(1,2,3,4,5,6,47)]
  midterm.test.x <- as.matrix(midterm.test.x)
  
  model_ridge <- cv.glmnet(midterm.train.x,midterm.train.y, alpha = 0, standardize = T, type.measure = "mse")
  model_ridge_pred <- as.data.frame(predict(model_ridge, s = shrinkage_ridge[5], newx = midterm.test.x))
  
  ridge_prediction <- rbind(ridge_prediction, model_ridge_pred)
  
  test_copy_ridge= rbind(test_copy_ridge,as.data.frame(midterm.test$res.sales.adj))
  
  
}

result_ridge <- cbind(ridge_prediction, test_copy_ridge[, 1])
names(result_ridge) <- c("Predicted", "Actual")
mse_ridge<- mean((result_ridge$Actual - result_ridge$Predicted)^2)
rmse_ridge <- mse_ridge^0.5
rmse_ridge 
# lambda=110, rmse = 476.77
# lambda=125, rmse = 475.37
# lambda=137, rmse = 477.97
# lambda=150, rmse = 481.05
# lambda=165, rmse = 484.48

###############################RIDGE REGRESSION ENDS HERE#################################################


############################LASSO REGRESSION CROSS VALIDATION#######################################
kfolds <-5

lasso_prediction = data.frame()
test_copy_lasso = data.frame()

midterm$kfolds <- sample(x=1:kfolds,size=nrow(midterm),replace=T) # assign folds

library(glmnet)
shrinkage_lasso <- c(6, 8, 10, 15, 19, 20)
for (i in 1:kfolds){
  
  
  
  midterm.train <- midterm[-which(midterm$kfolds==i),]
  midterm.test  <- midterm[which(midterm$kfolds==i),]
  
  midterm.train.x <- midterm.train[,-c(1,2,3,4,5,6,47)]
  midterm.train.y <- midterm.train[,4]
  midterm.train.x <- as.matrix(midterm.train.x)
  midterm.test.x <- midterm.test[,-c(1,2,3,4,5,6,47)]
  midterm.test.x <- as.matrix(midterm.test.x)
  
  model_lasso <- cv.glmnet(midterm.train.x,midterm.train.y, alpha = 1, standardize = T, type.measure = "mse")
  model_lasso_pred <- as.data.frame(predict(model_lasso, s = shrinkage_lasso[6], newx = midterm.test.x))
  
  lasso_prediction <- rbind(lasso_prediction, model_lasso_pred)
  
  test_copy_lasso= rbind(test_copy_lasso,as.data.frame(midterm.test$res.sales.adj))
  
  
}

result_lasso <- cbind(lasso_prediction, test_copy_lasso[, 1])
names(result_lasso) <- c("Predicted", "Actual")
mse_lasso<- mean((result_lasso$Actual - result_lasso$Predicted)^2)
rmse_lasso <- mse_lasso^0.5
rmse_lasso #601.43
#lambda=6  , rmse= 427.02
#lambda= 8 , rmse= 423.83
#lambda=10  , rmse= 422.75
#lambda=  15, rmse= 421.09
#lambda=  19, rmse= 420.91
#lambda= 20  , rmse= 420.91

###############################LASSO REGRESSION ENDS HERE##########################################


##########################CROSS VALIDATION FOR TREES########################################

######################CART  TREES########################################

kfolds <-5

cart_prediction = data.frame()
test_copy_cart = data.frame()

midterm$kfolds <- sample(x=1:kfolds,size=nrow(midterm),replace=T) # assign folds

library("rpart")

for (i in 1:kfolds){
  
  
  
  midterm.train <- midterm[-which(midterm$kfolds==i),]
  midterm.test  <- midterm[which(midterm$kfolds==i),]
  
  model_cart <- rpart(res.sales.adj ~ .-res.price-com.price-com.sales.adj-kfolds-year,data = midterm.train, method = "anova")
  
  model_cart_pred <- as.data.frame(predict(model_cart,newdata=midterm.test))
  
  cart_prediction <- rbind(cart_prediction, model_cart_pred)
  
  test_copy_cart= rbind(test_copy_cart,as.data.frame(midterm.test$res.sales.adj))
  
  
}

result_cart <- cbind(cart_prediction, test_copy_cart[, 1])
names(result_cart) <- c("Predicted", "Actual")
mse_cart<- mean((result_cart$Actual - result_cart$Predicted)^2)
rmse_cart <- mse_cart^0.5
rmse_cart  #####550.11

#############################TREES ENDS HERE####################################################


##################################RANDOM FOREST CROSS VALIDATION##############################

##################CV for random forest############
kfolds <-5
rmse_vec_rf <- c()
num_tree = c(20, 50, 100, 200, 300, 500, 1000, 2000, 4000, 5000)

midterm$kfolds <- sample(x=1:kfolds,size=nrow(midterm),replace=T) # assign folds

library("randomForest")
#num_tree = c(20, 50, 100, 200, 300, 500, 1000, 2000, 4000, 5000)
for (j in 1:length(num_tree)){
  rf_prediction = data.frame()
  test_copy_rf = data.frame()
  

for (i in 1:kfolds){
  
  
  
  midterm.train <- midterm[-which(midterm$kfolds==i),-c(36:46)]
  midterm.test  <- midterm[which(midterm$kfolds==i),-c(36:46)]
  
  model_rf <- randomForest(res.sales.adj ~ .-res.price-com.price-com.sales.adj-kfolds-year,data = midterm.train,ntree=num_tree[j])
  
  model_rf_pred <- as.data.frame(predict(model_rf,newdata=midterm.test))
  
  rf_prediction <- rbind(rf_prediction, model_rf_pred)
  
  test_copy_rf= rbind(test_copy_rf,as.data.frame(midterm.test$res.sales.adj))
  
  
}

result_rf <- cbind(rf_prediction, test_copy_rf[, 1])
names(result_rf) <- c("Predicted", "Actual")
mse_rf<- mean((result_rf$Actual - result_rf$Predicted)^2)
rmse_rf <- mse_rf^0.5
rmse_rf
rmse_vec_rf[j] <- rmse_rf
}
#tree = 20, 491.84
#tree = 50 , 487.39
#tree = 100, 476.31
#tree = 200 , 480.30
#tree= 300, 478.85
#tree = 500, 483.92
#tree =1000, 477.22
#tree = 2000, 479.02
#tree=4000, 477.56
#tree = 5000, 477.60
rmse_vec_rf <- cbind(as.data.frame(rmse_vec_rf),as.data.frame(num_tree))
rmse_vec_rf
# 1     465.1815       20
# 2     458.3646       50
# 3     452.6096      100
# 4     448.7968      200
# 5     446.3091      300
# 6     450.0767      500
# 7     447.9296     1000
# 8     445.4180     2000
# 9     447.0177     4000
# 10    447.9671     5000
# Ntree = 300, rmse = 446
########################RANDOM FOREST ENDS HERE######################################################

################################GRADIENT BOOSTING CROSS VALIDATION####################################
kfolds <-5
n_trees <- c(20, 50, 100, 200, 300, 500, 1000, 2000, 4000, 5000)
depth <- c(2,4,6,8,10)
gbm_prediction = data.frame()
test_copy = data.frame()
rmse_vec_gbm =c()
cnt=1
# add a column sampled from 1 to 5 which will be our fold
midterm$kfolds <- sample(x=1:kfolds,size=nrow(midterm),replace=T) # assign folds
library(gbm)
for (j in 1:length(n_trees)){
  for (k in 1:length(depth)){
    
    rf_prediction = data.frame()
    test_copy_rf = data.frame()
    
    for (i in 1:kfolds){
      
      
      
      midterm.train <- midterm[-which(midterm$kfolds==i),-c(36:46)]
      midterm.test  <- midterm[which(midterm$kfolds==i),-c(36:46)]
      
      model_gbm <- gbm (res.sales.adj ~ .-res.price-com.price-com.sales.adj-year-kfolds,data = midterm.train, distribution=
                          "gaussian",n.trees =n_trees[j] , interaction.depth =depth[k])
      
      model_gbm_pred <- as.data.frame(predict(model_gbm,newdata=midterm.test,n.trees=n_trees[j]))
      
      gbm_prediction <- rbind(gbm_prediction, model_gbm_pred)
      
      test_copy= rbind(test_copy,as.data.frame(midterm.test$res.sales.adj))
      
      
    }
    
    result_gbm <- cbind(gbm_prediction, test_copy[, 1])
    names(result_gbm) <- c("Predicted", "Actual")
    mse_gbm<- mean((result_gbm$Actual - result_gbm$Predicted)^2)
    rmse_gbm <- mse_gbm^0.5
    rmse_gbm
    
    rmse_vec_gbm[cnt] <- rmse_gbm
    
    cnt =cnt +1
  }
}
# 
# [1] 508.5709 496.2565 491.6690 490.2943 489.1568 481.9451 477.1917 474.4117 472.2884 470.4492 468.3055 467.3838 467.2031
# [14] 466.9318 466.0266 465.6714 465.8117 465.5755 465.1689 464.7049 464.8991 465.1346 465.4200 465.7344 465.8314 466.0824
# [27] 466.2947 466.7233 466.7233 466.9434 467.3820 467.3061 466.9275 467.3351 467.4917 467.6938 467.7428 467.9588 468.1870
# [40] 468.2485 468.7243 468.8739 468.9709 469.1081 469.1443 469.4459 469.4425 469.5875 469.5225 469.4490

# Ntree = 200, Depth = 10 , rmse = 464.70

###################################GRADIENT BOOSTING MACHINE ENDS HERE###############################################




#########################################GAMS CROSS VALIDATION######################################################
train_index <- sample(nrow(midterm), floor(.85 * nrow(midterm)))
midterm_train <- midterm[train_index,]
midterm_test <- midterm[-train_index,]
library(gam)
# gam_model <- gam(res.sales.adj ~ month + s(EMXP, df = 4) + s(MXSD, df = 4)
#                  + TPCP
#                  + TSNW+ s(Personal, df = 4) + s(PhD, df = 4)
#                  + s(S.F.Ratio, df = 4) 
#                  + s(Expend, df = 4) + Grad.Rate , data = college_train)


gam_midterm <- gam(res.sales.adj ~ s(EMXP, df=4)+
                 #s(MXSD,df=4)+
                 s(TPCP,df=4)+
                 #s(TSNW,df=4)+
                 s(EMXT,df=4)+
                 s(EMNT,df=4)+
                 s(MMXT,df=4)+
                 s(MMNT,df=4)+
                 s(MNTM,df=4)+
                 s(DT90,df=4)+
                 #s(DX32,df=4)+
                 #s(DT00,df=4)+
                 s(DT32,df=4)+
                 s(DP01,df=4)+
                 s(DP05,df=4)+
                 s(DP10,df=4)+
                 s(MDPT,df=4)+
                 s(VISIB, df=4)+
                 s(WDSP, df=4)+
                 s(MWSPD, df=4)+
                 s(GUST, df=4)+
                 s(HTDD,df=4)+
                 s(CLDD,df=4)+
                 s(LABOR,df=4)+
                 s(EMP,df=4)+
                 s(UNEMP,df=4)+
                 s(UNEMPRATE,df=4)+
                 s(PCINCOME,df=4)+
                 s(GSP,df=4)
                #s(month1, df=4)+s(month2, df=4)+s(month3, df=4)+s(month4, df=4)+s(month5, df=4)+s(month6, df=4)+
                  #s(month7, df=4)+s(month8, df=4)+s(month9, df=4)+s(month10, df=4)+s(month11, df=4)
                  , data = midterm_train)
                  
summary(gam_midterm)                  
plot(gam_midterm)
"Apps" =~ 1+ Apps + s(Apps, df = 2)+ s(Apps, df = 3)+ s(Apps, df = 4)

gam.step.midterm<- step.Gam(gam_midterm,scope = list("EMXP"=~ 1+EMXP,
                                               #s(MXSD,df=4)+
                                              "TPCP"=~1+TPCP ,
                                               #s(TSNW,df=4)
                                               "EMXT"=~1+EMXT+s(EMXT,df=2)+s(EMXT,df=3)+s(EMXT,df=4),
                                               "EMNT"=~1+EMNT+s(EMNT,df=2)+s(EMNT,df=3)+s(EMNT,df=4),
                                               "MMXT"=~1+MMXT,
                                               "MMNT"=~1+MMNT,
                                               "MNTM"=~1+MNTM,
                                               "DT90"=~1+DT90+s(DT90,df=2)+s(DT90,df=3)+s(DT90,df=4),
                                               #s(DX32,df=4)+
                                               #s(DT00,df=4)+
                                               "DT32"=~1+DT32,
                                               "DP01"=~1+DP01+s(DP01,df=2)+s(DP01,df=3)+s(DP01,df=4),
                                               "DP05"=~1+DP05,
                                               "DP10"=~1+DP10,
                                               "MDPT"=~1+MDPT,
                                             "VISIB"=~1+VISIB+s(VISIB,df=2)+s(VISIB,df=3)+s(VISIB,df=4),
                                             "WDSP"=~1+WDSP,
                                             "MWSPD"=~1+MWSPD,
                                             "GUST"=~1+GUST,
                                             "HTDD"=~1+HTDD,
                                             "CLDD"=~1+CLDD,
                                             "EMP"=~1+EMP,
                                             "PCINCOME"=~1+PCINCOME),
                                               
                    direction = "both",
                    trace=2)


gam.step<- step.Gam(gam_model4,scope = list("Apps" =~ 1+ Apps + s(Apps, df = 2)+ s(Apps, df = 3)+ s(Apps, df = 4),
                                            "F.Undergrad" =~ 1+ F.Undergrad + s(F.Undergrad, df = 2)+ s(F.Undergrad,                                              df = 3)+ s(F.Undergrad, df = 4),
                                            "Books" =~ 1+ Books + s(Books, df = 2)+ s(Books,                                                                      df = 3)+ s(Books, df = 4),
                                            "Personal" =~ 1+ Personal + s(Personal, df = 2)+ s(Personal,                                                                      df = 3)+ s(Personal, df = 4),
                                            "PhD" =~ 1+ PhD + s(PhD, df = 2)+ s(PhD,                                                                              df = 3)+ s(PhD, df = 4),
                                            "S.F.Ratio" =~ 1+ S.F.Ratio + s(S.F.Ratio, df = 2)+ s(S.F.Ratio,                                                                              df = 3)+ s(S.F.Ratio, df = 4),
                                            "Expend" =~ 1+ Expend + s(Expend, df = 2)+ s(Expend,                                                                              df = 3)+ s(Expend, df = 4),
                                            "Grad.Rate" =~ 1+ Grad.Rate + s(Grad.Rate, df = 2)+ s(Grad.Rate,                                                                              df = 3)+ s(Grad.Rate, df = 4)),
                    direction = "both",
                    trace=2)


gam_midterm_new <- gam(res.sales.adj ~ s(EMXP, df=2)+
                     #s(MXSD,df=4)+
                     s(TPCP,df=2)+
                     #s(TSNW,df=4)+
                     s(EMXT,df=3)+
                     s(EMNT,df=3)+
                     s(MMXT,df=2)+
                     s(MMNT,df=2)+
                     s(MNTM,df=2)+
                     s(DT90,df=3)+
                     #s(DX32,df=4)+
                     #s(DT00,df=4)+
                     s(DT32,df=2)+
                     s(DP01,df=2)+
                     s(DP05,df=2)+
                     s(DP10,df=2)+
                     s(MDPT,df=2)+
                     s(VISIB, df=3)+
                     s(WDSP, df=2)+
                     s(MWSPD, df=2)+
                     s(GUST, df=2)+
                     s(HTDD,df=2)+
                     s(CLDD,df=2)+
                     #s(LABOR,df=4)+
                     s(EMP,df=2)+
                     #s(UNEMP,df=4)+
                     #s(UNEMPRATE,df=4)+
                     s(PCINCOME,df=2)
                     #s(GSP,df=4)
                   #s(month1, df=4)+s(month2, df=4)+s(month3, df=4)+s(month4, df=4)+s(month5, df=4)+s(month6, df=4)+
                   #s(month7, df=4)+s(month8, df=4)+s(month9, df=4)+s(month10, df=4)+s(month11, df=4)
                   , data = midterm_train)

kfolds <-5

gam_prediction = data.frame()
test_copy_gam = data.frame()

# add a column sampled from 1 to 5 which will be our fold
#midterm$kfolds <- sample(x=1:kfolds,size=nrow(midterm),replace=T) # assign folds
for (i in 1:kfolds){
  
  
  
  midterm.train <- midterm[-which(midterm$kfolds==i),]
  midterm.test  <- midterm[which(midterm$kfolds==i),]
  
  model_gam <- gam(res.sales.adj ~ s(EMXP, df=2)+
                           #s(MXSD,df=4)+
                           s(TPCP,df=2)+
                           #s(TSNW,df=4)+
                           s(EMXT,df=3)+
                           s(EMNT,df=3)+
                           s(MMXT,df=2)+
                           s(MMNT,df=2)+
                           s(MNTM,df=2)+
                           s(DT90,df=3)+
                           #s(DX32,df=4)+
                           #s(DT00,df=4)+
                           s(DT32,df=2)+
                           s(DP01,df=2)+
                           s(DP05,df=2)+
                           s(DP10,df=2)+
                           s(MDPT,df=2)+
                           s(VISIB, df=3)+
                           s(WDSP, df=2)+
                           s(MWSPD, df=2)+
                           s(GUST, df=2)+
                           s(HTDD,df=2)+
                           s(CLDD,df=2)+
                           #s(LABOR,df=4)+
                           s(EMP,df=2)+
                           #s(UNEMP,df=4)+
                           #s(UNEMPRATE,df=4)+
                           s(PCINCOME,df=2)
                         #s(GSP,df=4)
                         #s(month1, df=4)+s(month2, df=4)+s(month3, df=4)+s(month4, df=4)+s(month5, df=4)+s(month6, df=4)+
                         #s(month7, df=4)+s(month8, df=4)+s(month9, df=4)+s(month10, df=4)+s(month11, df=4)
                         , data = midterm.train)
  
  
  model_gam_pred <- as.data.frame(predict(model_gam,newdata=midterm.test))
  
  gam_prediction <- rbind(gam_prediction, model_gam_pred)
  
  test_copy_gam= rbind(test_copy_gam,as.data.frame(midterm.test$res.sales.adj))
  
  
}

result_gam <- cbind(gam_prediction, test_copy_gam[, 1])
names(result_gam) <- c("Predicted", "Actual")
mse_gam<- mean((result_gam$Actual - result_gam$Predicted)^2)
rmse_gam <- mse_gam^0.5
rmse_gam   #### 486.92

##############################################GAMS END HERE######################################


##################FINAL MODEL############################
#############Lasso Model is selected

midterm_train <- midterm[-which(midterm$kfolds==5),]
midterm_test  <- midterm[which(midterm$kfolds==5),]

midterm_train_x <- midterm_train[,-c(1,2,3,4,5,6,47)]
midterm_train_y <- midterm_train[,4]
midterm_train_x <- as.matrix(midterm_train_x)
midterm_test_x <- midterm_test[,-c(1,2,3,4,5,6,47)]
midterm_test_x <- as.matrix(midterm_test_x)

final_model <- cv.glmnet(midterm_train_x,midterm_train_y, alpha = 1, standardize = T, type.measure = "mse")
finalmodel <- predict(final_model, s = 19, type = "coefficients")
