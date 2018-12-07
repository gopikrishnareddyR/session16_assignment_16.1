#session16_assisgnment_16.1

#1. Use the below given data set
#Data Set
library(readr)
library(data.table)

getwd()
data1<-fread("C:/Users/DELL/Documents/TestSet/Test_Case_1.csv")
data2<-fread("C:/Users/DELL/Documents/TestSet/Test_Case_3.csv")
data3<-fread("C:/Users/DELL/Documents/TestSet/Test_Case_5.csv")
data4<-fread("C:/Users/DELL/Documents/TestSet/Test_Case_7.csv")
data5<-fread("C:/Users/DELL/Documents/TestSet/Test_Case_9.csv")

test_set<-rbind(data1,data2,data3,data4,data5)
  
data6<-fread("C:/Users/DELL/Documents/Training/Features_Variant_1.csv")
data7<-fread("C:/Users/DELL/Documents/Training/Features_Variant_2.csv")
data8<-fread("C:/Users/DELL/Documents/Training/Features_Variant_3.csv")
data9<-fread("C:/Users/DELL/Documents/Training/Features_Variant_4.csv")
data10<-fread("C:/Users/DELL/Documents/Training/Features_Variant_5.csv")


train_set<-rbind(data6,data7,data8,data9,data10)
fbmsg<-rbind(test_set,data6)

colSums(is.na(fbmsg))


#2. Perform the below given activities:
# a. Predict the no of comments in next H hrs

x<-as.matrix(fbmsg[,1:53])
y<-as.matrix(fbmsg[,54])

########Note:-
#1. Use LASSO, Elastic Net and Ridge and other regression techniques that are covered in the module

library(glmnet)
fit_ridge<-glmnet(x,y, family = "gaussian", alpha = 0, lambda = 0.001)
lasso
summary(fit_ridge)
predictions1<-predict(lasso, x, type = "link")
mse1<-mean((y-predictions1)^2)
mse1

library(glmnet)
fit_lasso<-glmnet(x,y, family = "gaussian", alpha = 1, lambda = 0.001)
fit_lasso
summary(fit_lasso)
predictions2<-predict(fit_lasso, x, type = "link")
mse2<-mean((y-predictions2)^2)
mse2


fit_elnet<-glmnet(x,y, family = "gaussian", alpha = 0.5, lambda = 0.001)
fit_elnet
summary(fit_elnet)
predictions3<-predict(fit_elnet, x, type = "link")
mse3<-mean((y-predictions3)^2)
mse3

# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0

for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x, y, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}


# Plot solution paths:
par(mfrow=c(3,2))




library(earth)
model1<-earth(V54~., data = fbmsg)
model1

summary(model1)
evimp(model1)
predictions4<-predict(model1, fbmsg)

mse4<-mean((fbmsg$V54-predictions4)^2)
mse4

### step wise regression
base<-lm(V54~., fbmsg)
base

summary(base)

fitt<-step(base)
summary(fitt)
prediction5<-predict(fitt, fbmsg)

mse5<-mean((fbmsg$V54-prediction5)^2)
mse5



library(MASS)

fit_rgm<-rlm(V54~., data=fbmsg)
fit_rgm

#2. Report the training accuracy and test accuracy

#Lasso model accuracy prediction
mse<-mean((y-predictions2)^2)
mse

# Ridge model accuracy prediction
mse2<-mean((fbmsg$V54-predictions3)^2)
mse2

# stepwise model accuracy 
mse5<-mean((fbmsg$V54-prediction5)^2)
mse5

#3. compare with linear models and report the accuracy

library(lars)
x<-as.matrix(fbmsg[,1:53])
y<-as.matrix(fbmsg[,54])
fit<-lars(x,y,type = "lasso")
fit
summary(fit)

best_step<-fit$df[which.min(fit$RSS)]
best_step

predictions6<-predict(fit,x, s=best_step, type = "fit")$fit

mse6<-mean((y-predictions6)^2)
mse6

library(pls)

fit_pcr<-pcr(V54~., data=fbmsg, validation="CV")
fit_pcr
summary(fit_pcr)

predictions7<-predict(fit_pcr, fbmsg, ncomp = 6)
mse7<-mean((fbmsg$V54-predictions7)^2)
mse7

library(pls)
fit_pls<-plsr(V54~., data=fbmsg, validation="CV")
fit_pls
summary(fit_pls)

predictions8<-predict(fit_pls, fbmsg, ncomp = 6)


mse8<-mean((fbmsg$V54-predictions8)^2)
mse8

#4. create a graph displaying the accuracy of all models
# For plotting

plot(fit_lasso, xvar="lambda")
plot(fit10, main="LASSO")

plot(fit_ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit_elnet, xvar="lambda")
plot(fit5, main="Elastic Net")

