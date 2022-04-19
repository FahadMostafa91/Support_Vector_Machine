install.packages("e1071")
library("e1071")
install.packages("ROCR")
library("ROCR")
library(readxl)
svm_data <- read_excel("Downloads/svm_data.xlsx")
head(svm_data)

x = data.frame(svm_data$age, svm_data$sex, svm_data$days_after_vac_category, svm_data$vac_category, svm_data$pre_dis, svm_data$pre_ill)
head(x)
y = as.factor(svm_data$hospitalzed)

dat= data.frame(x, y)
head(dat)

train =sample(1826, 1278)
svmfit = svm(y~. , data=dat[train,], kernel="linear", cost=.10, scale = FALSE)
summary(svmfit)
ypred = predict(svmfit,dat)
table(predict=ypred, truth=dat$y)

svmfit.opt = svm(y~. , data=dat[train,], kernel="linear", cost=.10, decision.values =T)

par(mfrow = c(1,2))
plot(ypred,y)



# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(dat), size = floor(.75*nrow(dat)), replace = F)
dat.training <- dat[sample, ]
dat.test  <- dat[-sample, ]
# Linear Kernel
tobj <- tune.svm(y ~., data =dat.training,
                 cost = c(1,2,3,5,10,15,20,25), 
                 nrepeat=2, tunecontrol = tune.control(sampling = "cross",cross=3))
bestcost<- tobj$best.parameters[[1]];bestcost

fit.svm1 <- svm(y ~., data =dat.training, probability=TRUE, cost = bestcost,kernal="linear") 
pred.svm1<- predict(fit.svm1, type="prob", newdata=test, probability = TRUE)
#pred.svm1<-(pred.svm1-min(pred.svm1))/(max(pred.svm1)-min(pred.svm1))


#PREDICTION and missclassification error rate
pred.miss<-ifelse(pred.svm1<.5,0,1)
pred.miss<-as.numeric(as.vector(pred.miss))
miss.class<-table(actual =y.test, fitted = pred.miss)
miss.class.rate.lin<-1-sum(diag(miss.class))/sum(miss.class)
miss.class.rate.lin

#Area under ROC curve
AUC<- roc.area(obs=y.test, pred=pred.svm1)$A
SVM.lin<- verify(obs=y.test, pred=pred.svm1)

roc.plot(SVM.lin, plot.thres = NULL, col="red",main ="ROC Curve of Support Vector Machine")
text(x=0.7, y=0.2, paste("ROC =", round(AUC, digits=4), 
                         sep=" "), col="blue", cex=1.2)

AUC.SVM.lin<- AUC


