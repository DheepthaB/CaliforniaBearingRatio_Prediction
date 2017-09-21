n=function(x)
{
  nr=(x-min(x))/(max(x)-min(x))
  return(n=nr)
}
set.seed(seed=2)
md=read.csv("soil1.csv")
summary(md)
rg=max(md[,12])-min(md[,12])
mn=min(md[,12])
cbr=md[,12]
md=md[,-1]

for(i in 1:11)
{
  md[,i]=n(md[,i])
}
target=md[,11]
test=md[,-11]

#Neural net
library(nnet)
library(caret)
fit=nnet(CBR~.,data=md, size=5,rang=0.1, decay=5e-4, maxit=500, na.action=na.omit)
summary(fit)
p=predict(fit, test)
pcbr=(p*rg)+mn
soil=c(1:15)
dev.new(width=15,height=5)
plot(soil,cbr,type="b",col="red",xlab="Soil samples",ylab="CBR")
lines(soil,pcbr,type = "b")
legend(7,3.5,c("Predicted CBR","Original CBR"),lty=c(1,1),lwd=c(2,2),col=c("black","red"))
rn=RMSE(p,md[,11])
rwn=RMSE(pcbr,cbr)
rwn
rn

#PCA and ANN
library(caret)
fit1=pcaNNet(CBR~.,data=md,size=5,maxit=500,decay=5e-4,na.action=na.omit)
coef(fit1$finalModel)
p1=predict(fit1, test)
pcbr=(p1*rg)+mn
x=c(1:15)
dev.new(width=15,height=5)
plot(x,cbr,col="red",type = "b",xlab="Soil samples",ylab="CBR")
lines(x,pcbr,type="b")
legend(7,3.5,c("Predicted CBR","Original CBR"),lty=c(1,1),lwd=c(2,2),col=c("black","red"))
library(rminer)
print(mmetric(pcbr,cbr,metric="RMSE"))
print(mmetric(p1,md[,11],metric="RMSE"))
print(mmetric(pcbr,cbr,metric="MAE"))
print(mmetric(p1,md[,11],metric="MAE"))
print(mmetric(pcbr,cbr,metric="RSE"))
print(mmetric(p1,md[,11],metric="RSE"))
print(mmetric(pcbr,cbr,metric="COR"))
print(mmetric(p1,md[,11],metric="COR"))
print(mmetric(pcbr,cbr,metric="R2"))
print(mmetric(p1,md[,11],metric="R2"))


library(devtools)
library(reshape)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
fit1$pc
fit1$model
fit1$names
#plot each model
dev.new(width=5,height=5)
plot.nnet(fit)

