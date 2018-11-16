
classifiers=c("SimpleTree","RotateCl","AdaBoost","Bagging","RandomForest")
simpleclass=c("SimpleTree")
#simpleclass=NULL
ensemble=c("RotateCl","AdaBoost","Bagging","RandomForest")

resul1 <- c(0.7, 0.75, 0.77, 0.79,0.82)
plot(BTree,resul1,type = "l",)
head(meanDataSet)
View(meanDataSet)
dfm <- as.data.frame(meanDataSet) 
df <- as.data.frame(listPlotEns)
View(df)
st<-dfm[1,]

dfAllVal <- read.table("balance50ans1sure.txt",header = T,sep = "") 
View(dfAllVal)
p <- ggplot() + 
  geom_line(data = dfPlotEns, aes(x=RotationForestApart,y=BTree, colour = "RotationForest",)) +
  geom_line(data = dfPlotEns, aes(x=AdaBoostApart,y=BTree, colour = "AdaBoost")) +
  geom_line(data = dfPlotEns, aes(x=BaggingApart,y=BTree, colour = "BaggingApart")) +
  geom_line(data = dfPlotEns, aes(x=RandomForestApart,y=BTree, colour = "RandomForest")) +
    labs(x = "Accuracy, %", y = "Quantity Ensembles", colour = "Сlassifiers") +
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#12E12A"))
p

p1 <- ggplot() + 
  geom_line(data = dfPlotEns, aes(x=RotationForestApart,y=BTree, colour = "RotationForest")) +
  geom_line(data = dfPlotEns, aes(x=AdaBoostApart,y=BTree, colour = "AdaBoost")) +
  geom_line(data = dfPlotEns, aes(x=BaggingApart,y=BTree, colour = "BaggingApart")) +
  geom_line(data = dfPlotEns, aes(x=RandomForestApart,y=BTree, colour = "RandomForest")) +
  labs(x = "Accuracy, %", y = "Quantity Ensembles", colour = "Сlassifiers") +
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#12E12A"))
p1

RotationForestApart <- dfm[2:(length(BTree)+1),]
AdaBoostApart <- dfm[(2+length(BTree)):(1+2*length(BTree)),]
BaggingApart <- dfm[(2+2*length(BTree)):(1+3*length(BTree)),]
RandomForestApart <- dfm[(2+3*length(BTree)):(1+4*length(BTree)),]

# List for plot"dependents accuracy from quantity ensembles"
listPlotEns <-list(RotationForestApart,AdaBoostApart,BaggingApart,RandomForestApart)
listPlotEns
dfPlotEns <- data.frame(RotationForestApart,AdaBoostApart,BaggingApart,RandomForestApart,BTree)
dfPlotEns

plot(listPlotEns[[4]],BTree)
library(ggplot2)
ggplot(listPlotEns[[1]]~BTree)
qplot(listPlotEns[[1]], BTree,geom =("smooth"),xlab="Accuracy", ylab="Quantity ensembles")

datasets=c("balance50ans1sure")
dataset<- read.table(file = "balance50ans1sure.txt",header = TRUE)

# Dependent accuracy from quantity ensembles
nEns <- seq(10,50,by=10)
head(dataset[,-1])
length(nEns)
meanDataSet <- apply(dataset,2,mean)
ff <- as.list(meanDataSet)
View(ff)

for(i in 1:length(ff)){
if(names(ff)=="SimleTree"){"SimpleTree"=0}
}
names(ff)
if(names(ff)=="SimleTree"){"SimpleTree"=0}
View(dd)


lsmn <- as.list(meanDataSet)
plot(lsmn[-1],nEns)
lsmn

View(dataset)
plot(meanDataSet)

numdata=length(datasets)
nclassif=length(classifiers)
ntree=length(BTree)

ncl=nclassif
if(!is.null(simpleclass))
{
  ncl=ncl-1
}

data<- read.table(file=paste("./Test Results/",datasets[1],".txt",sep=""),header=T,sep="\t")
nrow=nrow(data)
ncol=ncol(data)
if(!is.null(simpleclass))
{
  len=(nclassif-1)*ntree+1
}
else
{
  len=nclassif*ntree
}
if(ncol!=len) return(-1)

massiv=NULL
median=array(0,c(numdata,len))
mean=array(0,c(numdata,len))
for(i in 1:numdata)
{
  nfile=paste("./Test Results/",datasets[i],".txt",sep="")
  data<- read.table(file=nfile,header=T,sep="\t")
  massiv=c(massiv,list(data))
  mm=sapply(1:(len),function(z) median(data[,z]))
  median[i,]= mm
  mm=apply(data,2,mean)
  mean[i,]= mm
}

