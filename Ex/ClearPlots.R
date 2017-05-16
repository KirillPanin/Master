require(ggplot2)
# STACKED AREA CHART

# Initialization and read data
BTree<-seq(10,100,by=10)
dt <- read.table(file="Pima.txt",header = T)
dt <- dt[,-1] 

# Calculate mean value of 100 instances 
meanDataSet <- apply(dt,2,mean)
meanDataSet <- as.vector(meanDataSet)
plot(meanDataSet)
# Calculate ratio between Accuracy
vec1<-c()
for(i in 1:length(meanDataSet)){
  if(i==11){break()}
  cof <- (meanDataSet[[i]]+meanDataSet[[i+length(Btree)]]+meanDataSet[[i+2*length(Btree)]]+
            meanDataSet[[i+3*length(Btree)]])/100
    #v10 <- c(meanDataSet[[i]]/cof, meanDataSet[[i+length(Btree)]]/cof, meanDataSet[[i+2*length(Btree)]]/cof, meanDataSet[[i+3*length(Btree)]]/cof)  
    #print(cof)    
  #print(paste(meanDataSet[[i]]/cof,meanDataSet[[i+length(Btree)]]/cof,meanDataSet[[i+2*length(Btree)]]/cof, meanDataSet[[i+3*length(Btree)]]/cof))
  vec1<-c(vec1,meanDataSet[[i]]/cof,meanDataSet[[i+length(Btree)]]/cof,meanDataSet[[i+2*length(Btree)]]/cof, meanDataSet[[i+3*length(Btree)]]/cof)
  #a1<-meanDataSet[[i]]/cof; a2<-meanDataSet[[i+length(Btree)]]/cof;
  #a3<-meanDataSet[[i+2*length(Btree)]]/cof; a4<- meanDataSet[[i+3*length(Btree)]]/cof;
  #print(a1+a2+a3+a4)
  }

# Create vecors with inf for df
NamesCL <- (rep(c("Rotation Forest","AdaBoost","Bagging","RandomForest"),length(BTree)))
QEns <- c()
for(i in 1:length(BTree)){
  QEns<-c(QEns,rep(BTree[[i]],4))
}
df <-data.frame(NamesCL,QEns,vec1)


ggplot(df, aes(x=QEns, y=vec1, fill=NamesCL)) + 
  geom_area()

########## INITIAL FOR ANOTHER DF2 ###############
NamesCL <- c(rep("RotateCL",length(BTree)),rep("AdaBoost",length(BTree)),rep("Bagging",length(BTree)),rep("RandomForest",length(BTree)))
ensembles<-c("RotateCl","AdaBoost","Bagging","RandomForest")
BTree
RotationForestApartdf2 <- meanDataSet[1:10]
AdaBoostApartdf2 <- meanDataSet[11:20]
BaggingApartdf2 <- meanDataSet[21:30]
RandomForestApartdf2 <- meanDataSet[31:40]
df2 <- data.frame(BTree,RotationForestApartdf2,AdaBoostApartdf2,BaggingApartdf2,RandomForestApartdf2)

# Plot for wins
vec2<-c(10,4,2,4,8,3,5,4,16,1,1,2,5,5,2,8,7,3,4,6,10,4,2,4,8,3,5,4,16,1,1,2,5,5,2,8,7,3,4,6)
NamesCL <- (rep(c("Rotation Forest","AdaBoost","Bagging","RandomForest"),length(BTree)))
df3<-data.frame(NamesCL,QEns,vec2)
ggplot(df3, aes(x=QEns, y=vec2, fill=NamesCL)) + 
  geom_area()

##################### PIE ####################
# for(i in 1:length(vec1){
#   if(i>4){break()};
#   list[i]<-vec1[i]+vec1[i+4]+vec1[i+2*4]+vec1[i+3*4]+vec1[i+4*4]+vec1[i+5*4]+vec1[i+6*4]+vec1[i+7*4]+vec1[i+8*4]+vec1[i+9*4];
# }
# srotf<-vec1[i]+vec1[i+4]+vec1[i+2*4]+vec1[i+3*4]+vec1[i+4*4]+vec1[i+5*4]+vec1[i+6*4]+vec1[i+7*4]+vec1[i+8*4]+vec1[i+9*4]
Prop = c(sum(vec1[1:10]),sum(vec1[11:20]),sum(vec1[21:30]),sum(vec1[31:40]))
pie(Prop , labels = c("RotationForest","Bagging","Boosting","RandomForest"))

###################### LINE PLOT FOR 100  ###########################
library(latticeExtra)

xyplot(RotationForestApartdf2+AdaBoostApartdf2+BaggingApartdf2+RandomForestApartdf2 ~ BTree, df2, type = "l")

p1 <- ggplot() + 
  geom_line(data = df2, aes(y=RotationForestApartdf2,x=BTree, colour = "RotationForest")) +
  geom_line(data = df2, aes(y=AdaBoostApartdf2,x=BTree, colour = "AdaBoost")) +
  geom_line(data = df2, aes(y=BaggingApartdf2,x=BTree, colour = "BaggingApart")) +
  geom_line(data = df2, aes(y=RandomForestApartdf2,x=BTree, colour = "RandomForest")) +
  labs(y = "Accuracy, %", x = "Quantity Ensembles", colour = "Сlassifiers") +
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#12E12A"))
p1

# Plot ROC
basicplot <- ggplot(df, aes(d = vec1, m = QEns)) + geom_roc()
basicplot

######### FOR DATA RESULTS INITIAL VALUES ################

classifiers=c("SimpleTree","RotateCl","AdaBoost","Bagging","RandomForest")
simpleclass=c("SimpleTree")
ensemble=c("RotateCl","AdaBoost","Bagging","RandomForest")
BTree=seq(10,100,by=10)
gene=2
DataSetsResult(classifiers,simpleclass,BTree,gene)

# STACKED AREA by WINS from dataresult
rate.table<-DataSetsResult(classifiers,simpleclass,BTree,gene,type=3)
rate.df <- data.frame(rate.table[-5,])
rate.df <- data.frame(rate.df,ensFrate,winss11)
ensFrate<-c("RandomForest","AdaBoost","RotateCl","Bagging")
BTree
View(rate.df)
NamesCL <- (rep(c("Rotation Forest","AdaBoost","Bagging","RandomForest"),length(BTree)))
df3<-data.frame(NamesCL,QEns,vec2)
ggplot(rate.df, aes(x=winss11, y=rank, fill=ensFrate)) + 
  geom_area()



##### FINAL TABLE ##############
datasets=c("BreastCancer","Vehicle")
dtRes <- read.table(file=paste("./Test Results/",datasets[2],".txt",sep=""),header = T,sep="\t")

# Calculate mean and sd, add in frame
mean <- apply(dtRes,2,mean)
sd <- apply(dtRes,2,sd)
tab <- data.frame(mean,sd)
tab.simpletree<-data.frame(tab[1,],datasets[1])
tab.sTreefPlot <- data.frame(rep(tab.simpletree,10),BTree)
tab.sTreefPlot
tab <- tab[-1,]
View(tab)

# Establishment vector with name classifiers 
#BTree<-seq(10,100,by=10)
NamesCL <- (rep(c("Rotation Forest","AdaBoost","Bagging","RandomForest"),length(BTree)))
NamesCLSeq <-c(rep("Rotation Forest",length(BTree)),rep("AdaBoost",length(BTree)),rep("Bagging",length(BTree)),rep("RandomForest",length(BTree)))
EnsemSeq <- c(rep(BTree,4))

# Adding dataframe all values for 
tab <- data.frame(tab,NamesCLSeq,EnsemSeq)
View(tab1)
tab1 <- data.frame(Rotationcl=tab$mean[1:10],AdaBoost=tab$mean[11:20],Bagging=tab$mean[21:30],RandomForest=tab$mean[31:40],BTree)

# Table with all values

AllMean <- apply(tab1[1:4],2,mean)
Allsd <- apply(tab1[1:4],2,sd)
dataTableall<-data.frame(datasets,RotationCL=paste0(round(AllMean[[1]],6),"±",round(Allsd[[1]],6)),
                         AdaBoost=paste0(round(AllMean[[2]],6),"±",round(Allsd[[2]],6)),
                          Bagging=paste0(round(AllMean[[3]],6),"±",round(Allsd[[3]],6)),
                           RandomForest=paste0(round(AllMean[[4]],6),"±",round(Allsd[[4]],6)))


View(dataTableall)
#nw <- paste0(round(AllMean[[1]],6),"±",round(Allsd[[1]],6))

##### FINAL TABLE FOR LOOP ##############
# INITIAL VALUES
# Establishment vector with name classifiers 
BTree<-seq(10,100,by=10)
NamesCL <- (rep(c("Rotation Forest","AdaBoost","Bagging","RandomForest"),length(BTree)))
NamesCLSeq <-c(rep("Rotation Forest",length(BTree)),rep("AdaBoost",length(BTree)),rep("Bagging",length(BTree)),rep("RandomForest",length(BTree)))
EnsemSeq <- c(rep(BTree,4))

datasets=c("BreastCancer","Vehicle","Sonar","Pima")
dddd<-{}
ddd<-data.frame()

for(i in 1:length(datasets)){
dtRes <- read.table(file=paste("./Test Results/",datasets[i],".txt",sep=""),header = T,sep="\t")

# Calculate mean and sd, add in frame
mean <- apply(dtRes,2,mean)
sd <- apply(dtRes,2,sd)
tab <- data.frame(mean,sd)
tab.simpletree<-data.frame(tab[1,],datasets[1])
tab.sTreefPlot <- data.frame(rep(tab.simpletree,10),BTree)
tab.sTreefPlot
tab <- tab[-1,]

# Adding dataframe all values for 
tab <- data.frame(tab,NamesCLSeq,EnsemSeq)
tab1 <- data.frame(Rotationcl=tab$mean[1:10],AdaBoost=tab$mean[11:20],Bagging=tab$mean[21:30],RandomForest=tab$mean[31:40],BTree)
# Table with all values
AllMean <- apply(tab1[1:4],2,mean)
Allsd <- apply(tab1[1:4],2,sd)
dd<-data.frame(datasets=datasets[i],RotationCL=paste0(round(AllMean[[1]],6),"±",round(Allsd[[1]],6)),
                         AdaBoost=paste0(round(AllMean[[2]],6),"±",round(Allsd[[2]],6)),
                         Bagging=paste0(round(AllMean[[3]],6),"±",round(Allsd[[3]],6)),
                         RandomForest=paste0(round(AllMean[[4]],6),"±",round(Allsd[[4]],6)))
ddd<-rbind(ddd,dd)

}
View(ddd)
df <- data.frame(dddd[1:5],dddd[6:10],dddd[11:15],dddd[16:20])
View(df)

# Plot tests
require(ggplot2)
ggplot(tab, aes(x=EnsemSeq, y=mean, fill=NamesCLSeq)) + 
  geom_area()

p1 <- ggplot() + 
  geom_line(data = tab1, aes(y= tab1$Rotationcl,x=tab1$BTree, colour = "RotationForest")) +
  geom_line(data = tab1, aes(y=tab1$AdaBoost,x=tab1$BTree, colour = "AdaBoost")) +
  geom_line(data = tab1, aes(y=tab1$Bagging,x=tab1$BTree, colour = "BaggingApart")) +
  geom_line(data = tab1, aes(y=tab1$RandomForest,x=tab1$BTree, colour = "RandomForest")) +
  #geom_line(data = tab.sTreefPlot, aes(y=tab.sTreefPlot$mean,x=tab.sTreefPlot$BTree, colour = "SimpleTree"))+
  labs(y = "Accuracy, %", x = "Quantity Ensembles", colour = "Сlassifiers") +
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#12E12A","#D55E00"))
p1

