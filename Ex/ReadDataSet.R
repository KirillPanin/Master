ReadDataSet<-function(data)
{
  # ћожно открыть и протестировать на наборах
  # datasets=c("BreastCancer","PimaIndiansDiabetes","Ionosphere","Glass","LetterRecognition","Sonar","Soybean","Vehicle",
  #   "Vowel","Zoo")
  #datasets=c("BreastCancer","PimaIndiansDiabetes","Sonar","Vehicle")
  
  
  #Expression Leukemia
  # data<- read.table(file="data_set_all_leu.txt",header=T,sep=',')
  #standartization
  # matr=data[,-1]
  # class=data[,1]
  # matr=scale(matr)
  # select genes
  # gene=seq(10,200,by=20)
  # gene=c(50)
  
  #Expression Limphoma
  # data<- read.table(file="Tab-DLBCL.norm.txt",header=T,sep=' ')
  # #standartization
  # matr=data[,-ncol(data)]
  # class=data[,ncol(data)]
  # matr=scale(matr)
  # #select genes
  # gene=seq(10,200,by=20)
  # gene=c(50)
  
  #BreastCancer
  # matr1=data[,-c(1,ncol(data))]
  # matr=sapply(1:ncol(matr1),function(z) as.numeric(matr1[[z]]))
  # colnames(matr)=colnames(matr1)
  # class=data[,ncol(data)]
  # gene=ncol(matr)
  
  #PimaDiabet
  # matr=data[,-ncol(data)]
  # matr=scale(matr)
  # class=data[,ncol(data)]
  # gene=ncol(matr)
  
  #Sonar
  # matr=data[,-ncol(data)]
  # matr=scale(matr)
  # class=data[,ncol(data)]
  # gene=ncol(matr)
   
  #Vehicle
  # data<-Vehicle
  # matr=data[,-ncol(data)]
  # matr=scale(matr)
  # class=data[,ncol(data)]
  # data[,ncol(data)]
  # gene=ncol(matr)


  
  #Parkinsons
  # data<- read.table(file="parkinsons.data.txt",header=T,sep=',')
  # data<-data[,-1]
  # as.numeric(data$Shimmer.APQ3)
  # matr=data # subtract class 
  # class=data$status
  # matr=scale(matr)
  #View(matr)
  #Impute Missing Values by median/mode.
  
  #Hepatitis.data.txt
  # Missing Values=Yes;
  # data<-read.table('hepatitis.data.txt',sep=',')
  # View(data)
  
  
 #wine.data
  # data <- read.table("wine.data.txt",sep=',')
  # #View(data)
  # #View(matr)
  # matr=data[,-1]
  # matr=scale(matr)
  # class=data$V1
  # gene=ncol(matr)
  
  # haberman.data 
   # data <- read.table("haberman.data.txt",sep=',')
   # #View(data)
   # matr=data[,-ncol(data)]
   # matr=scale(matr)
   # class=data[,ncol(data)]
   # data[,ncol(data)]
   # gene=ncol(matr)
  
 # balance.data 
   # data <- read.table("balance.dat",sep=',')
   # #View(data)
   # matr=data[,-ncol(data)]
   # matr=scale(matr)
   # class=data[,ncol(data)]
   # data[,ncol(data)]
   # gene=ncol(matr)
  
 # tae +
  # data <- read.table("tae.dat",sep=',')
  # #View(data)
  # matr=data[,-ncol(data)]
  # matr=scale(matr)
  # class=data[,ncol(data)]
  # data[,ncol(data)]
  # gene=ncol(matr)
   
 # newthyroid.dat
 #  data <- read.table("newthyroid.dat",sep=',')
 # # View(data)
 #  matr=data[,-ncol(data)]
 #  matr=scale(matr)
 #  class=data[,ncol(data)]
 #  data[,ncol(data)]
 #  gene=ncol(matr)
   
 # phoneme.data 
  # data <- read.table("phoneme.dat",sep=',')
  # View(data)
  # matr=data[,-ncol(data)]
  # matr=scale(matr)
  # class=data[,ncol(data)]
  # data[,ncol(data)]
  # gene=ncol(matr)
  
 # bupa +
  # data <- read.table("bupa.dat",sep=',')
  # #View(data)
  # matr=data[,-ncol(data)]
  # matr=scale(matr)
  # class=data[,ncol(data)]
  # data[,ncol(data)]
  # gene=ncol(matr)
  
  
  # elephant
  # data <- read.table("elephant.dat",sep=',')
  # #View(data)
  # matr=data[,-ncol(data)]
  # matr=scale(matr)
  # class=data[,ncol(data)]
  # data[,ncol(data)]
  # gene=ncol(matr)

  res<-list(matr=matr, class=class, gene=gene)
}

