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
  matr=data[,-ncol(data)]
  matr=scale(matr)
  class=data[,ncol(data)]
  gene=ncol(matr)
  
  #Impute Missing Values by median/mode.
  
  
  res<-list(matr=matr, class=class, gene=gene)
}