#flagGene=TRUE для отбора признаков. Используется только с данными генной экспрессии
Experiment<-function(data,nfile="result.txt",flagGene=FALSE)
{
  c.method="sub-sampling"
  
  library(WGCNA)
  library(rpart)
  library(adabag)
  library(randomForest)
  library(mlbench)
  #library(parallel)
  library(foreach)
  library(doParallel)
  
  debugSource("TestEnsemble.R")
  source("GraphResult.R")
  source("ReadDataSet.R")
  debugSource("RotateClassifier.R")
  
  res=ReadDataSet(data)
  matr=res$matr
  class=res$class
  gene=res$gene
  
  out=sapply(1:ncol(matr), function(z) is.factor(matr[,z]))
  matrF=matr
  
  if(any(out))
  {
  #convert nominal to numeric используется только в алгоритме RotateClassifier
  matrF=NULL
  nameF=NULL
  for(i in 1:ncol(matr))
  {
    if(is.factor(matr[[i]]))
    {
      nlevel=levels(matr[[i]])
      vrem=matrix(0,nrow(matr),length(nlevel))
      for(j in 1:length(nlevel))
      {
        index=which(matr[[i]]==nlevel[j])
        vrem[index,j]=1
      }
      matrF=cbind(matrF,vrem)
      nameF=c(nameF,paste(rownames(matr)[i],1:length(nlevel),sep=""))
    }
    else
    {
      matrF=cbind(matrF,matr[[i]])
      nameF=c(nameF,rownames(matr)[i])
    }
  }
  colnames(matrF)=nameF
  }
  
  #select number of trees in ensemble classifiers
  BTree=seq(30,40,by=10)
  
  simpleclass=c("SimpleTree")
  #simpleclass=NULL
  ensemble=c("RotateCl","AdaBoost","Bagging","RandomForest")
 
  out=TestEnsemble(matr,matrF,class,gene,simpleclass,ensemble,BTree,flagGene)
  #запись выхода в файл
  
  write.table(out$true.classified, file = nfile, sep = "\t", col.names = TRUE,row.names=FALSE)
  
  GraphResult(nfile,out$nclassifier,simpleclass,BTree,gene)
  
}