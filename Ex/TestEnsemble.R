wrongly.classified <- function(class,predicted.classes){
  return(class==predicted.classes)
}

general.fun<- function(classifiers,BTree,ind,dti.tr,dti.trF,class.tr,dti.test,dti.testF,class.test)
{
  all.class.sample=NULL
  all.misclass.sample=NULL
  
  train.data=as.data.frame(dti.tr[,ind,drop=FALSE])
  train.data$Class=as.factor(class.tr)
  #for Rotation
  train.datF=as.data.frame(dti.trF[,ind,drop=FALSE])
  train.datF$Class=as.factor(class.tr)
  
  test.data=as.data.frame(dti.test[,ind,drop=FALSE])
  test.datF=as.data.frame(dti.testF[,ind,drop=FALSE])
  
  for (j in 1:length(classifiers)){
    
    ### 
    switch(classifiers[j],
SimpleTree={
  control <- rpart.control(cp = 0.01, minsplit = 3, xval = 0)
  Simple.res <- rpart(Class ~., data = train.data, control=control)
  class.sample=predict(Simple.res,newdata=test.data, type="class")
  misclass.sample <- wrongly.classified(class.test,as.character(class.sample))
  
  all.class.sample=cbind(all.class.sample,as.character(class.sample))
  all.misclass.sample=cbind(all.misclass.sample,misclass.sample)
},
AdaBoost = {
  control <- rpart.control(cp = 0.01, minsplit = 3, xval = 0)
  for(i in 1:length(BTree))
  {
  AdaBoost.res <- boosting(Class~., data=train.data, boos=TRUE, mfinal=BTree[i],control=control)
  
  class.sample <-predict.boosting(AdaBoost.res, newdata=test.data,type="class")
  #class.sample=sapply(class.sample, function(z) toString(z))
  
  misclass.sample <- wrongly.classified(class.test,class.sample$class)
  
  all.class.sample=cbind(all.class.sample,class.sample$class)
  all.misclass.sample=cbind(all.misclass.sample,misclass.sample)
  }
},
Bagging={
  control <- rpart.control(cp = 0.01, minsplit = 3, xval = 0)
  for(i in 1:length(BTree))
  {
  Bagging.res <-  bagging(Class~., data=train.data, mfinal=BTree[i],control=control)
  
  class.sample <-predict.bagging(Bagging.res, newdata=test.data,type="class")
  #class.sample=sapply(class.sample, function(z) toString(z))
  
  misclass.sample <- wrongly.classified(class.test,class.sample$class)
  
  all.class.sample=cbind(all.class.sample,class.sample$class)
  all.misclass.sample=cbind(all.misclass.sample,misclass.sample)
  }
  
},
RandomForest={
  for(i in 1:length(BTree))
  {
    Forest.res <-  randomForest(Class~., data=train.data, ntree=BTree[i],na.action=na.omit)
    
    class.sample <-predict(Forest.res, newdata=test.data,type="class")
    #class.sample=sapply(class.sample, function(z) toString(z))
    
    misclass.sample <- wrongly.classified(class.test,as.character(class.sample))
    
    all.class.sample=cbind(all.class.sample,as.character(class.sample))
    all.misclass.sample=cbind(all.misclass.sample,misclass.sample)
  }

},
RotateCl={
  control <- rpart.control(cp = 0.01, minsplit = 3, xval = 0)
  for(i in 1:length(BTree))
  {
    Rotate.res=RotateClassifier(data=train.datF, ntree=BTree[i] , control=control)
    out <-predict.Rotate(Rotate.res, data=test.datF,type="class")
    class.sample=out$class.sample
    misclass.sample <- wrongly.classified(class.test,class.sample)
    
    all.class.sample=cbind(all.class.sample,class.sample)
    all.misclass.sample=cbind(all.misclass.sample,misclass.sample)
  }
}
)
  }
  return(list( all.class.sample= all.class.sample,all.misclass.sample=all.misclass.sample))
}

TestEnsemble<-function(matr,matrF,class,gene,simpleclass,ensemble,BTree,flagGene)
{
  
  feature.subset=length(gene)
  
  if(length(ensemble)==0)
  {
    dim3=simpleclass
    classifiers=simpleclass
  }
  else
  {
    nn=sapply(ensemble, function(z) paste(z,BTree,sep=''))
    dim3=c(simpleclass,as.vector(nn))
    classifiers=c(simpleclass,ensemble)
  }
  
  times.selected <- matrix(0,ncol(matr),feature.subset)
  
  dimnames(times.selected)=c(list(colnames(matr)),list(as.character(gene)))
  #for sub-sampling
  class.error<-array(0,c(nrow(matr),feature.subset,length(dim3)))
  attr(class.error, "dimnames")[[3]]=dim3
  
  num.sample=6 #количество повтрово разбиение на обучающее/тестовое
  num.test=1/10
  
  times.select.inst <- rep(0,nrow(matr))
  
  inddat <- 1:nrow(matr)
  #class=as.numeric(class)
  label=unique(class)
  
  confus=array(0,c(length(label),length(label),length(dim3)))
  dimnames(confus)=c(list(label),list(label),list(dim3))
  
  index.class=NULL
  size.sample=0
  for(i in 1:length(label))
  {
    index <-subset(inddat,class==label[i])
    index.class=c(index.class,list(index))
    size.sample=size.sample+ceiling(length(index)*num.test)
  }
  
  error=array(0,c(num.sample,feature.subset,length(dim3)))
  attr(error, "dimnames")[[3]]=dim3
  
  # Start the clock!
  ptm <- proc.time()
  
  for (ib in 1:num.sample)
  {
    index.test=NULL
    for(i in 1:length(label))
    {
      vrem=ceiling(num.test*length(index.class[[i]]))
      if(vrem>0)
      {
      index=sample(index.class[[i]],vrem) #test cases
      index.test=c(index.test,index)
      }
    }
    
    times.select.inst[index.test] <- times.select.inst[index.test] + 1
    #index.test использовать для оценки object error и prediction vote использовать если classifier=1 and gene=fix
    
    dti.tr <- matr[-index.test,]
    dti.trF <- matrF[-index.test,]
    class.tr=class[-index.test]
    dti.test<- matr[index.test,]
    dti.testF<- matrF[index.test,]
    class.test=class[index.test]
    
    # отбор признаков критерий отношение среднеквадратичного отклонения между группами к отклонению в группах
    # выполняется только для данных экспресии генов
    if(flagGene)
    {
    vrem=t(dti.tr)
    res=stat.bwss(vrem, as.numeric(class.tr))
    s_out=sort(res$bss,decreasing = TRUE,index.return=TRUE)
    ssel=s_out$ix
    }
    else
    {
      ssel=1:ncol(matr)
    }
    # resamplimg estimation 

     
    for(q in 1:length(gene))
    {
        ind=ssel[1:gene[q]]
        times.selected[ind,q] <- times.selected[ind,q] + 1
        
        out=general.fun(classifiers,BTree,ind,dti.tr,dti.trF,class.tr,dti.test,dti.testF,class.test)
        class.error[index.test,q,dim3]=class.error[index.test,q,dim3]+out$all.misclass.sample
        error[ib,q,dim3]=length(index.test)-colSums(out$all.misclass.sample)
        if(q==length(gene))
        {
        for(j in 1:length(dim3))
        {
            vrem=table(class.test,out$all.class.sample[,j])
            confus[rownames(vrem),colnames(vrem),dim3[j]]=confus[rownames(vrem),colnames(vrem),dim3[j]]+vrem
        }
        }
    }

    cat(paste("Iteration",ib))
    # ptms=proc.time() - ptm
  }
  
  # Stop the clock
  ptms=proc.time() - ptm
  ptms
  
  if(feature.subset>1)
  {
    true.classified=NULL
    for(i in 1:feature.subset)
    {
      true.classified=cbind(true.classified,1-error[,i,dim3]/size.sample)
    }
    classscores <- data.frame(class,times.select.inst,class.error[,feature.subset,])
  }
  else
  {
    true.classified=1-error[,1,dim3]/size.sample
    
    dim(true.classified)=c(num.sample,length(dim3))
    colnames(true.classified)=attr(error, "dimnames")[[3]]
    #boxplot(true.classified,main = "Cross validation",ylab = "Classification accuracy",xlab="Classifiers",col=3)
    classscores <- data.frame(class,times.select.inst,class.error[,1,])
  }
  
  
  time.correct <- rep(".time_correct",length(dim3))
  for (i in 1:length(dim3)){
    time.correct[i] <- paste(dim3[i],time.correct[i],sep="")
  }
  names(classscores) <- c("true.label","time.selected",time.correct)
 
  res <- list(nclassifier=classifiers,classifiers=dim3,predictions=classscores,no.selected=times.selected,true.classified=true.classified,confus=confus)
}  
  