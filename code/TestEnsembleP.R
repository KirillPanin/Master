wrongly.classified <- function(class,predicted.classes){
  return(class==predicted.classes)
}

cfun <- function(a,b) {
  
  x=cbind(a$all.class.sample,b$all.class.sample)
  y=cbind(a$all.misclass.sample,b$all.misclass.sample)
  res<-list(all.class.sample=x,all.misclass.sample=y)
  res
}

cfunT<-function(a,b) {
  
  x=cbind(a$error,b$error)
  y=a$class.error+b$class.error
  z=a$times.select.inst+b$times.select.inst
  s=a$times.selected+b$times.selected
  
  res=list(error=x,class.error=y,times.select.inst=z,times.selected=s)
  res
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


# general.fun<- function(classifiers,BTree,ind,dti.tr,dti.trF,class.tr,dti.test,dti.testF,class.test,no_cores)
# {
#   train.data=as.data.frame(dti.tr[,ind,drop=FALSE])
#   train.data$Class=as.factor(class.tr)
#   #for Rotation
#   train.datF=as.data.frame(dti.trF[,ind,drop=FALSE])
#   train.datF$Class=as.factor(class.tr)
# 
#   test.data=as.data.frame(dti.test[,ind,drop=FALSE])
#   test.datF=as.data.frame(dti.testF[,ind,drop=FALSE])
# 
#   cl <- makeCluster(no_cores)
#   #cl <- makeCluster(length(classifiers))
#   registerDoParallel(cl)
# 
#   ptm <- proc.time()
# 
#   ls<-foreach(j=1:length(classifiers), .combine=cfun,.export=c('wrongly.classified','RotateClassifier','predict.Rotate'), .packages=c('randomForest','adabag','rpart')) %dopar%
#   #for (j in 1:length(classifiers))
#   {
# 
#     ### Nearest shrunken centroids
#     switch(classifiers[j],
# SimpleTree={
#   control <- rpart.control(cp = 0.001, minsplit = 3, xval = 0)
#   Simple.res <- rpart(Class ~., data = train.data, control=control)
#   class.sample=predict(Simple.res,newdata=test.data, type="class")
#   misclass.sample <- wrongly.classified(class.test,as.character(class.sample))
# 
#   all.class.sample=as.character(class.sample)
#   all.misclass.sample=misclass.sample
# },
# AdaBoost = {
#   all.class.sample=NULL
#   all.misclass.sample=NULL
#   control <- rpart.control(cp = 0.001, minsplit = 3, xval = 0)
#   for(i in 1:length(BTree))
#   {
#   AdaBoost.res <- boosting(Class~., data=train.data, boos=TRUE, mfinal=BTree[i],control=control)
# 
#   class.sample <-predict.boosting(AdaBoost.res, newdata=test.data,type="class")
#   #class.sample=sapply(class.sample, function(z) toString(z))
# 
#   misclass.sample <- wrongly.classified(class.test,class.sample$class)
# 
#   all.class.sample=cbind(all.class.sample,class.sample$class)
#   all.misclass.sample=cbind(all.misclass.sample,misclass.sample)
#   }
# },
# Bagging={
#   all.class.sample=NULL
#   all.misclass.sample=NULL
#   control <- rpart.control(cp = 0.001, minsplit = 3, xval = 0)
#   for(i in 1:length(BTree))
#   {
#   Bagging.res <-  bagging(Class~., data=train.data, mfinal=BTree[i],control=control)
# 
#   class.sample <-predict.bagging(Bagging.res, newdata=test.data,type="class")
#   #class.sample=sapply(class.sample, function(z) toString(z))
# 
#   misclass.sample <- wrongly.classified(class.test,class.sample$class)
# 
#   all.class.sample=cbind(all.class.sample,class.sample$class)
#   all.misclass.sample=cbind(all.misclass.sample,misclass.sample)
#   }
# 
# },
# RandomForest={
#   all.class.sample=NULL
#   all.misclass.sample=NULL
#   for(i in 1:length(BTree))
#   {
#     Forest.res <-  randomForest(Class~., data=train.data, ntree=BTree[i],na.action=na.omit)
# 
#     class.sample <-predict(Forest.res, newdata=test.data,type="class")
#     #class.sample=sapply(class.sample, function(z) toString(z))
# 
#     misclass.sample <- wrongly.classified(class.test,as.character(class.sample))
# 
#     all.class.sample=cbind(all.class.sample,as.character(class.sample))
#     all.misclass.sample=cbind(all.misclass.sample,misclass.sample)
#   }
# 
# },
# RotateCl={
#   all.class.sample=NULL
#   all.misclass.sample=NULL
#   control <- rpart.control(cp = 0.001, minsplit = 3, xval = 0)
#   for(i in 1:length(BTree))
#   {
#     Rotate.res=RotateClassifier(data=train.datF, ntree=BTree[i] , control=control)
#     out <-predict.Rotate(Rotate.res, data=test.datF,type="class")
#     class.sample=out$class.sample
#     misclass.sample <- wrongly.classified(class.test,class.sample)
# 
#     all.class.sample=cbind(all.class.sample,class.sample)
#     all.misclass.sample=cbind(all.misclass.sample,misclass.sample)
#   }
# }
# )
# res=list(all.class.sample= all.class.sample,all.misclass.sample=all.misclass.sample)
# res
# }
#   stopCluster(cl)
#   ptms=proc.time() - ptm
# ls
# }

TestEnsembleP<-function(matr,matrF,class,gene,simpleclass,ensemble,BTree,flagGene)
{
  
  no_cores <- detectCores() - 1
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
  
  num.sample=10 #количество повтрово разбиение на обучающее/тестовое
  num.test=1/10
  
  inddat <- 1:nrow(matr)
  #class=as.numeric(class)
  label=unique(class)
  
  # confus=array(0,c(length(label),length(label),length(dim3)))
  # dimnames(confus)=c(list(label),list(label),list(dim3))
  
  index.class=NULL
  size.sample=0
  for(i in 1:length(label))
  {
    index <-subset(inddat,class==label[i])
    index.class=c(index.class,list(index))
    size.sample=size.sample+ceiling(length(index)*num.test)
  }
  
  # Start the clock!
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)
  
  ptm <- proc.time()
  
  #for (ib in 1:num.sample)
  ls<-foreach(icount(num.sample),.combine=cfunT, .export=c('general.fun','wrongly.classified','RotateClassifier','predict.Rotate'), .packages=c('randomForest','adabag','rpart','WGCNA')) %dopar%
  {
    times.selected <- matrix(0,ncol(matr),feature.subset)
    
    dimnames(times.selected)=c(list(colnames(matr)),list(as.character(gene)))
    #for sub-sampling
    class.error<-array(0,c(nrow(matr),feature.subset,length(dim3)))
    attr(class.error, "dimnames")[[3]]=dim3
    
    error=array(0,c(feature.subset,length(dim3)))
    attr(error, "dimnames")[[2]]=dim3
    
    times.select.inst <- rep(0,nrow(matr))
    
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
        error[q,dim3]=length(index.test)-colSums(out$all.misclass.sample)
        # if(q==length(gene))
        # {
        # for(j in 1:length(dim3))
        # {
        #     vrem=table(class.test,out$all.class.sample[,j])
        #     confus[rownames(vrem),colnames(vrem),dim3[j]]=confus[rownames(vrem),colnames(vrem),dim3[j]]+vrem
        # }
        # }
    }
    
    res=list(error=error,class.error=class.error,times.select.inst=times.select.inst,times.selected=times.selected)
    res
    #ptms=proc.time() - ptm
  }
  stopCluster(cl)
  # Stop the clock
  ptms=proc.time() - ptm
  ptms
  
  error=ls$error
  class.error=ls$class.error
  times.select.inst=ls$times.select.inst
  times.selected=ls$times.selected
  
  if(feature.subset>1)
  {
    true.classified=NULL
    for(i in 1:feature.subset)
    {
      vrem=1-error[i,]/size.sample
      c(vrem)=dim(length(dim3),num.sample)
      true.classified=rbind(true.classified,vrem)
    }
    true.classified=t(true.classified)
    vrem=attr(error, "dimnames")[[2]][1:length(dim3)]
    colnames(true.classified)=rep(vrem,feature.subset)
    classscores <- data.frame(class,times.select.inst,class.error[,feature.subset,])
  }
  else
  {
    true.classified=1-error[1,]/size.sample
    dim(true.classified)=c(length(dim3),num.sample)
    true.classified=t(true.classified)
    colnames(true.classified)=attr(error, "dimnames")[[2]][1:length(dim3)]
    #boxplot(true.classified,main = "Cross validation",ylab = "Classification accuracy",xlab="Classifiers",col=3)
    classscores <- data.frame(class,times.select.inst,class.error[,1,])
  }
  
  
  time.correct <- rep(".time_correct",length(dim3))
  for (i in 1:length(dim3)){
    time.correct[i] <- paste(dim3[i],time.correct[i],sep="")
  }
  names(classscores) <- c("true.label","time.selected",time.correct)
 
  res <- list(nclassifier=classifiers,classifiers=dim3,predictions=classscores,no.selected=times.selected,true.classified=true.classified)#,confus=confus)
}  
  