GraphResult<-function(nfile,classifiers,simpleclass,BTree,gene,boxflag=FALSE, type=FALSE)
{
  #indexE - индекс 
  # т.е. анализ только при варьируюшемся числе деревьев или числе признаков
  if((length(gene)>1)&&(length(BTree)>1)) return(-1) 
  data<- read.table(file=nfile,header=T,sep="\t")
  nclassif=length(classifiers)
  ntree=length(BTree)
  ngene=length(gene)
  par(mfrow=c(1,1))
  
  if((ntree>1)&&(ngene==1))
  {
    #убираем столбец отвечающий за простое дерево
    if(!is.null(simpleclass))
    {
      data=data[,-1]
      nclassif=nclassif-1
      classifiers=classifiers[-1]
    }
    
    if(type)
    {
    if(boxflag)
    {
      if(nclassif==1)
      {
        vrem=matrix(1:ncol(data),ncol(data),1)
      }
      else
      {
        vrem=sapply(classifiers,function(z) grep(z,colnames(data)))
      }
      
      #par(mfrow=c(length(classifiers),1))
      par(mfrow=c(1,1))
      for(i in 1:nclassif)
      {
        if(ntree==1)
        {
          values=data[,vrem[i],drop=FALSE]
        }
        else
        {
          values=data[,vrem[,i],drop=FALSE]
        }
        colnames(values)=BTree
        box=boxplot(values,main = paste("Cross validation",classifiers[i]),ylab = "Classification accuracy",xlab="n of trees",col=i+1)
      }
    }
    else
    {
    #график изменения точности для классификаторов с изменением кол-ва деревьев BTree (quartile)
    # accur=sapply(1:(ntree*nclassif),function(z) quantile(data[,z], probs = c(0.25, 0.5, 0.75)))
    #   dim(accur)=c(3,ntree,nclassif)
    #   plot(0,0,xlim = c(0,100),ylim = c(0.92,1.0),type = "n",xlab="Ensemble size",ylab="Accuracy")#,xaxt="n")
    #   for (i in 1:nclassif){
    #     lines(BTree,accur[2,,i],col = i,type = 'b',lwd=2)
    #     for(j in 1:ntree)
    #     {
    #       lines(rep(BTree[j],2),c(accur[1,j,i],accur[3,j,i]),lty=2,col = i)
    #     }
    #   }
    #график изменения точности для классификаторов с изменением кол-ва деревьев BTree (mean, sd)     
    mean=apply(data,2,mean)
    sd=apply(data,2,sd)
    dim(mean)=c(ntree,nclassif)
    dim(sd)=c(ntree,nclassif)

    plot(0,0,xlim = c(0,100),ylim = c(0.5,1.0),type = "n",xlab="Ensemble size",ylab="Accuracy",xaxt="n")
    for (i in 1:nclassif){
      lines(BTree,mean[,i],col = i,type = 'b',lwd=2)
      for(j in 1:ntree)
      {
      lines(rep(BTree[j],2),c(mean[j,i]-sd[j,i],mean[j,i]+sd[j,i]),lty=2,col = i)
      }
    }
    axis(1, at=seq(0,100,by=10))
    legend("bottomright", legend = classifiers, col=1:nclassif,pch=19) # optional legend
    }
    }
    else #plot boxplot for the selected treesize BTree[j]
    {
      jtr=ntree #индекс вектора BTree размерностей ансамбля
      matr=as.matrix(data)
      dim(matr)=c(nrow(data),ntree,nclassif)
      matr=matr[,jtr,]
      colnames(matr)=classifiers
      box=boxplot(matr,main = "Cross validation",ylab = "Classification accuracy",xlab="Classifiers",col=3)
    }
  }
  if((ngene>1)&&(ntree==1))
  {
    if(boxflag)
    {
      if(nclassif==1)
      {
        vrem=matrix(1:ncol(data),ncol(data),1)
      }
      else
      {
        vrem=sapply(classifiers,function(z) which(colnames(data)==z))
      }
      
      par(mfrow=c(length(classifiers),1))
      for(i in 1:nclassif)
      {
        if(ngene==1)
        {
          values=data[,vrem[i],drop=FALSE]
        }
        else
        {
          values=data[,vrem[,i],drop=FALSE]
        }
        colnames(values)=gene
        box=boxplot(values,main = paste("Cross validation",classifiers[i]),ylab = "Classification accuracy",xlab="n of features",col=i+1)
      }
    }
    else
    {
    accur=sapply(1:(ngene*nclassif),function(z) median(data[,z]))
    dim(accur)=c(nclassif,ngene)
    plot(0,0,xlim = c(0,100),ylim = c(0,1.1),type = "n",xlab="Feature size",ylab="Accuracy",xaxt="n")
    for (i in 1:nclassif){
      lines(gene,accur[i,],col = i,type = 'b',lwd=2)
    }
    axis(1, at=seq(0,100,by=10))
    legend("bottomright", legend = classifiers, col=1:nclassif,pch=19) # optional legend
    }
  }
  if((ngene==1)&&(ntree==1))
  {
    box=boxplot(data,main = "Cross validation",ylab = "Classification accuracy",xlab="Classifiers",col=3)
  }

}