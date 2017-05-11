DataSetsResult<-function(classifiers,simpleclass,BTree,gene,type=3)
{
  #type тип рисунка
  library(mlbench)
  library(RColorBrewer)
  
 # datasets=c("BreastCancer","Pima","Sonar","Vehicle")
  datasets=c("balance50ans1sure","bupa50ans1sure","newthyroid50ans1sure","phoneme50ans1sure","tae50ans1sure","wine50ans1sure")
  
  #test only when lengh(gene)=1
  if(length(gene)>1) return(-1)
  
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
  
  
  
  if(type==1)
  {
  #выполним анализ по всем данным
  #определение наилучшего ансамбля для каждого набора с учетом размера ансамбля.
  win=matrix(0,numdata,ntree)
  
  #исключаем классификатор - единичное дерево
  if(!is.null(simpleclass))
  {
    nclassif=nclassif-1
    classifiers=classifiers[-1]
  }
  
  for(i in 1:numdata)
  {
    res=mean[i,]
    if(!is.null(simpleclass))
    {
      res=res[-1]
    }
    dim(res)=c(ntree,nclassif)
    for(j in 1:ntree)
    {
      win[i,j]=which.max(res[j,])
    }
  }
  
  out=matrix(0,nclassif,ntree)
  rownames(out)=classifiers
   for(i in 1:ntree)
   {
     vrem=classifiers[win[,i]]
     tt=table(vrem)
     tt=tt/numdata
     out[names(tt),i]=tt
   }
  #рисование относительного выигрыша каждого классификатора
  # n <- length(y)
  # x <- 1:n
  # s = smooth.spline(x, y, spar=0.5)
  # xy <- predict(s, seq(min(x), max(x), by=1)) # Some vertices on the curve
  # m <- length(xy$x)                         
  # x.poly <- c(xy$x, xy$x[m], xy$x[1])         # Adjoin two x-coordinates
  # y.poly <- c(xy$y, 0, 0)                     # .. and the corresponding y-coordinates
  # plot(range(x), c(0, max(y)), type='n', xlab="X", ylab="Y")
  # polygon(x.poly, y.poly, col=gray(0.95), border=NA)          # Show the polygon fill only
  # lines(s)
  #cols<-brewer.pal(n=nclassif,name="Set1")
  cols=topo.colors(nclassif)
  for(i in 1:nclassif)
  {
    if(i==1)
    {
      plot(BTree,out[i,],ylim = c(0,1.1),type = "b",xlab="Ensemble size",ylab="Accuracy",col = cols[i],lwd=2)#,xaxt="n")
      x.poly <- c(BTree, BTree[ntree], BTree[1])
      y.poly <- c(out[i,], 0, 0)
      polygon(x.poly, y.poly, col=cols[i], border=NA)
      str1=out[i,]
    }
    else
    {
      str2=colSums(out[1:i,])
      lines(BTree,str2,col = cols[i],type = 'b',lwd=2)
      x.poly <- c(BTree, rev(BTree))
      y.poly <- c(str2,rev(str1))
      polygon(x.poly, y.poly, col=cols[i], border=NA)
      str1=str2
    }
  }
  legend("bottomright",legend = classifiers, col=1:nclassif,pch=19) # optional legend 
  }
  
  if(type==2)
  {
  #рисунок с диагональю относительной важности классификатора x-ось сравниваемый классификатор
  compare="RotateCl"
  #compare="B"
  #определенный размер дерева например BTree[j]
  jtr=1
  x=NULL
  y=NULL
  
  for(i in 1:numdata)
  {
    res=mean[i,]
    res1=res

    if(!is.null(simpleclass))
    {
      res1=res[-1]
    }
    dim(res1)=c(ntree,ncl)
    sel=res1[jtr,]
    
    if(!is.null(simpleclass))
    {
      sel=c(res[1],sel)
    }
    names(sel)=classifiers
    
    
    index1=grep(compare,classifiers)
    y=c(y,sel[index1])
    
    sel[index1]=-1
    index2=which.max(sel)
    x=c(x,sel[index2])
  }
  plot(c(0.7,1.0),c(0.7,1.0),xlim=c(0.7,1.0),ylim = c(0.7,1.0),type = "l",xlab="Best other",ylab=compare,col = 1,lwd=2)#,xaxt="n")
  points(x,y,col = 2,pch=19,cex=1.2)
  }
  
  if(type==3)
  {
    #расчет количества попарных преимуществ
    #определенный размер дерева например BTree[j]
    jtr=2
    comp=matrix(0,nclassif,nclassif)
    sig=matrix(0,nclassif,nclassif)
    for(i in 1:numdata)
    {
      dat=massiv[[i]]
      dat1=dat
      res=mean[i,]
      res1=res
      if(!is.null(simpleclass))
      {
        res1=res[-1]
        dat1=dat[,-1]
      }
      dim(res1)=c(ntree,ncl)
      dat1=as.matrix(dat1)
      dim(dat1)=c(nrow,ntree,ncl)
      
      seldat=dat1[,jtr,]
      sel=res1[jtr,]
      if(!is.null(simpleclass))
      {
        sel=c(res[1],sel)
        seldat=cbind(dat[,1],seldat)
      }
      
      names(sel)=classifiers
      colnames(seldat)=classifiers
      
      for(j in 1:(nclassif-1))
      {
        for(l in (j+1):nclassif)
        {
          vrem<-if(sel[j]<=sel[l]) 1 else 0
          comp[j,l]=comp[j,l]+vrem
          if(sel[j]==sel[l])
          {
            comp[l,j]=comp[l,j]+vrem
          }
          else
          {
          comp[l,j]=comp[l,j]+1-vrem
          }
          
          ttest=wilcox.test(seldat[,j], seldat[,l], paired = TRUE)
          if(ttest$p.value<0.05)
          {
            sig[j,l]=comp[j,l]+vrem
            sig[l,j]=comp[l,j]+1-vrem
          }
        }
      }
    }
    
    #расчет рейтинга
    rate=matrix(0,nclassif,3)
    dimnames(rate)=c(list(classifiers),list(c("Rank","Wins","Losses")))
    #using comp matrix
    win=colSums(comp)
    loss=rowSums(comp)
    rate[,2]=win
    rate[,3]=loss
    rate[,1]=win-loss
    out=sort(rate[,1],decreasing = TRUE,index.return=TRUE)
    rate=rate[out$ix,]
    rate
  }

}
