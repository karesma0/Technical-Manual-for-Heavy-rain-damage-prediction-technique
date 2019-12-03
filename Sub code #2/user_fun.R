
f1_score<-function(x){
  SEN<-x[2,2]/sum(x[2,])
  prec<-x[2,2]/sum(x[,2])
  f1_score=2*SEN*prec/(SEN+prec)
  return(list( SEN= SEN, prec=prec, f1_score=f1_score))
}


monthly<-function(Train_DB){
  Train_DB$month<-as.integer(substr(Train_DB$Date,start = 6,stop = 7))
  
  Train_DB<-Train_DB[Train_DB[,36]>=6 & Train_DB[,36]<=9, ]
  Train_DB<-Train_DB[Train_DB[,11]!=0, ]
  return(Train_DB)
}