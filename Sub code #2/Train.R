
Train<- function(TR.DATA){
  library(caret)
  
  
  ## 선행강우
  DPC_DB<-TR.DATA[,c(3:9)]
  
  preProc_D <- preProcess(
    DPC_DB,
    # centering/scaling/zero-var/near zero-var/
    method = c("center", "scale", "zv", "nzv", "pca"),  
    # thresh = 0.7    # proportion of variation
    pcaComp = 1       # number of principle component
  )
  {if(preProc_D$rotation[1]<0){preProc_D$rotation<-preProc_D$rotation*-1}
    else{preProc_D$rotation<-preProc_D$rotation}}
  
  # pc for training set
  Dpc_tr1 <- predict(preProc_D, newdata = DPC_DB)  
  colnames(Dpc_tr1)<-c("PC.D")
  
  ##지속시간별 최대강우
  
  XPC_DB<-TR.DATA[,c(11:34)]
  
  preProc_X <- preProcess(
    XPC_DB,
    # centering/scaling/zero-var/near zero-var/
    method = c("center", "scale", "zv", "nzv", "pca"),  
    # thresh = 0.7    # proportion of variation
    pcaComp = 1       # number of principle component
  )
  {if(preProc_X$rotation[1]<0){preProc_X$rotation<-preProc_X$rotation*-1}
    else{preProc_X$rotation<-preProc_X$rotation}}
  
  # pc for training set
  Xpc_tr1 <- predict(preProc_X, newdata = XPC_DB)  
  colnames(Xpc_tr1)<-c("PC.X")
  
  #1. D 
  
  #설명변수 계급 구간 설정하기 
  
  n_class<-round(1+(log(nrow(Dpc_tr1)/log(2))))
  # 
  
  delta_d<-(max(Dpc_tr1[,1])-min(Dpc_tr1[,1]))/n_class
  i=1
  standard<-0
  while (i<=n_class) {
    standard[i]<-min(Dpc_tr1[,1])+delta_d*(i-1)
    i=i+1}
  
  Class<-cut(Dpc_tr1[,1],breaks = standard, labels = c(paste0('class', 1:(n_class-1))),include.lowest = T)
  
  barplot(table(Class))
  
  #2. X
  
  #설명변수 계급 구간 설정하기 
  
  n_class1<-round(1+(log(nrow(Xpc_tr1)/log(2))))
  # 
  
  delta_x<-(max(Xpc_tr1[,1])-min(Xpc_tr1[,1]))/n_class
  i=1
  standard1<-0
  while (i<=n_class1) {
    standard1[i]<-min(Xpc_tr1[,1])+delta_d*(i-1)
    i=i+1}
  
  Class1<-cut(Xpc_tr1[,1],breaks = standard, labels = c(paste0('class', 1:(n_class-1))),include.lowest = T)
  
  barplot(table(Class1))
  
  
  library('dplyr')
  
  pc_tr1<-cbind(TR.DATA[,c(1:2,35)], Dpc_tr1, Xpc_tr1)
  
  
  ## 후보 기준 설정 
  
  prob.<-as.data.frame(matrix(nrow = 5, ncol = n_class))
  colnames(prob.)<-c(paste0('class', 1:n_class))
  row.names(prob.)<-c('PC.D', 'PC.X', 'satisfied day','Damage day','prob.')
  
  i=1
  while (i<=n_class) {
    
    prob.[1,i]<-standard[i]
    prob.[2,i]<-standard1[i]
    prob.[3,i]<-nrow(pc_tr1[prob.[1,i]<=pc_tr1[,4] | prob.[2,i]<=pc_tr1[,5],])
    prob.[4,i]<-nrow(pc_tr1[prob.[1,i]<=pc_tr1[,4] & pc_tr1[,3]!=0 | prob.[2,i]<=pc_tr1[,5]& pc_tr1[,3]!=0,]  )
    prob.[5,i]<-prob.[4,i]/prob.[3,i]
    i=i+1}
  
  return(list(
    prob.=prob.,
    PCA_D=preProc_D,
    PCA_X=preProc_X
  ))
}
