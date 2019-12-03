Test<-function(model,TE.DATA){
  prob.<-model$prob.
  
  preProc_D<-model$PCA_D
  preProc_X<-model$PCA_X
  
  
  DPC_DB<-TE.DATA[,c(3:9)]
  XPC_DB<-TE.DATA[,c(11:34)]
  
  Dpc_te <- predict(preProc_D, newdata = DPC_DB)  
  colnames(Dpc_te)<-c("PC.D")
  
  Xpc_te <- predict(preProc_X, newdata = XPC_DB)  
  colnames(Xpc_te)<-c("PC.X")
  
  pc_te1<-cbind(TE.DATA[,c(1:2,35)], Dpc_te, Xpc_te)
  
  
  pc_te1[pc_te1[,3]!=0,]
  ncol(prob.)
  abc=1
  result_matrix<-as.data.frame(matrix(nrow = ncol(prob.),ncol = 6))
  colnames(result_matrix)<-c('HTR_D', 'HTR_X', 'prb.', 'SEN', 'prec', 'f1-score')
  
  confu_list<-NULL
  HTR<-0
  while (abc<=ncol(prob.)) {
    HTR[1]=prob.[1,abc];HTR[2]=prob.[2,abc]
    
    confus_M<-as.data.frame(matrix(nrow = 2,ncol = 2))
    colnames(confus_M)<-c('0','1')
    row.names(confus_M)<-c('0','1')
    
    confus_M[1,1]<-nrow(pc_te1[pc_te1[,4]< HTR[1] & pc_te1[,3]==0 & pc_te1[,5]< HTR[2] & pc_te1[,3]==0, ])
    confus_M[1,2]<-nrow(pc_te1[pc_te1[,4]>= HTR[1] & pc_te1[,3]==0 | pc_te1[,5]>= HTR[2] & pc_te1[,3]==0, ])
    confus_M[2,1]<-nrow(pc_te1[pc_te1[,4]< HTR[1] & pc_te1[,3]!=0 & pc_te1[,5]< HTR[2] & pc_te1[,3]!=0, ])
    confus_M[2,2]<-nrow(pc_te1[pc_te1[,4]>=HTR[1] & pc_te1[,3]!=0 | pc_te1[,5]>= HTR[2] & pc_te1[,3]!=0, ])
    
    
    TM<-f1_score(confus_M)
    
    confu_list[[abc]]<-confus_M
    result_matrix[abc,1]<-HTR[1]
    result_matrix[abc,2]<-HTR[2]
    result_matrix[abc,3]<-prob.[4,abc]
    result_matrix[abc,4]<-TM[1]
    result_matrix[abc,5]<-TM[2]
    result_matrix[abc,6]<-TM[3]
    
    
    if(TM$SEN==0|TM$prec==0){break}
    abc=abc+1}
  
  return(list(
    confusion=confu_list,
    result_matrix=result_matrix))
}
