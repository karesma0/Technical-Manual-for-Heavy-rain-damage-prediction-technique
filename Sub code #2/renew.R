
renew<-function(subdata,model){
  renew_DB<-subdata[as.Date(subdata[,2]) <  as.Date(paste0(as.integer(year[ny])-1,'-12-31')),]
  
  renew_D_DB<-renew_DB[,3:9]
  
  preProc_D <- preProcess(
    renew_D_DB,
    # centering/scaling/zero-var/near zero-var/
    method = c("center", "scale", "zv", "nzv", "pca"),  
    # thresh = 0.7    # proportion of variation
    pcaComp = 1       # number of principle component
  )
  {if(preProc_D$rotation[1]<0){preProc_D$rotation<-preProc_D$rotation*-1}
    else{preProc_D$rotation<-preProc_D$rotation}}
  
  renew_X_DB<-renew_DB[,11:34]
  
  preProc_X <- preProcess(
    renew_X_DB,
    # centering/scaling/zero-var/near zero-var/
    method = c("center", "scale", "zv", "nzv", "pca"),  
    # thresh = 0.7    # proportion of variation
    pcaComp = 1       # number of principle component
  )
  {if(preProc_X$rotation[1]<0){preProc_X$rotation<-preProc_X$rotation*-1}
    else{preProc_X$rotation<-preProc_X$rotation}}
  
  return(list(
    PCA_D=preProc_D,
    PCA_X=preProc_X))
  
}
