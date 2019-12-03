Datmix<-function(Damage, db){


  DB<-Damage[Damage[,1]==Name,]
  DB[,6]<-1
  Rain<-db
  Rain$Date<-as.Date(Rain$Date)
  k=1
  
  while (k<=length(DB[,1])) {
    date<-seq(DB[k,4],DB[k,5],by= 'day')
    
    date<-as.data.frame(date)
    colnames(date)<-'Date'
    date$damage<-1
    
    
    if(k==1){Date<-date}
    else{Date<-rbind(Date,date)}
    j=j+1}
  DATA<-merge(Rain,Date,by='Date',all.x = TRUE)
  DATA[is.na(DATA$damage), 35 ]<-0
  
return(DATA)

}
