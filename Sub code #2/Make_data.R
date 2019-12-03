
Make_data<-function(sp.region, Damage){
  i=1
  class<-paste0('Class ',  1:11)
  Database<-split(class,class)
  n<-length(sp.region)
  while (i<=n) {
    dblist<-sp.region[[i]]
    
    nn<-length(dblist[,1])
    j=1
    while (j<=nn) {
      
      db<-read.csv(paste0('1.input/Rain/',dblist[j,3],'.csv'))
      Name<-as.character(dblist[j,3])
      
      DB<-Damage[Damage[,1]==Name,]
      if(nrow(DB)==0){Merge_DB=Merge_DB}
      else{
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
          k=k+1}
        DATA<-merge(Rain,Date,by='Date',all.x = TRUE)
        DATA[is.na(DATA$damage), 35 ]<-0
        
        merge_DB<-DATA[,c(2,1,3:35)]
        
        if(j==1){Merge_DB<-merge_DB}
        else{Merge_DB<-rbind(Merge_DB,merge_DB)}
      }
      
    j=j+1}
    
    Database[[i]]<-Merge_DB
    
    i=i+1}
  return(Database)
}
