PI.Database

i=1
Total<-NULL
while (i<=11) {
  DPBa<-PI.Database[[i]]
  
  if(i==1){Total<-DPBa}
  else{Total<-rbind(Total,DPBa)}
i=i+1}

write.csv(Total,'Actual Application/1111/Total.csv',row.names = F)
Name2<-unique(Total[,1])

i=1
while (i<=225) {
  DB<-Total[Total[,1]==Name2[i], ]
  DB<-DB[,-35]
  write.csv(DB,paste0('Actual Application/1111/',Name2[i],'.csv'),row.names = F)
i=i+1}
