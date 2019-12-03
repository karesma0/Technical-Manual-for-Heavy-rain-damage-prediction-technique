
FN<-list.files('1.input/Rain/')

i=1
while (i<=length(FN)) {
  DB<-read.csv(paste0('1.input/Rain/', FN[i]))
  DB$Date<-as.Date(DB$Date)
  DB<-DB[DB$Date>=as.Date('2017-01-01'), ]
  write.csv(DB, paste0('Actual Application/Application_Data/', FN[i]),row.names = F)
i=i+1}
