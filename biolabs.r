##########startup package #################
biolabs<-function(){
  library("vegan", lib.loc="~/R/win-library/3.1")
  library()
  install.packages("ggplot2", lib="/data/Rpackages/")
  library(ggplot2, lib.loc="/data/Rpackages/")
}

##############Backup database ###############
backupdata<-function(){
  dr<-getwd()
  dr<-gsub("R-project", "databaseBackup/", dr)
  time<-format(Sys.time(), "%Y-%m-%d_%H-%M")
  tmp<-paste(dr, time, ".VnRotifera.db.sqlite", sep="")
  
  if(.Platform$OS.type=="windows"){
    dr.backup<-gsub("/", "\\", tmp, fixed = TRUE)
    datafile<-gsub("/", "\\", datafile(), fixed = TRUE)
  }else{
    dr.backup<-tmp
    datafile<-datafile()
  }
  rs<-file.copy(datafile, dr.backup)
  if(rs==TRUE){message(paste("Datafile was backup", dr.backup, sep=" "))}
  
  logfile<-paste(dr, "log.txt", sep="")
  write(time, file=logfile, append=TRUE)
}
