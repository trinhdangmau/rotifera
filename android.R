##########startup package #################
biolabs<-function(){
  library(vegan)
  library(RSQLite)
}
#### Download data set ###############
download.data<-function(){
  mainfolder<-paste(getwd(), "/biolabs", sep="")
  dir.create(mainfolder)
  setwd(mainfolder)
  
  source.dir<-paste(getwd(), "/R-project", sep="")
  data.dir<-paste(getwd(), "/Image", sep="")
  backup<-paste(getwd(), "/databaseBackup", sep="")
  export<-paste(getwd(), "/R.export", sep="")
  dir.create(backup)
  dir.create(data.dir)
  dir.create(source.dir)
  
  datafile<-paste(data.dir, "/VnRotifera.db.sqlite", sep="")
  sourcefile<-paste(source.dir, "/biolab.r", sep="")
  
  download.file("http://anthaiphusteel.com/dmautrinh/VnRotifera.db.sqlite", destfile=datafile, method = "curl")
  download.file("https://raw.githubusercontent.com/trinhdangmau/rotifera/master/biolabs.r", destfile=sourcefile, method="curl")
  
  setwd(source.dir)
  source(sourcefile)
}
