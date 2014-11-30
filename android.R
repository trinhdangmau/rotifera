##########startup package #################
biolabs<-function(){
  mainDir<-"/sdcard/biolabs/R-project/"
  subDir<-"biolabs.r"
  if(file.exists(file.path(mainDir, subDir))==TRUE){
  setwd(file.path(mainDir))
  source(subDir)
  library(vegan)
  library(RSQLite)
  }else{
    if(file.exists("/sdcard/biolabs/")==TRUE){
     file.remove("/sdcard/biolabs/")
     setwd("/sdcard/")
     download.data()
    }else{
     setwd("/sdcard/")
     download.data()
     }
  }

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
  
  dir.create(export)
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
