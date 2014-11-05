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
###############Get export location ###############
ex.dir<-function(filename){
  dr<-getwd()
  dr<-gsub("R-project", "R.export", dr)
  dr<-file.path(dr, filename, fsep = .Platform$file.sep)
  
  if(.Platform$OS.type=="windows"){
    dr.out<-gsub("/", "\\", dr, fixed = TRUE)
  }else{
    dr.out<-dr
  }
  dr.out
}

###############Export richness, sd to data ##############
exportspecaccum <- function(data, filename)
{
  if (class(data)=="specaccum"){
    x<-rbind.data.frame(data$richness, data$sd)
    as.data.frame(x)
    colnames(x)<-c(data$sites)
    rownames(x)<-c("Richness","Sd")
    file.dir<-ex.dir(filename)
    write.csv(x, file = file.dir)
  }else{
    class<-class(data)
    print(class)}
}

########### Connect R to BioLabs' database ##################
datafile<-function(){
  dr<-getwd()
  dr<-gsub("R-project", "Image", dr)
  tmp<-paste(dr, "/VnRotifera.db.sqlite", sep="")
  datafile<-tmp
  datafile
}

########## Show number of species GROUP BY Family or GENUS#####################
high.taxon<-function(location = NULL, time = NULL){
  if (length(time)==0){
    specieslist<-species.list(location)
  }else{specieslist<-species.list(location, time)}
  
  ############ data for family vector
  family<-table(specieslist$familyname)
  family<-as.data.frame(family)
  colnames(family)<-c("familyname", "spnumber")
  family<-as.data.frame(family[order(-family$spnumber),], row.names=1:nrow(family))
  
  ############ data for genus vector
  genus<-table(specieslist$Genus)
  genus<-as.data.frame(genus)
  colnames(genus)<-c("genusname", "spnumber")
  genus<-as.data.frame(genus[order(-genus$spnumber),], row.name=1:nrow(genus))
  
  out<-list(spfamily=family, spgenus=genus)
  out
}

######### show species name from species ID ######################
spname<-function(spid){ 
  drv<-dbDriver("SQLite")
  con<-dbConnect(drv, datafile())
  tmp<-spid
  sql<-paste("SELECT spname FROM species WHERE speciesID = ","'", tmp, "'", sep="")
  spname<-dbGetQuery(con, sql)
  dbDisconnect(con)
  if(!length(spname[,1])==0){
    spname<-cbind(spid, spname)
  }else{
    spname<-cbind(spid, "no spid")
  }
  spname
}

######### show species names from species IDs ###################################
spnames<-function(spids){
  spids<-gsub(" ", "", spids)
  spids<-strsplit(spids, ",")
  spids<-spids[[1]]
  species.name<-NULL
  null.sp<-"SpID not found in list:"
  for(i in 1:length(spids)){
    if(!spname(spids[i])[1,2]=="no spid"){
      species.name<-rbind(species.name, spname(spids[i])[1,]) 
    }else{
      null.sp<-paste(null.sp, spids[i], sep=",")
    }
    #print()
    flush.console()
  }
  species.names<-list(spnames=species.name, notfound=null.sp)
  species.names
}

###### show species list by location ###############################
species.list<-function(location = "", time=NULL){ ######can be one or multi location###
  drv<-dbDriver("SQLite")
  con<-dbConnect(drv, datafile())
if(length(time)==0){
  tmp<-gsub(" ", "", location)
  tmp<-strsplit(tmp, ",")[[1]]

  loc.col<-NULL
  loc.where<-paste(" locationidi = ", "'", tmp[1], "'",sep="")
  
  ############ where location column   
  for(i in 1:length(tmp)){
      loc.col<-paste(loc.col, ", SUM(locationidi = ", "'", tmp[i], "'", ") AS ", "'", tmp[i], "'",sep="")
      flush.console()
    }
  
  ############# where location
  for(i in 2:length(tmp)){
      loc.where<-paste(loc.where, " OR locationidi = ", "'", tmp[i], "'",sep="")
      flush.console()
    }
  loc.where<-paste("(", loc.where, ")", sep="")
  
  ###########
  sql <- paste("SELECT  familyname, specieid, genus, species, spname, author", loc.col,
               "FROM fullspecimen
        WHERE countsp = 'FALSE' AND ", loc.where,
        "Group by spname
        ORDER BY familyname, spname", sep="")
  
    
}else{
  tmp<-gsub(" ", "", location)
  tmp<-strsplit(tmp, ",")[[1]]
  
  loc.col<-NULL
  loc.where<-paste(" locationidi = ", "'", tmp[1], "'",sep="")
  
  
  ############ where location column   
  for(i in 1:length(tmp)){
    loc.col<-paste(loc.col, ", SUM(locationidi = ", "'", tmp[i], "'", ") AS ", "'", tmp[i], "'",sep="")
    flush.console()
  }
  
  ############# where location
  for(i in 2:length(tmp)){
    loc.where<-paste(loc.where, " OR locationidi = ", "'", tmp[i], "'",sep="")
    flush.console()
  }
  loc.where<-paste("(", loc.where, ")", sep="")
  
  #############Time ##########
  temp<-gsub(" ", "", time)
  temp<-strsplit(temp, ",")[[1]]
  time.where<-paste(" strftime('%Y', Date) = ", "'", temp[1], "'",sep="")
  
  for(i in 2:length(tmp)){
    time.where<-paste(time.where, " OR strftime('%Y', Date) = ", "'", temp[i], "'",sep="")
    flush.console()
  }
  time.where<-paste("(", time.where, ")", sep="")
  
  ########### sql
  sql <- paste("SELECT  familyname, specieid, genus, species, spname, author", loc.col,
               "FROM fullspecimen
        WHERE countsp = 'FALSE' AND ", loc.where, "AND", time.where,
               "Group by spname
        ORDER BY familyname, spname", sep="")
  
}
  
  species.list<-dbGetQuery(con, sql)
  
  dbDisconnect(con)  

  species.list
}

############## Species ID ###################
spid.list<-function(location = NULL, time=NULL){
  drv<-dbDriver("SQLite")
  con<-dbConnect(drv, datafile())
  if (!length(time)==0){
    
    ########## location ID ##########
    tmp<-gsub(" ", "", location)
    tmp<-strsplit(tmp, ",")[[1]]
    loc.where<-paste(" locationidi = ", "'", tmp[1], "'",sep="")
    for(i in 2:length(tmp)){
      loc.where<-paste(loc.where, " OR locationidi = ", "'", tmp[i], "'",sep="")
      flush.console()
    }
    loc.where<-paste("(", loc.where, ")", sep="")
    
    #############Time ##########
    temp<-gsub(" ", "", time)
    temp<-strsplit(temp, ",")[[1]]
    time.where<-paste(" strftime('%Y', Date) = ", "'", temp[1], "'",sep="")
    for(i in 2:length(tmp)){
      time.where<-paste(time.where, " OR strftime('%Y', Date) = ", "'", temp[i], "'",sep="")
      flush.console()
    }
    time.where<-paste("(", time.where, ")", sep="")
    
    sql<-paste("SELECT  specieid FROM fullspecimen WHERE countsp = 'FALSE' AND ", loc.where, "AND", time.where, "GROUP BY specieid 
        ORDER BY specieid", sep="")
    
  }else{
    tmp<-gsub(" ", "", location)
    tmp<-strsplit(tmp, ",")[[1]]
    loc.where<-paste(" locationidi = ", "'", tmp[1], "'",sep="")
    for(i in 2:length(tmp)){
      loc.where<-paste(loc.where, " OR locationidi = ", "'", tmp[i], "'",sep="")
      flush.console()
    }
    loc.where<-paste("(", loc.where, ")", sep="")
    
    sql<-paste("SELECT  specieid FROM fullspecimen WHERE countsp = 'FALSE' AND ", loc.where, "GROUP BY specieid 
        ORDER BY specieid", sep="")
    
    
  }

  
  spid.list<-dbGetQuery(con, sql)
  
  dbDisconnect(con)  
  
  spid.list
  
}

############## Species distribution ##################
sp.dis<-function(location, time=NULL){
  drv<-dbDriver("SQLite")
  con<-dbConnect(drv, datafile())
  if(length(time)==0){
    if(!location==""){
      tmp<-gsub(" ", "", location)
      tmp<-strsplit(tmp, ",")[[1]]
      spid.tem<-spid.list(location)
      string.spid.tmp<-NULL
      loc.where<-paste(" locationidi = ", "'", tmp[1], "'",sep="")
      for(i in 1:nrow(spid.tem)){
        string.spid.tmp<-paste(string.spid.tmp, ", COUNT(CASE WHEN Specieid = ", "'", spid.tem[i,], "'", " THEN Specieid END) AS ", "'", spid.tem[i,], "'",sep="")
        flush.console()
      }
      
      for(i in 2:length(tmp)){
        loc.where<-paste(loc.where, " OR locationidi = ", "'", tmp[i], "'",sep="")
        flush.console()
      }
      
      sql<-paste("SELECT  localityID", string.spid.tmp, "FROM fullspecimen WHERE", loc.where, "GROUP BY localityID 
        ORDER BY localityID", sep="")
      
      sp.dis<-dbGetQuery(con, sql)
      
      dbDisconnect(con)  
      
      sp.dis
      
    }
  }
  else{
    if(!location==""){
      tmp<-gsub(" ", "", location)
      tmp<-strsplit(tmp, ",")[[1]]
      spid.tem<-spid.list(location, time)
      string.spid.tmp<-NULL
      loc.where<-paste(" locationidi = ", "'", tmp[1], "'",sep="")
      for(i in 1:nrow(spid.tem)){
        string.spid.tmp<-paste(string.spid.tmp, ", COUNT(CASE WHEN Specieid = ", "'", spid.tem[i,], "'", " THEN Specieid END) AS ", "'", spid.tem[i,], "'",sep="")
        flush.console()
      }
      
      for(i in 2:length(tmp)){
        loc.where<-paste(loc.where, " OR locationidi = ", "'", tmp[i], "'",sep="")
        flush.console()
      }
      loc.where<-paste("(", loc.where, ")", sep="")
      
      #############Time ##########
      temp<-gsub(" ", "", time)
      temp<-strsplit(temp, ",")[[1]]
      time.where<-paste(" strftime('%Y', Date) = ", "'", temp[1], "'",sep="")
      for(i in 2:length(tmp)){
        time.where<-paste(time.where, " OR strftime('%Y', Date) = ", "'", temp[i], "'",sep="")
        flush.console()
      }
      time.where<-paste("(", time.where, ")", sep="")
      
      
      sql<-paste("SELECT  localityID", string.spid.tmp, "FROM fullspecimen WHERE", loc.where, "AND", time.where, "GROUP BY localityID 
        ORDER BY localityID", sep="")
      
      sp.dis<-dbGetQuery(con, sql)
      
      dbDisconnect(con)  
      
      sp.dis
      
    }
  }
  
  
  
}

#########separate date and month and year ###############


######### Rare species names from data.frame sp.dis ###########
rare.sp<-function(data, rares){
  if(rares=="single"){
    ncol<-ncol(data)
    rare.sp<-NULL
    for (i in 1:ncol){
      if(is.integer(data[,i])==TRUE){
        if(sum(data[,i])==1){
          rare.sp<-paste(rare.sp, names(data)[i], sep=", ")
        } 
      }
      flush.console()
    }
    rare.sp.com<-spnames(rare.sp)
    rare.sp.com
  }else{
    if(rares=="double"){
      ncol<-ncol(data)
      rare.sp<-NULL
      for (i in 1:ncol){
        if(is.integer(data[,i])==TRUE){
          if(sum(data[,i])==2){
            rare.sp<-paste(rare.sp, names(data)[i], sep=", ")
          }
        }
        flush.console()
      }
      rare.sp.com<-spnames(rare.sp)
      rare.sp.com
    }
    
  }

}


##################convert date time file in ###########
update.date<-function(){
  drv<-dbDriver("SQLite")
  con<-dbConnect(drv, datafile())
  sql<-"SELECT localityID FROM Locality"
  locality.list<-dbGetQuery(con, sql)
  
  for(i in 1:nrow(locality.list)){
    sql.time<-paste("SELECT Date FROM Locality WHERE localityID = ", "'", locality.list[i,], "'", sep="")
    datelo<-dbGetQuery(con, sql.time)
    dateloc<-as.Date(datelo[,1], "%m/%d/%Y")
    sql1<-paste("UPDATE Locality SET Date = ", "'", dateloc, "'", " WHERE localityID = ", "'", locality.list[i,], "'", sep="")
    date.lo<-dbGetQuery(con, sql1)
    dbDisconnect(con)
  }
  
}

#######rare species with distribution#########
rare.sp.dis<-function(data, rares){ ### data from sp.dis function
  rare.data<-rare.sp(data, rares)$spnames
  rare.data<-cbind(rare.data, locality=c(1:length(rare.data$SpName)))
  for(i in 1:length(rare.data$SpName)){
    spid.tem<-as.character(rare.data$spid[i])
    col.id<-grep(spid.tem, colnames(data))
    v<-paste(data$LocalityID[data[,col.id]==1], sep=",")
    
    if(length(v)>1){
      loc.v<-v[1]
      for(i in 2:length(v)){
        loc.v<-paste(loc.v, v[i], sep=",")
      }
    }else{loc.v<-v}
    
    rare.data$locality[rare.data$spid==spid.tem]<-loc.v
  }
  rare.data
}

#######species distribution - data from sp.dis function ############
locality.sp<-function(spid, data){
  col.id<-grep(spid, colnames(data))
  v<-paste(data$LocalityID[data[,col.id]==1], sep=",")
  if(length(v)>1){
    loc.v<-v[1]
    for(i in 2:length(v)){
      loc.v<-paste(loc.v, v[i], sep=",")
    }
  }else{loc.v<-v}
  name<-spname(spid)[1,2]
  locality.sp<-paste(spid, name, loc.v, sep=",")
  locality.sp
}


#######bio.taxon # data sheet prepare for tree analysis #############
bio.taxon<-function(location, time = NULL){
  drv<-dbDriver("SQLite")
  con<-dbConnect(drv, datafile())
  
  if (!length(time)==0){
    
    ########## location ID ##########
    tmp<-gsub(" ", "", location)
    tmp<-strsplit(tmp, ",")[[1]]
    loc.where<-paste(" locationidi = ", "'", tmp[1], "'",sep="")
    for(i in 2:length(tmp)){
      loc.where<-paste(loc.where, " OR locationidi = ", "'", tmp[i], "'",sep="")
      flush.console()
    }
    loc.where<-paste("(", loc.where, ")", sep="")
    
    #############Time ##########
    temp<-gsub(" ", "", time)
    temp<-strsplit(temp, ",")[[1]]
    time.where<-paste(" strftime('%Y', Date) = ", "'", temp[1], "'",sep="")
    for(i in 2:length(tmp)){
      time.where<-paste(time.where, " OR strftime('%Y', Date) = ", "'", temp[i], "'",sep="")
      flush.console()
    }
    time.where<-paste("(", time.where, ")", sep="")
    
    sql<-paste("SELECT  specieid, genus, familyname FROM fullspecimen WHERE countsp = 'FALSE' AND ", loc.where, "AND", time.where, "GROUP BY specieid 
        ORDER BY specieid", sep="")
    
  }else{
    tmp<-gsub(" ", "", location)
    tmp<-strsplit(tmp, ",")[[1]]
    loc.where<-paste(" locationidi = ", "'", tmp[1], "'",sep="")
    for(i in 2:length(tmp)){
      loc.where<-paste(loc.where, " OR locationidi = ", "'", tmp[i], "'",sep="")
      flush.console()
    }
    loc.where<-paste("(", loc.where, ")", sep="")
    
    sql<-paste("SELECT  specieid, genus, familyname FROM fullspecimen WHERE countsp = 'FALSE' AND ", loc.where, "GROUP BY specieid 
        ORDER BY specieid", sep="")
    
    
  }
  
  
  bio.taxon<-dbGetQuery(con, sql)
  
  dbDisconnect(con)  
  
  bio.taxon
  
}

######### Tree diverstiy ########
tree.diver<-function(location, time = NULL){
  data<-sp.dis(location, time)
  data1<-data[,-1]
  row.names(data1)<-data[,1]
  data.taxon<-bio.taxon(location, time)
  d <- taxa2dist(data.taxon, varstep=TRUE)
  cl<-hclust(d, "aver")
  dtree<-treedist(data1, cl)
  dtree
}

############Species pool (update Chao 2)##################
specpool2 <- function (x, pool) 
{
  x <- as.matrix(x)
  if (missing(pool)) 
    pool <- rep("All", nrow(x))
  if (length(pool) != NROW(x)) 
    stop("length of 'pool' and number rows in 'x' do not match")
  if (any(nas <- is.na(pool))) {
    pool <- pool[!nas]
    x <- x[!nas, , drop = FALSE]
  }
  out <- seq(1:nrow(x))
  groups <- table(pool)
  inds <- names(groups)
  S <- var.chao <- chao <- var.chao2 <- chao.2 <- var.jack1 <- jack.1 <- jack.2 <- var.boot <- bootS <- rep(NA, 
                                                                                                            length(inds))
  names(S) <- names(var.chao) <- names(chao) <- names(var.chao2) <- names(chao.2) <- names(var.jack1) <- names(jack.1) <- names(jack.2) <- names(var.boot) <- names(bootS) <- inds
  for (is in inds) {
    a1 <- a2 <- NA
    gr <- out[pool == is]
    n <- length(gr)
    if (n <= 0) 
      next
    X <- x[gr, , drop = FALSE]
    freq <- colSums(X > 0)
    p <- freq[freq > 0]/n
    S[is] <- sum(freq > 0)
    if (S[is] == 0) 
      next
    if (n >= 1) 
      a1 <- sum(freq == 1)
    if (n >= 2) 
      a2 <- sum(freq == 2)
    else 0
    chao[is] <- S[is] + if (!is.na(a2) && a2 > 0)
      (a1*a1)/2/a2
    chao.2[is] <- S[is] + if (!is.na(a2) && a2 > 0) 
      ((n-1)/n)*((a1*a1-a1)/2/(a2+1))
    else 0
    jack.1[is] <- S[is] + a1 * (n - 1)/n
    jack.2[is] <- S[is] + a1 * (2 * n - 3)/n - a2 * (n - 
                                                       2)^2/n/(n - 1)
    bootS[is] <- S[is] + sum((1 - p)^n)
    aa <- if (!is.na(a2) && a2 > 0) 
      a1/a2
    else 0
    var.chao[is] <- a2 * (0.5 + (1 + aa/4) * aa) * aa * 
      aa
    var.chao2[is] <- ((n-1)/n)*((a1*a1-a1)/2/(a2+1))+((n-1)/n)^2*((a1*(2*a1-1)^2)/4/(a2+1)^2) + ((n-1)/n)^2*((a1*a1*a2*(a1-1)^2)/4/(a2+1)^4)
    if (!is.na(a1) && a1 > 0) {
      jf <- table(rowSums(X[, freq == 1, drop = FALSE] > 
                            0))
      var.jack1[is] <- (sum(as.numeric(names(jf))^2 * 
                              jf) - a1/n) * (n - 1)/n
    }
    pn <- (1 - p)^n
    X <- X[, freq > 0, drop = FALSE]
    Zp <- (crossprod(X == 0)/n)^n - outer(pn, pn, "*")
    var.boot[is] <- sum(pn * (1 - pn)) + 2 * sum(Zp[lower.tri(Zp)])
  }
  out <- list(Species = S, chao = chao, chao.se = sqrt(var.chao), chao2=chao.2, chao2.se = sqrt(var.chao2),
              jack1 = jack.1, jack1.se = sqrt(var.jack1), jack2 = jack.2, 
              boot = bootS, boot.se = sqrt(var.boot), n = as.vector(groups))
  out <- as.data.frame(out)
  attr(out, "pool") <- pool
  out
}


#
###############Slop of Accumulation ##################
slop.accum<-function(acumuData){
  if (class(acumuData)=="specaccum"){
    x<-rbind.data.frame(acumuData$sites, acumuData$richness, acumuData$sd)
    as.data.frame(x)
    colnames(x)<-c(acumuData$sites)
    x
    slop.out<-NULL
    nr<-length(x)-1
    for(i in 1:nr){
      n<-i+1
      element<-(x[2,n]-x[2,i])/(x[1,n]-x[1,i])
      slop.out<-cbind(slop.out, element)
      flush.console()
    }
    slop.out[1,]
  }else{
    class<-class(acumuData)
    print(class)}
}

############## run slop specaccum ###############
slope.specaccum<-function(dataspeca, nsp, rep){ #####dataspeca = sp.dis("location", "time")
  dataspeca<-dataspeca[,-1]
  #ns<-sample(nrow(dataspeca), nsp)
  slop<-NULL
  for(i in 1:rep){
    ns<-sample(nrow(dataspeca), nsp)
    slop<-rbind(slop, slop.accum(specaccum(dataspeca[ns,])))
    flush.console()
  }
  slop
}

########### run plot slop specaccum #############
plot.slop.run<-function(datasp, nsp, rep){
  slop.sp<-slope.specaccum(datasp, nsp, rep)
  max<-max(slop.sp)+1
  plot(slop.sp[1,], type="l", ylim=c(0, max), xlim=c(0, nsp), xlab="sites", ylab="slope", panel.first=grid())
  for(i in 2:rep){
    lines(slop.sp[i,])
    flush.console()
  }
  
}

############## run specaccum ###############
run.specaccum<-function(dataspeca, nsp, rep){ #####dataspeca = sp.dis("location", "time")
  dataspeca<-dataspeca[,-1]
  #ns<-sample(nrow(dataspeca), nsp)
  run.spe<-NULL
  for(i in 1:rep){
    ns<-sample(nrow(dataspeca), nsp)
    run.spe<-rbind(run.spe, specaccum(dataspeca[ns,])$richness)
    flush.console()
  }
  run.spe
}


########### run plot specaccum #############
plot.specaccum.run<-function(datasp, nsp, rep){  #####dataspeca = sp.dis("location", "time")
  specaccum.sp<-run.specaccum(datasp, nsp, rep)
  max<-max(specaccum.sp)+1
  plot(specaccum.sp[1,], type="l", ylim=c(0, max), xlim=c(0, nsp), xlab="sites", ylab="slope", panel.first=grid())
  for(i in 2:rep){
    lines(specaccum.sp[i,])
    flush.console()
  }
  
}

######### Search Advance ############
biosearch<-function(type = NULL, x = NULL){ #### type: data type which you want to search; x: variable
  drv<-dbDriver("SQLite")
  con<-dbConnect(drv, datafile())
  x<- paste(toupper(substring(x,1,1)), substring(x,2), sep="")
  
  if(type=="family"){
    sql<-paste("SELECT spname, specieID FROM fullspecimen WHERE familyname = ", "'", x, "'", " GROUP BY spname", sep="")
  }
  
  if(type=="genus"){
    sql<-paste("SELECT spname, specieID FROM fullspecimen WHERE genus = ", "'", x, "'", " GROUP BY spname", sep="")
  }
  
  if(type=="location"){
    sql<-paste("SELECT spname, specieID FROM fullspecimen WHERE genus = ", "'", x, "'", " GROUP BY spname", sep="")
  }
  rs.biosearch<-dbGetQuery(con, sql)
  dbDisconnect(con)
  
  rs.biosearch
}


####### Table list #################
table.list<-function() {
  drv<-dbDriver("SQLite")
  con<-dbConnect(drv, datafile())
  table.list<-dbListTables(con)
  dbDisconnect(con)
  table.list
}
##### Field List ################
field.list<-function(tablename){
  drv<-dbDriver("SQLite")
  con<-dbConnect(drv, datafile())
  field.list<-dbListFields(con, tablename)
  dbDisconnect(con)
  field.list
}
###### Data Maps ######
data.map<-function(){
  drv<-dbDriver("SQLite")
  con<-dbConnect(drv, datafile())
  data.map<-data.frame()
  #colnames(data.map)<-c("table", "field")
  
  for(i in 1:length(table.list())){
    tablei<-table.list()[i]
    fieldi<-field.list(tablei)
    data.map[i,1]<-tablei
    
    for(j in 1:length(fieldi)){
      data.map[i,j+1]<-fieldi[j]
    }
  }
  
  dbDisconnect(con)
  data.map
}

####### Count data by Single samples ##############
count.locality<-function(localityid){
  drv<-dbDriver("SQLite")
  con<-dbConnect(drv, datafile())
  sql<-paste("SELECT specieid, count50, count100, count150, count200, count250, count300, count400, count500, count700, count1000 FROM fullspecimen WHERE localityid = '", localityid, "'", sep="")
  count.locality<-dbGetQuery(con, sql)
  dbDisconnect(con)
  count.locality
}

######## Count data by Single samples with accumulate by level (count100 = count50+count100)#################
count.locality.acc<-function(localityid){
  drv<-dbDriver("SQLite")
  con<-dbConnect(drv, datafile())
  sql<-paste("SELECT specieid, count50, sum(count100, count50) AS Count100, count150, count200, count250, count300, count400, count500, count700, count1000 FROM fullspecimen WHERE localityid = '", localityid, "'", sep="")
  count.locality.acc<-dbGetQuery(con, sql)
  dbDisconnect(con)
  count.locality.acc
}
