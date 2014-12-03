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
### out put data is a vector incluse $spfamily and $spgenus ----- show number of taxa encouted in each taxa level
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
spid.list<-function(location, time=NULL){
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
  sql<-paste("SELECT specieid, count50,
             (count50 + count100) AS sum100, 
             (count50+count100+count150) AS sum150, 
             (count50+count100+count150+count200) AS sum200, 
             (count50+count100+count150+count200+count250) AS sum250, 
             (count50+count100+count150+count200+count250+count300) AS sum300,
             (count50+count100+count150+count200+count250+count300+count400) AS sum400,
             (count50+count100+count150+count200+count250+count300+count400+count500) AS sum500,
             (count50+count100+count150+count200+count250+count300+count400+count500+count700) AS sum700,
             (count50+count100+count150+count200+count250+count300+count400+count500+count700+count1000) AS sum1000
             FROM fullspecimen WHERE localityid = '", localityid, "'", sep="")
  count.locality.acc<-dbGetQuery(con, sql)
  dbDisconnect(con)
  count.locality.acc
}

########### transpose - special for count.locality data type ###############
transpose<-function(data){
  pdata<-data[,-1]
  rownames(pdata)<-data[,1]
  tdata<-t(pdata)
  ## this is the new chang from souce
  tdata
}

###### Number of species in each level of individual by Locality #####
sp.ind.level<-function(locality){
  sumlocality<-count.locality.acc(locality)
  sumlocality[is.na(sumlocality)]<-0
  number.sp<-length(sumlocality[,1])
  sp.ind.level<-as.data.frame(setNames(replicate(11,numeric(0), simplify=F),c("locality","L50","L100","L150","L200","L250", "L300", "L400", "L500", "L700", "L1000")))
  sp.ind.level[1,1]<-locality
  for(i in 2:11){
    sp.ind.level[1,i]<-number.sp - sum(sumlocality[,i]==0)
  }
  sp.ind.level
  
}

###### Locality by Locations ###############
locality.list<-function(location, time = NULL){
  drv<-dbDriver("SQLite")
  con<-dbConnect(drv, datafile())
  
  ########## location ID 
  tmp<-gsub(" ", "", location)
  tmp<-strsplit(tmp, ",")[[1]]
  loc.where<-paste(" locationidi = ", "'", tmp[1], "'",sep="")
  for(i in 2:length(tmp)){
    loc.where<-paste(loc.where, " OR locationidi = ", "'", tmp[i], "'",sep="")
    flush.console()
  }
  loc.where<-paste("(", loc.where, ")", sep="")
  
  
  if (!length(time)==0){
    #############Time 
    temp<-gsub(" ", "", time)
    temp<-strsplit(temp, ",")[[1]]
    time.where<-paste(" strftime('%Y', Date) = ", "'", temp[1], "'",sep="")
    for(i in 2:length(tmp)){
      time.where<-paste(time.where, " OR strftime('%Y', Date) = ", "'", temp[i], "'",sep="")
      flush.console()
    }
    time.where<-paste("(", time.where, ")", sep="")
    
    sql<-paste("SELECT  localityid FROM fullspecimen WHERE ", loc.where, "AND", time.where,  " Group BY localityid", sep="")
    
    
  }else{
    sql<-paste("SELECT  localityid FROM fullspecimen WHERE ", loc.where, " Group BY localityid",sep="")
    
  }
  locality.list<-dbGetQuery(con, sql)
  dbDisconnect(con)
  locality.list[,1]
}

###### Number of species in each level of individual by Location #######

sp.ind.levels<-function(location, time = NULL){
  locality.list<-locality.list(location, time)
  sp.ind.levels<-NULL
  for(i in 1:length(locality.list)){
    sp.ind.level<-sp.ind.level(locality.list[i])
    sp.ind.levels<-rbind(sp.ind.levels, sp.ind.level)
  }
  sp.ind.levels
}

#### Ignore BT locality ###
ignore.locality<-function(locationid){
  if(locationid=="TT"){
    ignore<-c("V0064", "V0065", "V0066")
  }else{
    if(locationid=="TH"){
      ignore<-c("V0007", "V0032")
    }else{
      if(locationid=="BT"){
        ignore<-c("V0102")
      }
    }
  }
  ignore
}


##### plot combine species richness indies (sp accumulation and estimator)######
bioplot<-function(location, time = NULL, estimator = "chao, chao2, jack1, jack2, boot", specaccum.method="exact"){
  sprichness<-specaccum(sp.dis(location, time)[,-1], method = specaccum.method)
  specpool<-specpool2(sp.dis(location, time)[,-1])
  n <- length(sprichness$sites)
  lchao<-rep(specpool$chao, n)
  lchao2<-rep(specpool$chao2, n)
  ljack1<-rep(specpool$jack1, n)
  ljack2<-rep(specpool$jack2, n)
  lboot<-rep(specpool$boot, n)
  
  ##### estimator
  tmp<-gsub(" ", "", estimator)
  tmp<-strsplit(tmp, ",")[[1]]
  
  max1<-max(sprichness$richness)
  max2<-max(specpool)
  maxy<-max(max1, max2)
  maxsd<-max(sprichness$sd)
  
  ###plot
  plot(sprichness$sites, sprichness$richness, ylim=range(c(0, maxy+maxsd)), type = "l", xlab="Sites", ylab=specaccum.method)
  
  epsilon = 0.2
  
  for(i in 1:length(sprichness$sites)) {
    up = sprichness$richness[i] + sprichness$sd[i]
    low = sprichness$richness[i] - sprichness$sd[i]
    segments(sprichness$sites[i],low , sprichness$site[i], up)
    segments(sprichness$sites[i]-epsilon, up , sprichness$sites[i]+epsilon, up)
    segments(sprichness$sites[i]-epsilon, low , sprichness$sites[i]+epsilon, low)
    }
  if(length(tmp[tmp=="chao"])==1){
    par(new=TRUE)
    plot(lchao, ylim=range(c(0, maxy+maxsd)), type="l", xlab="", ylab="", axes=F)
    text(x=2, y=specpool$chao+3, paste("Chao = ", round(specpool$chao,0), sep=""), cex=0.8, pos=4)
  }
  if(length(tmp[tmp=="chao2"])==1){
    par(new=TRUE)
    plot(lchao2, ylim=range(c(0, maxy+maxsd)), type="l", xlab="", ylab="", axes=F)
    text(x=2, y=specpool$chao2+3, paste("Chao2 = ", round(specpool$chao2,0), sep=""), cex=0.8, pos=4)
  }
  if(length(tmp[tmp=="jack1"])==1){
    par(new=TRUE)
    plot(ljack1, ylim=range(c(0, maxy+maxsd)), type="l", xlab="", ylab="", axes=F)
    text(x=2, y=specpool$jack1+3, paste("Jack1 = ", round(specpool$jack1,0), sep=""), cex=0.8, pos=4)
  }
  if(length(tmp[tmp=="jack2"])==1){
    par(new=TRUE)
    plot(ljack2, ylim=range(c(0, maxy+maxsd)), type="l", xlab="", ylab="", axes=F)
    text(x=2, y=specpool$jack2+3, paste("Jack2 = ", round(specpool$jack1,0), sep=""), cex=0.8, pos=4)
  }
  if(length(tmp[tmp=="boot"])==1){
    par(new=TRUE)
    plot(lboot, ylim=range(c(0, maxy+maxsd)), type="l", xlab="", ylab="", axes=F)
    text(x=2, y=specpool$boot+3, paste("Boot = ", round(specpool$boot,0), sep=""), cex=0.8, pos=4)
  }
  
}


### species number or species richness by localityid#######
spnumber.locality<-function(localityid){
  drv<-dbDriver("SQLite")
  con<-dbConnect(drv, datafile())
  
  sql<-paste("SELECT localityid, count(specieid) AS spnumber FROM fullspecimen WHERE localityid = '", localityid, "'", sep="")
  
  spnumber.locality<-dbGetQuery(con, sql)
  
  dbDisconnect(con) 
  spnumber.locality
}

### species number by location and time #########
spnumber.location<-function(locationid, time=NULL){
  spnumber.location<-nrow(spid.list(locationid, time))
  spnumber.location
}

### species number by separated location ###
spnumber.locations<-function(locationid, time=NULL){
  spnumber.locations<-data.frame()
  
  tmp<-gsub(" ", "", locationid)
  tmp<-strsplit(tmp, ",")[[1]]
  
  for(i in 1:length(tmp)){
    spnumber.locations[i,1] <- tmp[i]
    spnumber.locations[i,2] <- spnumber.location(tmp[i], time)
  }
  spnumber.locations
}

### species number by separated locality ####
spnumber.localitys<-function(locationid, time=NULL){
  locality.list<-locality.list(locationid, time)
  spnumber.localitys<-NULL
  for(i in 1:length(locality.list)){
    spnumber.locality<-spnumber.locality(locality.list[i])
    spnumber.localitys<-rbind(spnumber.localitys, spnumber.locality)
  }
  spnumber.localitys
}

############### se #############
se<-function(x){
  se<-sd(x)/sqrt(length(x))
  se
}

##### plot android ####
plot.ad<-function(x, filename, type = "plot"){
pdf(ex.dir(filename), height=5, width = 7)
if(type == "boxplot"){
boxplot(x)
}
if(type == "pie"){
pie(x)
}
if(type=="plot"){
plot(x)
}
dev.off()
}

############update plot on android #############
plot.ad2<-function(x, filename){
pdf(ex.dir(filename), height=5, width = 7)
x
dev.off()
}
