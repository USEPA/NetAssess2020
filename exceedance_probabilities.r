##############################################################################
## Calculate ozone and PM2.5 exceedance probabilities using Downscaler outputs
## Note: Script run on NCC/HPCC cluster (atmos), some files too large for PC
##############################################################################
setwd("/work/YODA/users/bwells01/Boot/DS/")
options(stringsAsFactors=FALSE,warn=-1)
library(data.table); library(shapefiles);

## Get year of most recent Downscaler run from EPA website
download.file(url="https://www.epa.gov/hesc/rsig-related-downloadable-data-files",destfile="temp.txt")
html.txt <- c(read.table(file="temp.txt",sep="\n")[,1])
years <- substr(gsub("<ul>","",gsub("\t","",html.txt[grep("(CONUS)",html.txt)])),5,8)
curr.year <- as.numeric(years[length(years)])
unlink("temp.txt"); rm(html.txt,years);

## Download and process daily ozone and PM2.5 outputs from RSIG website
get.ds.output <- function(year) {
  base.url <- "https://ofmpub.epa.gov/rsig/rsigserver?data/FAQSD/outputs/"
  download.file(paste(base.url,year,"_ozone_daily_8hour_maximum.txt.gz",sep=""),
    destfile=paste("DSoutput_ozone_",year,".txt.gz",sep=""))
  ozone <- read.csv(gzfile(paste("DSoutput_ozone_",year,".txt.gz",sep="")),
    colClasses=c(rep("character",2),rep("numeric",4)))
  colnames(ozone) <- c("date","fips","lon","lat","pred","stderr")
  dates <- gsub("/","-",unique(ozone$date)); tracts <- unique(ozone$fips);
  ozone.pred <- matrix(ozone$pred,nrow=length(tracts),ncol=length(dates),dimnames=list(tracts,dates))
  ozone.stderr <- matrix(ozone$stderr,nrow=length(tracts),ncol=length(dates),dimnames=list(tracts,dates))
  save(list=c("ozone.pred","ozone.stderr"),file=paste("DSoutput_ozone_",year,".Rdata",sep=""))
  unlink(paste("DSoutput_ozone_",year,".txt.gz",sep=""))
  download.file(paste(base.url,year,"_pm25_daily_average.txt.gz",sep=""),
    destfile=paste("DSoutput_PM25_",year,".txt.gz",sep=""))
  pm25 <- read.csv(gzfile(paste("DSoutput_PM25_",year,".txt.gz",sep="")),
    colClasses=c(rep("character",2),rep("numeric",4)))
  colnames(pm25) <- c("date","fips","lon","lat","pred","stderr")
  dates <- gsub("/","-",unique(pm25$date)); tracts <- unique(pm25$fips);
  pm25.pred <- matrix(pm25$pred,nrow=length(tracts),ncol=length(dates),dimnames=list(tracts,dates))
  pm25.stderr <- matrix(pm25$stderr,nrow=length(tracts),ncol=length(dates),dimnames=list(tracts,dates))
  save(list=c("pm25.pred","pm25.stderr"),file=paste("DSoutput_PM25_",year,".Rdata",sep=""))
  unlink(paste("DSoutput_PM25_",year,".txt.gz",sep=""))
}
get.ds.output(curr.year)

## Use bootstrapping to estimate annual ozone and PM2.5 exceedance probabilities
## Note: this part of the script takes several hours to run
load(paste("DSoutput_ozone_",curr.year,".Rdata",sep=""))
load(paste("DSoutput_PM25_",curr.year,".Rdata",sep=""))
prob.out <- data.frame(CTFIPS=rownames(ozone.pred),ozone_prob=NA,pm25_prob=NA)
set.seed(8675309)
for (i in 1:nrow(prob.out)) {
  ozone.boot <- mapply(function(pred,se) rnorm(1000,pred,se),ozone.pred[i,],ozone.stderr[i,])
  ozone.max4 <- apply(ozone.boot,1,function(x) x[order(x,decreasing=TRUE)][4])
  prob.out$ozone_prob[i] <- sum(ozone.max4 >= 71)/1000
  pm25.boot <- mapply(function(pred,se) rnorm(1000,pred,se),pm25.pred[i,],pm25.stderr[i,])
  ly <- as.numeric(curr.year %% 4 == 0)
  q <- data.frame(b=c(1,91,182,274)+c(0,rep(ly,3)),e=c(90,181,273,365)+rep(ly,4))
  pm25.mean <- apply(pmax(pm25.boot,0),1,function(x) (mean(x[q$b[1]:q$e[1]]) + 
    mean(x[q$b[2]:q$e[2]]) + mean(x[q$b[3]:q$e[3]]) + mean(x[q$b[4]:q$e[4]]))/4)
  pm25.p98 <- apply(pm25.boot,1,function(x) x[order(x,decreasing=TRUE)][8])
  prob.out$pm25_prob[i] <- sum(pm25.mean >= 12.05 | pm25.p98 >= 35.5)/1000
  if (i %% 1000 == 0) { cat(i,as.character(Sys.time()),"\n") }
}
save(prob.out,file="ozone_pm25_excprob.Rdata")

## Download and extract 2010 census tract shapefiles from Census Bureau website
base.url <- "https://www2.census.gov/geo/tiger"
file.name <- "Tract_2010Census_DP1.zip"
download.file(url=paste(base.url,"TIGER2010DP1",file.name,sep="/"),destfile=file.name)
unzip(zipfile=file.name,files=c(gsub(".zip",".dbf",file.name),
  gsub(".zip",".shp",file.name),gsub(".zip",".shx",file.name)))

## Convert census tract shapefiles into .Rdata format
## Note: this part of the script took about an hour to run
shp.data <- read.shapefile(gsub(".zip","",file.name))
dbf <- as.data.frame(shp.data$dbf$dbf)
shp <- change.id(convert.to.simple(shp.data$shp),dbf$GEOID10)
index <- table(shp$Id)
nparts <- unlist(lapply(shp.data$shp$shp,function(x) x$num.parts))
parts <- lapply(shp.data$shp$shp,function(x) x$parts+1)
rm(shp.data)
tract.shape <- vector("list",nrow(dbf))
start.row <- 1
for (i in 1:nrow(dbf)) {
  rows <- c(start.row:(start.row+index[match(dbf$GEOID10[i],names(index))]-1))
  t <- shp[rows,]
  if (i %% 1000 == 0) { cat(i,as.character(Sys.time()),object.size(tract.shape),"\n") }
  if (nparts[i] == 1) { 
    tract.shape[[i]] <- data.frame(fips=as.character(dbf$GEOID[i]),id=1,lat=t$Y,lon=t$X)
    start.row <- start.row + length(rows)
    next
  }
  for (j in 1:nparts[i]) {
    k <- c(parts[[i]][j]:ifelse(j < nparts[i],parts[[i]][j+1]-1,nrow(t)))
    temp <- data.frame(fips=as.character(dbf$GEOID[i]),id=j,lat=t$Y[k],lon=t$X[k])
    tract.shape[[i]] <- rbind(tract.shape[[i]],temp)
  }
  start.row <- start.row + length(rows)
}
rm(dbf,shp)
tract.shape <- as.data.frame(rbindlist(tract.shape))
tract.shape <- tract.shape[order(tract.shape$fips,tract.shape$id),]
save(tract.shape,file="census_tract_shape.Rdata")
unlink(c(gsub(".zip",".dbf",file.name),gsub(".zip",".shp",file.name),
  gsub(".zip",".shx",file.name),file.name))

## Generate ozone and PM2.5 exceedance probability maps for the network assessment app overlays
source("/work/YODA/users/bwells01/Census/shape2014/rmapfuns.r")
load("census_tract_shape.Rdata")
load("ozone_pm25_excprob.Rdata")
tract.shape <- subset(tract.shape,fips %in% prob.out$CTFIPS)
coord <- spTransform(SpatialPoints(tract.shape[,c("lon","lat")],CRS("+proj=longlat")),
  CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0"))
tract.shape$lat <- coord@coords[,2]; tract.shape$lon <- coord@coords[,1];
tract.shape <- split(tract.shape,tract.shape$fips)
tract.ozone <- assign.colors(prob.out$ozone_prob,range=c(0,1),palette=tim.colors(16)[4:13])
tract.pm25 <- assign.colors(prob.out$pm25_prob,range=c(0,1),palette=tim.colors(16)[4:13])

png(file="ozone_prob.png",width=3000,height=1500,bg="transparent")
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlab="",xlim=coord@bbox[1,],
  yaxs='i',ylab="",ylim=coord@bbox[2,],main="")
for (i in 1:length(tract.shape)) {
  add.layer(type="polygon",x=tract.shape[[i]]$lon,y=tract.shape[[i]]$lat,
    id=tract.shape[[i]]$id,rescale=FALSE,border=NA,col=tract.ozone[i])
}
dev.off()

png(file="pm25_prob.png",width=3000,height=1500,bg="transparent")
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlab="",xlim=coord@bbox[1,],
  yaxs='i',ylab="",ylim=coord@bbox[2,],main="")
for (i in 1:length(tract.shape)) {
  add.layer(type="polygon",x=tract.shape[[i]]$lon,y=tract.shape[[i]]$lat,
    id=tract.shape[[i]]$id,border=NA,col=tract.pm25[i],rescale=FALSE)
}
dev.off()

## Re-create the probability bar for the app legend
setwd("C:/Users/bwells01/Documents/Monitoring/Network Assessment/shinyApp_test")
png(file="www/images/probLegend.png",width=350,height=70)
par(mar=c(2.5,1.5,0.5,1.5),cex.axis=1.5,cex.lab=1.5)
image(x=c(1:10)-0.5,y=1,z=matrix(c(1:10),10,1),xlim=c(0,10),ylim=c(0,1),zlim=c(0,10),
  axes=FALSE,col=tim.colors(16)[4:13],legend.only=TRUE)
axis(side=1,at=seq(0,10,2),labels=paste(seq(0,100,20),"%",sep=""))
box(); dev.off();
