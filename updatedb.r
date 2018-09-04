setwd("C:/Users/bwells01/Documents/Monitoring/Network Assessment/shinyApp_test/")
source("C:/Users/bwells01/Documents/R/get_aqs_data.r")
options(stringsAsFactors=FALSE,warn=-1)
library(data.table); library(DBI); library(plyr); library(RSQLite); library(shapefiles);
db <- dbConnect(SQLite(),dbname="netassess.sqlite") ## Connect to SQLite database
dir.create("data") ## Create a new directory to store intermediate data files

###########################################################################
## Step 1: Update Geography Tables using latest available Census data
## type = one of "nation", "state", "county", "cbsa", or "csa"
## year = year of shapefile publication - for latest year available, check
##   https://www.census.gov/geo/maps-data/data/tiger-cart-boundary.html
###########################################################################
update.geo <- function(type,year) {
  base.url <- paste("https://www2.census.gov/geo/tiger/GENZ",year,"/shp",sep="")
  file.name <- paste("cb",year,"us",type,"20m.zip",sep="_")
  if (!file.exists(paste("data",file.name,sep="/"))) {
    download.file(url=paste(base.url,file.name,sep="/"),
      destfile=paste("data",file.name,sep="/"),quiet=TRUE)
    unzip(zipfile=paste("data",file.name,sep="/"),files=c(gsub(".zip",".dbf",file.name),
      gsub(".zip",".shp",file.name),gsub(".zip",".shx",file.name)),exdir="data")
  }
  shp.data <- read.shapefile(paste("data",gsub(".zip","",file.name),sep="/"))
  dbf <- as.data.frame(shp.data$dbf$dbf)
  if (type == "nation") {
    out <- data.frame(CODE="US",NAME="United States",GEOMETRY=NA)
  }
  if (type == "state") {
    out <- data.frame(CODE=dbf$STATEFP,NAME=dbf$NAME,ABBR=dbf$STUSPS,GEOMETRY=NA)
  }
  if (type == "county") {
    out <- data.frame(STATE_CODE=dbf$STATEFP,COUNTY_CODE=dbf$COUNTYFP,NAME=dbf$NAME,
      GEOMETRY=NA)
  }
  if (type == "cbsa") {
    out <- data.frame(CODE=dbf$CBSAFP,NAME=dbf$NAME,TYPE=sapply(dbf$LSAD,function(x)
      ifelse(x == "M1","Metropolitan","Micropolitan")),GEOMETRY=NA)
  }
  if (type == "csa") {
    out <- data.frame(CODE=dbf$CSAFP,NAME=dbf$NAME,GEOMETRY=NA)
  }
  shp <- change.id(convert.to.simple(shp.data$shp),dbf$GEOID)
  index <- lapply(dbf$GEOID,function(x) grep(x,shp$Id))
  nparts <- unlist(lapply(shp.data$shp$shp,function(x) x$num.parts))
  parts <- lapply(shp.data$shp$shp,function(x) x$parts+1)
  for (i in 1:nrow(out)) {
    t <- shp[index[[i]],]
    temp <- vector("list",nparts[i])
    for (j in 1:nparts[i]) {
      k <- c(parts[[i]][j]:ifelse(j < nparts[i],parts[[i]][j+1]-1,nrow(t)))
      temp[[j]] <- paste("structure(c(",paste(unlist(c(t$Y[k],t$X[k])),collapse=","),
        "),.Dim=c(",length(k),"L,2L),.Dimnames=list(NULL,c('lat','lng')))",sep="")
    }
    out$GEOMETRY[i] <- paste("list(",paste(temp,collapse=","),")",sep="")
  }
  out <- out[order(dbf$GEOID),]
  table.name <- switch(type,nation="usborder",state="states",
    cbsa="cbsas",csa="csas",county="counties")
  dbWriteTable(db,name=table.name,value=out,row.names=FALSE,overwrite=TRUE)
  file.rm <- list.files("data")[grep(gsub(".zip","",file.name),list.files("data"))]
  unlink(paste("data",file.rm,sep="/"))
  invisible(NULL)
}
update.geo(type="nation",year=2017)
update.geo(type="state",year=2017)
update.geo(type="county",year=2017)
update.geo(type="cbsa",year=2017)
update.geo(type="csa",year=2017)

#######################################################################################
## Step 3: Update census tract population information - MANUAL
## a) Download latest census tract geographic information from the ensus Bureau
##    https://www2.census.gov/geo/docs/maps-data/data/gazetteer/Gaz_tracts_national.zip
##    Extract the .zip file to the current working 'data' directory
## b) Download latest census tract level population data from American Factfinder
##    https://factfinder.census.gov/faces/nav/jsf/pages/download_center.xhtml
##    Choose 'SF1' dataset from most recent census with 'DP1' table option
##    Extract the .zip file to the current working 'data' directory
## c) Run the 'exceedance_probabilities.r' script to get ozone and pm2.5 probabilities
##    Note: script takes several hours to run and uses a large amount of memory
##    When finished, copy the final output file to the current working 'data' directory
##    Also copy ozone and pm2.5 probability surfaces to 'www/images' directory
## d) Run below code to read, merge, and write the tract-level data to the database
##    To generate the ozone and PM2.5 probability surfaces, run 'probability_surfaces.r'
########################################################################################
update.population <- function(geo.file,pop.file,prob.file) {
  t <- read.delim(paste("data",geo.file,sep="/"),header=TRUE,
    colClasses=c(rep("character",2),rep("numeric",8)))
  geo <- data.frame(CTFIPS=t$GEOID,latitude=round(t$INTPTLAT,6),
    longitude=round(t$INTPTLONG,6),area_km=t$ALAND/1e6,population=t$POP10)
  t <- read.csv(paste("data",pop.file,sep="/"),header=FALSE,skip=2,colClasses="character")
  pop <- data.frame(CTFIPS=t[,2],total=as.integer(t[,4]),male=as.integer(t[,54]),
    female=as.integer(t[,104]),white=as.integer(t[,158]),black=as.integer(t[,160]),
    native=as.integer(t[,162]),asian=as.integer(t[,164]),islander=as.integer(t[,180]),
    other=as.integer(t[,190]),multiple=as.integer(t[,192]),hispanic=as.integer(t[,216]),
    age_0_4=as.integer(t[,6]),age_5_9=as.integer(t[,8]),age_10_14=as.integer(t[,10]),
    age_15_19=as.integer(t[,12]),age_20_24=as.integer(t[,14]),age_25_29=as.integer(t[,16]),
    age_30_34=as.integer(t[,18]),age_35_39=as.integer(t[,20]),age_40_44=as.integer(t[,22]),
    age_45_49=as.integer(t[,24]),age_50_54=as.integer(t[,26]),age_55_59=as.integer(t[,28]),
    age_60_64=as.integer(t[,30]),age_65_69=as.integer(t[,32]),age_70_74=as.integer(t[,34]),
    age_75_79=as.integer(t[,36]),age_80_84=as.integer(t[,38]),age_85_up=as.integer(t[,40]))
  geo.pop <- merge(geo,pop[,-2],by="CTFIPS")
  load(paste("data",prob.file,sep="/"))
  tracts <- merge(geo.pop,prob.out,by="CTFIPS",all=TRUE)
  dbWriteTable(db,name="tracts",value=tracts,row.names=FALSE,overwrite=TRUE)
}
update.population(geo.file="Gaz_tracts_national.txt",
  pop.file="DEC_10_SF1_SF1DP1_with_ann.csv",
  prob.file="ozone_pm25_excprob.Rdata")

## Update AQS parameter code/pollutant standards info
download.file(url="https://aqs.epa.gov/aqsweb/documents/codetables/pollutant_standards.csv",
  destfile=paste("data","pollutant_standards.csv",sep="/"),quiet=TRUE)
t <- read.csv("data/pollutant_standards.csv")
t <- t[order(t$Pollutant.Standard.ID),]
naaqs <- data.frame(ps_id=t$Pollutant.Standard.ID,
  naaqs=t$Pollutant.Standard.Short.Name,
  pollutant=sapply(t$Pollutant.Standard.Short.Name,
    function(x) tolower(unlist(strsplit(x," "))[1])),
  poll_name=sapply(t$Pollutant.Standard.Short.Name,
    function(x) gsub("25","2.5",unlist(strsplit(x," "))[1])),
  avg_time=sapply(t$Pollutant.Standard.Short.Name,
    function(x) tolower(unlist(strsplit(x," "))[2])),
  year=sapply(t$Pollutant.Standard.Short.Name,
    function(x) as.numeric(substr(x,nchar(x)-3,nchar(x)))),
  parameter_code=t$Parameter.Code,parameter_desc=t$Parameter,
  daily_statistic=NA,annual_statistic=t$NAAQS.Statistic,
  level=t$Primary.Standard.Level,units=t$Standard.Units,row.names=1:nrow(t))
naaqs$daily_statistic <- sapply(naaqs$avg_time,function(x) 
  ifelse(grepl("hour",x) & !grepl("24",x),"max","mean"))
naaqs$naaqs[which(naaqs$ps_id == 20)] <- "NO2 1-hour 2010"
naaqs$year[which(naaqs$ps_id == 20)] <- 2010
naaqs$level[which(naaqs$ps_id == 5)] <- 500
naaqs$units <- sapply(naaqs$units,function(x)
  ifelse(grepl("Micrograms",x),"ug/m3",ifelse(grepl("billion",x),"ppb","ppm")))
dbWriteTable(db,name="standards",value=naaqs,row.names=FALSE,overwrite=TRUE)

## Get all AQS monitors currently operating
par <- unlist(dbGetQuery(db,"SELECT DISTINCT parameter_code FROM standards"))
year <- as.numeric(substr(Sys.Date(),1,4)) - ifelse(as.numeric(substr(Sys.Date(),6,7)) > 4,1,2)
monitors <- get.aqs.data(paste("
SELECT monitors.mo_id AS mo_id,
       sites.si_id AS key,
       monitors.pa_parameter_code AS parameter_code,
       sites.state_code || sites.county_code || sites.site_id AS site_id,
       monitors.poc AS poc,
       TO_CHAR(sp.sampling_begin_date+1/24,'YYYY-MM-DD') AS begin_date
  FROM monitors monitors,
       sample_periods sp,
       site_basic sites
 WHERE monitors.si_si_id = sites.si_id
   AND sp.mo_mo_id = monitors.mo_id
   AND monitors.status_ind = 'P'
   AND sites.status_ind = 'P'
   AND sp.status_ind = 'P'
   AND EXTRACT(YEAR FROM monitors.last_sampling_date) >= ",year,"
   AND monitors.pa_parameter_code IN (",
       paste(sapply(par,function(x) paste("\'",x,"\'",sep="")),collapse=","),")
   AND sites.state_code NOT IN ('80','CC')
   AND sp.sampling_end_date IS NULL
 ORDER BY 3,4,5",sep=""))
dbWriteTable(db,name="monitors",value=monitors,row.names=FALSE,overwrite=TRUE)

## Get site information for all AQS monitors
sites <- subset(get.aqs.data("
SELECT sites.si_id AS key,
       sites.state_code || sites.county_code || sites.site_id AS site_id,
       sites.local_site_name AS site_name,
       sites.street_address AS address,
       sites.standard_latitude AS latitude,
       sites.standard_longitude AS longitude,
       TO_NUMBER(regions.epa_region_code,'99') AS epa_region,
       states.state_name AS state_name,
       counties.county_name AS county_name,
       COALESCE(cbsas.cbsa_name,' ') AS cbsa_name,
       COALESCE(cbsas.csa_title,' ') AS csa_title
  FROM cbsa_mapping cbsas,
       counties counties,
       epa_regions regions,
       site_basic sites,
       states states
 WHERE regions.epa_region_code = states.epar_epa_region_code
   AND sites.county_code = cbsas.county_code(+)
   AND sites.county_code = counties.county_code
   AND sites.state_code = cbsas.state_code(+)
   AND sites.state_code = counties.stt_state_code
   AND sites.state_code = states.state_code
   AND sites.status_ind = 'P'
 ORDER BY 2"),key %in% unique(monitors$key))
sites$monitor_count <- sapply(sites$key,function(x) sum(monitors$key == x))
dbWriteTable(db,name="sites",value=sites,row.names=FALSE,overwrite=TRUE)

## Get most recent 3 years of daily summary data for all AQS monitors
naaqs <- dbGetQuery(db,"SELECT * FROM standards")
sites <- dbGetQuery(db,"SELECT * FROM sites")
years <- as.numeric(substr(Sys.Date(),1,4)) - c(3:1) - 
  ifelse(as.numeric(substr(Sys.Date(),6,7)) > 4,0,1)
curr.psid <- c(1,2,4,12,14,19,20,21,23)
temp <- vector("list",length(curr.psid))
for (p in curr.psid) {
  temp[[match(p,curr.psid)]] <- get.aqs.data(paste("
  SELECT ds.pollutant_standard_id AS ps_id, 
         ds.mo_mo_id AS mo_id,
         TO_CHAR(ds.daily_coll_date,'YYYY-MM-DD') AS sample_date, ",
         switch(naaqs$daily_statistic[which(naaqs$ps_id == p)],
           max="GREATEST(ds.daily_max_sample_value,0) AS value ",
           mean="GREATEST(ds.daily_arith_mean,0) AS value "),
   "FROM daily_summaries ds
   WHERE ds.daily_criteria_ind = 'Y'
     AND ds.edt_edt_id IN (0,2)
     AND ds.pollutant_standard_id = ",p,"
     AND EXTRACT(YEAR FROM ds.daily_coll_date) IN (",paste(years,collapse=","),")
   ORDER BY 1,2,3",sep=""))
  cat(naaqs$naaqs[which(naaqs$ps_id == p)],"\n")
}
temp <- as.data.frame(rbindlist(temp))
xwalk <- get.aqs.data(paste("
SELECT site_basic.si_id, monitors.mo_id
  FROM monitors, site_basic
 WHERE monitors.si_si_id = site_basic.si_id
   AND monitors.pa_parameter_code IN (",
       paste(sapply(par,function(x) paste("\'",x,"\'",sep="")),collapse=","),")
 ORDER BY 1",sep=""))
temp$pollutant <- naaqs$pollutant[match(temp$ps_id,naaqs$ps_id)]
temp$key <- xwalk$si_id[match(temp$mo_id,xwalk$mo_id)]
temp <- subset(temp,key %in% sites$key)
daily <- ddply(temp,c("pollutant","key","sample_date"),summarize,value=max(value))
dbWriteTable(db,name="daily",value=daily,row.names=FALSE,overwrite=TRUE)

## Get annual summary data for 2000-present for all AQS monitors
naaqs <- dbGetQuery(db,"SELECT * FROM standards")
sites <- dbGetQuery(db,"SELECT * FROM sites")
years <- c(2000:(as.numeric(substr(Sys.Date(),1,4)) -  
  ifelse(as.numeric(substr(Sys.Date(),6,7)) > 4,1,2)))
curr.psid <- c(1,2,4,12,14,16,19,20,22,23)
temp <- vector("list",length(curr.psid))
for (p in curr.psid) {
  poll <- naaqs$pollutant[which(naaqs$ps_id == p)]
  if (poll == "pm25") { std <- ifelse(p == 16,"daily","annual") }
  temp[[match(p,curr.psid)]] <- get.aqs.data(paste("
  SELECT ans.pollutant_standard_id AS ps_id, 
         ans.mo_mo_id AS mo_id,
         ans.annual_summary_year AS year, ",switch(poll,
      co="sm.max_sample_value AS value ",
    lead="ans.annual_arith_mean AS value ",
     no2="sp.percentile_sample_value AS value ",
   ozone="sm.max_sample_value AS value ",
    pm10="sm.max_sample_value AS value ",
    pm25=switch(std,annual="ans.weighted_arith_mean AS value ",
         daily="sp.percentile_sample_value AS value "),
    so2="sp.percentile_sample_value AS value "),"
    FROM annual_summaries ans",switch(poll,
      co=", summary_maximums sm ",lead=" ",
     no2=", summary_percentiles sp ",
   ozone=", summary_maximums sm ",
    pm10=", summary_maximums sm ",
    pm25=switch(std,annual=" ",daily=", summary_percentiles sp "),
     so2=", summary_percentiles sp "),"
   WHERE ans.annual_obs_pct >= 50
     AND ans.edt_edt_id IN (0,2)
     AND ans.annual_summary_year IN (",paste(years,collapse=","),")
     AND ans.pollutant_standard_id = ",p,switch(poll,
      co=" AND sm.ans_ans_id = ans.ans_id AND sm.max_level = 2 ",lead=" ",
     no2=" AND sp.ans_ans_id = ans.ans_id AND sp.percentile_num = 98 ",
   ozone=" AND sm.ans_ans_id = ans.ans_id AND sm.max_level = 4 ",
    pm10=" AND sm.ans_ans_id = ans.ans_id AND sm.max_level = 2 ",
    pm25=switch(std,annual=" ",daily=" AND sp.ans_ans_id = ans.ans_id AND sp.percentile_num = 98 "),
     so2=" AND sp.ans_ans_id = ans.ans_id AND sp.percentile_num = 99 "),"
   ORDER BY 1,2,3",sep=""))
}
temp <- as.data.frame(rbindlist(temp))
xwalk <- get.aqs.data(paste("
SELECT site_basic.si_id, monitors.mo_id
  FROM monitors, site_basic
 WHERE monitors.si_si_id = site_basic.si_id
   AND monitors.pa_parameter_code IN (",
       paste(sapply(par,function(x) paste("\'",x,"\'",sep="")),collapse=","),")
 ORDER BY 1",sep=""))
temp$pollutant <- naaqs$pollutant[match(temp$ps_id,naaqs$ps_id)]
temp$pollutant <- mapply(function(p,s) paste(p,ifelse(p == "pm25",
  ifelse(s == 16,"d","a"),""),sep=""),p=temp$pollutant,s=temp$ps_id)
temp$key <- xwalk$si_id[match(temp$mo_id,xwalk$mo_id)]
temp <- subset(temp,key %in% sites$key)
annual <- ddply(temp,c("pollutant","key","year"),summarize,value=max(value))
dbWriteTable(db,name="annual",value=annual,row.names=FALSE,overwrite=TRUE)

## Calculate 3-year design values from daily/annual summary data
max.na <- function(x) { return(ifelse(all(is.na(x)),NA,max(x,na.rm=TRUE))) }
annual <- dbGetQuery(db,"SELECT * FROM annual")
sites <- dbGetQuery(db,"SELECT * FROM sites")
years <- c(2000:(as.numeric(substr(Sys.Date(),1,4)) -  
  ifelse(as.numeric(substr(Sys.Date(),6,7)) > 4,1,2)))
dvs <- vector("list",length(unique(annual$pollutant)))
names(dvs) <- unique(annual$pollutant)
dvs$co <- subset(annual,pollutant == "co" & year >= 2002)

## Lead - calculate max 3-month rolling average for each 3-year period
vals <- subset(get.aqs.data(paste("
SELECT lead.si_id AS key,
       lead.summary_year AS year,
       lead.summary_month AS month,
       GREATEST(lead.arith_mean,0) AS value
  FROM lead_site_monthly_summaries lead
 WHERE lead.edt_id IN (0,2)
   AND lead.total_sampled_day_count > 0
   AND lead.summary_year IN (",paste(years,collapse=","),")
 ORDER BY 1,2,3",sep="")),key %in% sites$key)
vals <- vals[which(!duplicated(paste(vals$key,vals$year,vals$month))),]
ids <- unique(vals$key)
all <- data.frame(key=rep(ids,each=12*length(years)),
  year=rep(years,each=12,times=length(ids)),
  month=rep(c(1:12),times=(length(ids)*length(years))))
temp <- merge(all,vals,by=c("key","year","month"),all=TRUE)
temp$val3 <- unlist(tapply(temp$value,list(temp$key),
  function(x) filter(x,rep(1/3,3))))
temp <- ddply(temp,c("key","year"),summarize,max3=max.na(val3))
temp <- matrix(temp$max3,nrow=length(years),ncol=length(ids),dimnames=list(years,ids))
temp <- pmax(temp[c(3:length(years)),],temp[c(2:(length(years)-1)),],
  temp[c(1:(length(years)-2)),],na.rm=TRUE)
dvs$lead <- na.omit(data.frame(pollutant="lead",
  key=as.numeric(rep(colnames(temp),each=nrow(temp))),
  year=as.numeric(rep(rownames(temp),times=ncol(temp))),
  value=c(round(temp,2))))

## Design values for other pollutants - take 3-year averages of annual statistics
design.values <- function(poll) {
  vals <- subset(annual,pollutant == poll)
  ids <- unique(vals$key)
  all <- data.frame(key=rep(ids,each=length(years)),year=rep(years,times=length(ids)))
  temp <- merge(all,vals,by=c("key","year"),all=TRUE)
  temp <- matrix(temp$value,nrow=length(years),ncol=length(ids),dimnames=list(years,ids))
  temp <- (temp[c(3:length(years)),] + temp[c(2:(length(years)-1)),] + 
    temp[c(1:(length(years)-2)),])/3
  if (poll %in% c("no2","pm10","pm25d","so2")) { temp <- round(temp,0) }
  if (poll == "pm25a") { temp <- round(temp,1) }
  if (poll == "ozone") { temp <- floor(temp*1000)/1000 }
  return(na.omit(data.frame(pollutant=poll,
    key=as.numeric(rep(colnames(temp),each=nrow(temp))),
    year=as.numeric(rep(rownames(temp),times=ncol(temp))),
    value=c(temp))))
}
dvs$no2 <- design.values(poll="no2")
dvs$ozone <- design.values(poll="ozone")
dvs$pm10 <- design.values(poll="pm10")
dvs$pm25a <- design.values(poll="pm25a")
dvs$pm25d <- design.values(poll="pm25d")
dvs$so2 <- design.values(poll="so2")
dvs <- data.frame(rbindlist(dvs))
dbWriteTable(db,name="dvs",value=dvs,row.names=FALSE,overwrite=TRUE)

## Remove the temporary data directory and its contents
unlink("data",recursive=TRUE,force=TRUE)
dbDisconnect(db)
