options(stringsAsFactors=FALSE)
require(data.table,quietly=TRUE)
require(deldir,quietly=TRUE)
require(fields,quietly=TRUE)
require(plyr,quietly=TRUE)
require(reshape2,quietly=TRUE)
require(rgdal,quietly=TRUE)
require(rgeos,quietly=TRUE)
require(RSQLite,quietly=TRUE)
require(sp,quietly=TRUE)

if (!dir.exists("www/images/temp")) { dir.create("www/images/temp") }
unlink(paste("www/images/temp",list.files("www/images/temp/"),sep="/"))
db <- dbConnect(SQLite(),dbname="netassess.sqlite")
usborder <- SpatialPolygons(list(Polygons(lapply(eval(parse(text=dbGetQuery(db,"SELECT geometry FROM usborder"))),
        function(p) Polygon(p[,c("lng","lat")])),ID="US")))
tracts <- SpatialPointsDataFrame(coords=dbGetQuery(db,"SELECT longitude, latitude FROM tracts"),
  data=dbGetQuery(db,"SELECT * FROM tracts"))
temp <- dbGetQuery(db,"SELECT * FROM counties")
poly <- sapply(temp$geometry,function(x) eval(parse(text=x)))
counties <- SpatialPolygonsDataFrame(Sr=SpatialPolygons(Srl=mapply(function(poly,id) {
      Polygons(srl=lapply(poly,function(x) Polygon(x[,c("lng","lat")])),ID=id) },
      poly=poly,id=temp$code)),data=as.data.frame(temp[,1:4]),match.ID="code")
rm(temp,poly)

createSites <- function() {
  jsonArray <- function(x,quote=FALSE) {
    return(ifelse(quote,paste('["',paste(x,collapse='", "'),'"]',sep=""), 
      paste("[",paste(x,collapse=", "),"]",sep=""))) 
  }
  jsonObject <- function(x) { 
    return(paste("{",paste(paste('"',names(x),'"',sep=""),
      sapply(x,function(y) ifelse(substr(y,1,1) %in% c("[","{") & 
      substr(y,nchar(y),nchar(y)) %in% c("}","]"),y,paste('"',y,'"',sep=""))),
      sep = ": ",collapse=", "),"}",sep=""))
  }
  m <- dbGetQuery(db,"SELECT * FROM sites")
  coord <- paste(round(m$latitude,6),round(m$longitude,6))
  s <- m[!duplicated(coord),]
  d <- m[duplicated(coord),]
  sites <- sapply(c(1:nrow(s)),function(r) {
    alt <- d$latitude == s$latitude[r] & d$longitude == s$longitude[r]
    key <- s$key[r]
    site_id <- s$site_id[r]
    if(sum(alt) > 0) {
      key <- c(key,d$key[alt])
      site_id <- c(site_id,d$site_id[alt])
      s$monitor_count[r] <- s$monitor_count[r]+sum(d$monitor_count[alt])
    }
    key <- jsonArray(key)
    site_id <- jsonArray(site_id,TRUE)
    p <- c(key=key,site_id=site_id,as.list(s[r,c("site_name","address","epa_region",
      "state_name","county_name","cbsa_name","csa_title","monitor_count","pollutants")]))
    p$site_name <- gsub('"',"&quot;",gsub("'","&#039;",p$site_name,fixed=TRUE),fixed=TRUE)
    p$address <- gsub('"',"&quot;",gsub("'","&#039;",p$address,fixed=TRUE),fixed=TRUE)
    p <- jsonObject(p)
    g <- jsonArray(c(round(s$longitude[r],6),round(s$latitude[r],6)))
    g <- jsonObject(list(type="Point",coordinates=g))
    return(jsonObject(list(type="Feature",geometry=g,properties=p)))
  })
  write(jsonObject(list(type="FeatureCollection",features=jsonArray(sites))),
    file="www/data/sites.geojson")
}
createSites()
