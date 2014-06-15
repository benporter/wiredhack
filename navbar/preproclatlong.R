# Preprocessing for pulling lat log coords for zip codes

library(sqldf)
library(ggmap) #for the geocode function

zipDF <- sqldf("select Zip, count(*) as Students 
               from students
               group by Zip")
# remove non-standard, missing, low-count, and 
# otherwise difficult to search zip codes
zipDF <- subset(zipDF,Zip != c("","00000"))
zipDF <- subset(zipDF,!is.na(Zip))
zipDF <- subset(zipDF,Students>5)
zipDF$ziplength <- nchar(zipDF$Zip)
zipDF <- subset(zipDF,ziplength==5)

# create columns for storing lat lon values
zipDF$lon <- rep(0.0,nrow(zipDF))
zipDF$lat <- rep(0.0,nrow(zipDF))
zipDF$lon_num <- rep(0.0,nrow(zipDF))
zipDF$lat_num <- rep(0.0,nrow(zipDF))

# pull the lat long coords, takes ~1-2 mins
for(i in 1:nrow(zipDF)) {
  geotemp <- geocode(zipDF$Zip[i])
  zipDF$lon[i] <- geotemp[1]
  zipDF$lat[i] <- geotemp[2]
}

# make a second copy of lat long, storing as numeric values
# so that jitter can be added in the app
zipDF$lon_num <- as.numeric(zipDF$lon)
zipDF$lat_num <- as.numeric(zipDF$lat)

library(RH2) # makes the errors from this join go away, dunno why
latlonStudent <- sqldf("select s.* ,zdf.lat_num,zdf.lon_num
                       from students s left join 
                       zipDF zdf
                       on s.Zip = zdf.Zip")

# save just the result, the latlonStudent object
save(latlonStudent,file="/home/benporter/ShinyApps/navbar/latlotstudent.Rdata")