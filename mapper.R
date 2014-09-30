library(data.table);
library(maps);
library(maptools);
library(spatstat);
library(zipcode);
library(GISTools);
library(dplyr);
#load the zipcode dataset
data(zipcode)

## File may be found here:
## http://aspe.hhs.gov/health/reports/2014/MarketPlaceEnrollment/EnrollmentByZip/rpt_EnrollmentByZip.cfm
## Originally XLSX, conversion within Excel to CSV seems easiest.
zip_enrollment  =  read.csv("zip_enrollment.csv", na.strings="*")

## File may be found here:
## http://udsmapper.org/zcta-crosswalk.cfm
## Again Excel, convert to csv.
zctaToZip  =  read.csv("Zip_to_ZCTA_Crosswalk_JSI2014.csv")

## Census Gazatteer summaries. Version used here is the 2010 version.
## URL: https://www.census.gov/geo/maps-data/data/gazetteer.html
## 2010 Gaz. Zip Code Tabulation Areas.
gaz = read.table("Gaz_zcta_national.txt", header=TRUE, colClasses=c("character", rep("numeric", 8)))


## Filter data sets to state specific
zip_enrollment = filter(zip_enrollment,state=='Kansas')
zctaToZip = filter(zctaToZip,STATE=='KS')
zip_enrollment[,1]=as.character(zip_enrollment[,1])

colnames(gaz)[1]="zip"

density = inner_join(zip_enrollment,gaz,by="zip") %>% mutate(percent=enroll/POP10)
density$breaks = cut(density$percent,breaks=c(0,.005,.01,.015,.02,.025,.03,.035),labels=FALSE)

shapes = shapes = readShapeSpatial("tl_2010_20_zcta510.shp")
#convert the zip from a factor to a character
shapes@data$ZCTA_CHAR = levels(shapes@data$ZCTA5CE10)[shapes@data$ZCTA5CE10]
#store all of our original row numbers, since we'll need to match them up later
shapes@data$original_rownum = rownames(shapes@data)

merged = merge(shapes@data, density, by.x=c("ZCTA_CHAR"), by.y=c("zip"), all.x=TRUE, sort=FALSE);
merged = merged[order(as.numeric(merged$original_rownum)),]
#*********************************************************************************
#force the row names of the data frame to be the same as the ID's of the polygons, 
#because, according to 
#http://rss.acs.unt.edu/Rdoc/library/sp/html/SpatialPolygonsDataFrame-class.html, 
#things will get re-ordered otherwise and our plot will get scrambled
#this will allow you to see the row names of the polygons:
#ids = sapply(shapes@polygons, function(x){x@ID})
#this will allow you to see the row names of the dataframe:
#dataids = rownames(shapes@data)
#they have to match: identical(ids, dataids)
#*********************************************************************************
rownames(merged) = merged$original_rownum
shapes@data = merged

#make a color palette
my.palette  =  colorRampPalette(c("white","black"), bias=1, space="Lab")
shapes.plotvar = (shapes@data$percent)

#ignore any zcta's with outlier data
#shapes.plotvar[is.na(shapes.plotvar)] = 0
# shapes.plotvar[!is.finite(shapes.plotvar)] = 0
#shapes.plotvar = shapes.plotvar[!is.na(shapes.plotvar)]

#nShades = 10
#autoshades = auto.shading(shapes.plotvar,rangeCuts,digits = 5 ,n=8, cols=brewer.pal(8,'YlGnBu'))
manualpallete = brewer.pal(6,'GnBu')
manualpallete[1]="#FFFFFF"
shades = shading(breaks=c(.01,.02,.025,.03,.035),cols=manualpallete)
attr(x = shades$breaks,"names") = c("1%","2%","2.5%","3%","3.5")

choropleth(shapes, v=shapes.plotvar, shades, bg="white", border="NA", lwd=0.1)
title("Enrollment by Zip Code as % of Population")
legend(-99.99,36.75,title='% Enrollment',ncol=2,
       legend=c("Unk","1-2","2-2.5","2.5-3","3-3.5","3.5+"),col=manualpallete,
       pch=15,bg="gray")
map(database = "state", regions = "Kansas", col="white", lwd=1.0,boundary = TRUE, add=TRUE);
# Outline
map("state", regions = "Kansas", col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 1)
map("county",regions = "Kansas",col="black",fill=FALSE, add = TRUE, lty=1,lwd=1)

out_summary = inner_join(density,zipcode,by="zip") %>% dplyr::select(city,zip,enroll,POP10,percent) %>% arrange(-percent) 

write.csv(out_summary,"enrollment_summary.csv")
pdf(file = "without_counties.pdf",width = 1280,height=1024)
choropleth(shapes, v=shapes.plotvar, shades, bg="white", border="NA", lwd=0.1)
title("Enrollment by Zip Code as % of Population")
legend(-99.99,36.75,title='% Enrollment',ncol=2,
       legend=c("Unk","1-2","2-2.5","2.5-3","3-3.5","3.5+"),col=manualpallete,
       pch=15,bg="gray")
map(database = "state", regions = "Kansas", col="white", lwd=1.0,boundary = TRUE, add=TRUE);
# Outline
map("state", regions = "Kansas", col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 1)
dev.off()

pdf(file="with_counties.pdf",width=1280,height=1024)
choropleth(shapes, v=shapes.plotvar, shades, bg="white", border="NA", lwd=0.1)
title("Enrollment by Zip Code as % of Population")
legend(-99.99,36.75,title='% Enrollment',ncol=2,
       legend=c("Unk","1-2","2-2.5","2.5-3","3-3.5","3.5+"),col=manualpallete,
       pch=15,bg="gray")
map(database = "state", regions = "Kansas", col="white", lwd=1.0,boundary = TRUE, add=TRUE);
# Outline
map("state", regions = "Kansas", col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 1)
map("county",regions = "Kansas",col="black",fill=FALSE, add = TRUE, lty=1,lwd=1)
dev.off()