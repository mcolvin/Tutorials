library(spatstat)
library(rgeos)
library(raster)
library(sp)
library(rgdal)
library(dismo)
library(geosphere)
sr <- "+proj=utm +zone=16 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 

## Bay 1 polygon: longview
xy1<-matrix(c(	
	-89.8291043921992,34.12394040226167, 
	-89.83853801663966,34.1189798333901,
	-89.83914137788919,34.11488127734756,
	-89.83837994333877,34.11002694744741, 
	-89.8348783061175,34.10749480699699,
	-89.83016873755433,34.10709019000608,
	-89.82355227879253,34.11144545569044,
	-89.82137732988944,34.11407856050789,
	-89.81884292728778,34.1172884085509,
	-89.81923001796407,34.12071762331159,
	-89.82193829928013,34.12433326164657,
	-89.8291043921992,34.12394040226167),
	ncol=2, byrow=TRUE)	

	# BILLIES	
xy2<-matrix(c(	
	-89.77289828816011,34.12873397365311,
	-89.76774994101028,34.12668478124235,
	-89.76266944254215,34.12628818013323,
	-89.75931113396815,34.12712965948358,
	-89.75832500358763,34.12898146027732,
	-89.75875790900869,34.13012770412038,
	-89.75956198837545,34.13306528303878,
	-89.76032752522802,34.13562525651049,
	-89.76099817993331,34.13790799169687,
	-89.76312565910443,34.13926482932697, 
	-89.76556160304746,34.14136708399852,
	-89.76617598985348,34.14324607146693,
	-89.76878432735586,34.14479967335202,
	-89.77492333746093,34.14640413252697,
	-89.77555592736375,34.14431798895685,
	-89.77643367333054,34.14200220172483,
	-89.77738235493442,34.13842937845001,
	-89.77747288929112,34.13526043231848,
	-89.77289828816011,34.12873397365311),
		ncol=2, byrow=TRUE)	
		
		
sp_poly2 <- SpatialPolygons(
	list(Polygons(list(Polygon(xy2)), ID=2)),
	proj4string=CRS("+proj=longlat +datum=NAD83"))
sp_poly1 <- SpatialPolygons(
	list(Polygons(list(Polygon(xy1)), ID=1)),
	proj4string=CRS("+proj=longlat +datum=NAD83"))	

xy2_pnts<-spsample(sp_poly2, n=100,type="regular")
plot(xy2_pnts, add=TRUE)
coords<- slot(xy2_pnts, "coords")


cama <- destPoint(coords,
	b=45,# bearing
	d=50)#distance in meters
camb<- destPoint(coords,
	b=225,# bearing
	d=50)#distance in meters
	
plot(sp_poly2)
plot(xy2_pnts, add=TRUE,pch=3,cex=0.25)
points(cama,col="red",pch=19,cex=0.5)
points(camb,col="red",pch=19,cex=0.5)
# PLOT CAPTURE FIELD
angle<- 45
distance<- 110
cama_bearing<- seq(225-angle*0.5,225+angle*0.5,length.out=20)
camb_bearing<- seq(45-angle*0.5,45+angle*0.5,length.out=20)

out<-list()
for(i in 1:nrow(cama))
	{
	tmp<- destPoint(cama[i,],b=cama_bearing,d=distance)
	out[[i]]<- data.frame(camera=i,
		side="a",
		lon=c(cama[i,1],tmp[,1],cama[i,1]),
		lat=c(cama[i,2],tmp[,2],cama[i,2]))
	}
for(i in 1:nrow(camb))
	{
	tmp<- destPoint(camb[i,],b=camb_bearing,d=distance)
	out[[i+nrow(camb)]]<- data.frame(camera=i,
		side="b",
		lon=c(camb[i,1],tmp[,1],camb[i,1]),
		lat=c(camb[i,2],tmp[,2],camb[i,2]))
	}
trans_black<- rgb(0,0,0,alpha=80,maxColorValue=255)
sapply(1:length(out), function(x)
	{
	tmp<- out[[x]]
	polygon(tmp$lon, tmp$lat,col=trans_black,border=trans_black)
	})
#


## MAKE CAMERA ZONES INTO SPATIAL POLYGONS
ppp<-SpatialPolygons(lapply(1:length(out),function(x)
	{
	tmp<- out[[x]]
	Polygons(list(Polygon(cbind(tmp$lon, tmp$lat))),
		unique(paste(tmp$camera,tmp$side,sep="-")))
	}),	proj4string=CRS("+proj=longlat +datum=NAD83"))
	 
	 

sp_poly1utm<-spTransform(sp_poly1, CRS(sr))
sp_poly2utm<-spTransform(sp_poly2, CRS(sr))	
ppputm<-spTransform(ppp, CRS(sr))	
		 
	 
# PUT CRITTERS ON PLOT
n_critters<- 1000
critters<-spsample(sp_poly2, n=n_critters,type="random")
xx<-over(critters, ppp)

plot(sp_poly2)
plot(ppp,add=TRUE)
plot(critters, add=TRUE,pch=3,cex=0.25)
plot(critters[which(is.na(xx)==FALSE),], 
	add=TRUE,pch=3,cex=0.25,col="red")

# TRUE DENSITY IN SQUARE METERS
plotArea<-areaPolygon(sp_poly2)
plotDensity<- n_critters/plotArea
##
camArea<-areaPolygon(ppp)



ex<- round(extent(sp_poly2utm))
x_seq<- seq(ex[1],ex[2],by=450)
y_seq<- seq(ex[3],ex[4],by=450)
xy <- expand.grid(x = x_seq, y = y_seq)
grid.pts<-SpatialPointsDataFrame(coords= xy, 
	data=xy, proj4string =CRS(sr))
gridded(grid.pts) <- TRUE
gridsp <- as(grid.pts, "SpatialPolygons")
grid <- SpatialPolygonsDataFrame(gridsp, 
	data=data.frame(id=row.names(gridsp), 
	row.names=row.names(gridsp)))
camera_grid <- intersect(sp_poly2utm, grid)
centroid<- coordinates(camera_grid)# centroid of cell
plot(camera_grid)
names.grd<-sapply(camera_grid@polygons, function(x) slot(x,"ID"))
text(coordinates(camera_grid), 
	labels=sapply(slot(camera_grid, "polygons"), function(i) slot(i, "ID")), cex=0.3)


