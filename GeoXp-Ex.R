pkgname <- "GeoXp"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('GeoXp')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("angleplotmap")
### * angleplotmap

flush(stderr()); flush(stdout())

### Name: angleplotmap
### Title: Detection of an eventual directional trend
### Aliases: angleplotmap
### Keywords: spatial

### ** Examples

######
# data Conflicts Africa
data(afcon)

# afcon is a data.frame object. We have to create
# a Spatial object, by using first the longitude and latitude
# to create Spatial Points object ...
afcon.sp = SpatialPoints(cbind(afcon$x,afcon$y))
# ... and then by integrating other variables to create SpatialPointsDataFrame
afcon.spdf = SpatialPointsDataFrame(afcon.sp, afcon)
# For more details, see vignette('sp', package="sp")

# optional : we add some contours that don't correspond to the spatial unit
# but are nice for mapping
africa <- readShapePoly(system.file("shapes/Africa.shp", package = "GeoXp")[1])
africa.contour<-spdf2list(africa)$poly

# A basic call of histomap function
angleplotmap(afcon.spdf,"totcon", carte= africa.contour,
identify=TRUE, cex.lab=0.6)


#####
# Data Meuse
data(meuse)

# meuse is a data.frame object. We have to create
# a Spatial object, by using first the longitude and latitude
# to create Spatial Points object ...
meuse.sp = SpatialPoints(cbind(meuse$x,meuse$y))
# ... and then by integrating other variables to create SpatialPointsDataFrame
meuse.spdf = SpatialPointsDataFrame(meuse.sp, meuse)

# meuse.riv is used for contour plot
data(meuse.riv)

angleplotmap(meuse.spdf,"copper",
col="green",quantiles=0.9, cex.lab=0.7,
xlab="Concentration in plomb (in ppm)",pch=7,carte=meuse.riv[c(21:65,110:153),])




cleanEx()
nameEx("barmap")
### * barmap

flush(stderr()); flush(stdout())

### Name: barmap
### Title: Interactive Bar plot and map
### Aliases: barmap
### Keywords: spatial univar

### ** Examples

######
# data eire
eire <- readShapePoly(system.file("etc/shapes/eire.shp", package="spdep")[1],
ID="names", proj4string=CRS("+proj=utm +zone=30 +units=km"))

# a basic usage ...
barmap(eire,"pale")

# ... with all options
barmap(eire,3, type = "percent",col=c("pink","orange"),
names.arg=c("not pale","pale"), names.attr=names(eire), 
criteria=NULL, identify=TRUE, cex.lab=0.8, pch=c(10,11), 
xlab="Are majority people are pale ?", ylab="Percent", 
axes=TRUE, lablong="longitude", lablat="latitude")




cleanEx()
nameEx("barnbmap")
### * barnbmap

flush(stderr()); flush(stdout())

### Name: barnbmap
### Title: Bar plot of the number of neighbour and map
### Aliases: barnbmap
### Keywords: spatial

### ** Examples

######
# data on price indices of real estate in France
data(immob)
row.names(immob)<-immob$Nom

# immob is a data.frame object. We have to create
# a Spatial object, by using first the longitude and latitude
# to create Spatial Points object ...
immob.sp = SpatialPoints(cbind(immob$longitude,immob$latitude))

# Spatial weight matrix based on nearest neighbours
immob.nb <- dnearneigh(coordinates(immob.sp), 0,175000)

# a simple use of barnbmap
barnbmap(immob.sp,immob.nb)

######
# Data columbus
example(columbus)

# col.gal.nb is a spatial weight matrix included in spdep package...
barnbmap(columbus,col.gal.nb,criteria=(columbus$EW==1),
col=colors()[98:106], identify=TRUE, cex.lab=0.7, pch=1:9)



cleanEx()
nameEx("boxplotmap")
### * boxplotmap

flush(stderr()); flush(stdout())

### Name: boxplotmap
### Title: Interactive boxplot and map
### Aliases: boxplotmap
### Keywords: spatial univar

### ** Examples

######
# data columbus
example(columbus)

# a basic use of boxplotmap
boxplotmap(columbus,"CRIME", criteria=(columbus@data$CP==1),
xlab="Crime",ylab="Percent",identify=TRUE, cex.lab=0.7)

######
# data boston
data(boston)

# creation of a Spatial object
boston.sp = SpatialPoints(cbind(boston.c$LON,boston.c$LAT))
# ... and then by integrating other variables to create SpatialPointsDataFrame
boston.spdf = SpatialPointsDataFrame(boston.sp, boston.c)

# a simple use of boxplotmap
boxplotmap(boston.spdf,"MEDV",criteria=(boston.c$CHAS==1))



cleanEx()
nameEx("clustermap")
### * clustermap

flush(stderr()); flush(stdout())

### Name: clustermap
### Title: Classification of dataset using kmeans or hclust algorithm and
###   representation of clusters on a map.
### Aliases: clustermap
### Keywords: spatial multivariate

### ** Examples

#####
# data columbus
example(columbus)

# a basic example using the kmeans method
clustermap(columbus, c("HOVAL","INC","CRIME","OPEN","PLUMB","DISCBD"), 3,
criteria=(columbus@data$CP==1), identify=TRUE, cex.lab=0.7)

# example using the hclust method
clustermap(columbus,c(7:12), 3, method="hclust",
criteria=(columbus@data$CP==1),col=colors()[20:22],identify=TRUE,
cex.lab=0.7, names.arg=c("Group 1","Group 2","Group 3"), xlab="Cluster")



cleanEx()
nameEx("dbledensitymap")
### * dbledensitymap

flush(stderr()); flush(stdout())

### Name: dbledensitymap
### Title: Double Kernel density estimates and map
### Aliases: dbledensitymap
### Keywords: spatial smooth

### ** Examples


#########
# data auckland
data(auckland)

# creation of a Spatial object
auckland.sp = SpatialPoints(cbind(auckland$Easting,auckland$Northing))
# ... and then by integrating other variables to create SpatialPointsDataFrame
auckland.spdf = SpatialPointsDataFrame(auckland.sp, auckland)
# For more details, see vignette('sp', package="sp")

# optional : we add some contours that don't correspond to the spatial unit
# but are nice for mapping
contours.auckland<-polylist2list(auckpolys)

dbledensitymap(auckland.spdf, c("Deaths.1977.85","Under.5.1981"),carte=contours.auckland,
xlab=c("Deaths.1977.85","Under.5.1981"),
criteria=(auckland$Deaths.1977.85>mean(auckland$Deaths.1977.85)))


######
# data eire
eire <- readShapePoly(system.file("etc/shapes/eire.shp", package="spdep")[1],
ID="names", proj4string=CRS("+proj=utm +zone=30 +units=km"))

dbledensitymap(eire,c("A","towns"),kernel="normal",
xlab=c("Individuals rate of blood type A",
"Surface urbaine"),identify=TRUE)




cleanEx()
nameEx("dblehistomap")
### * dblehistomap

flush(stderr()); flush(stdout())

### Name: dblehistomap
### Title: Double Interactive Histogram and map
### Aliases: dblehistomap
### Keywords: spatial multivariate

### ** Examples

######
# data on price indices of real estate in France
data(immob)
row.names(immob)<-immob$Nom

# immob is a data.frame object. We have to create
# a Spatial object, by using first the longitude and latitude
# to create Spatial Points object ...
immob.sp = SpatialPoints(cbind(immob$longitude,immob$latitude))
# ... and then by integrating other variables to create SpatialPointsDataFrame
immob.spdf = SpatialPointsDataFrame(immob.sp, immob)
# For more details, see vignette('sp', package="sp")

# optional : we add some contours that don't correspond to the spatial unit
# but are nice for mapping
midiP <- readShapePoly(system.file("shapes/region.shp", package="GeoXp")[1])
cont_midiP<-spdf2list(midiP[-c(22,23),])$poly

# A basic call of dblehistomap function
dblehistomap(immob.spdf,c("prix.vente","prix.location"),
carte= cont_midiP, identify=TRUE, cex.lab=0.6)

###
# data colombus
x <- readShapePoly(system.file("shapes/columbus.shp", package="maptools")[1])

# example of use with many options
dblehistomap(x,c("HOVAL","CRIME"), nbcol=c(5,10),type="percent",
xlab=c("hoval","crime"),identify=TRUE, cex.lab=0.7, ylab=c("percent","percent"),
col=c("pink","orange"),pch=14,axes=TRUE)




cleanEx()
nameEx("densitymap")
### * densitymap

flush(stderr()); flush(stdout())

### Name: densitymap
### Title: Kernel density estimates and map
### Aliases: densitymap
### Keywords: spatial smooth univar multivariate

### ** Examples


######
# data oldcol
example(columbus)

# columbus is included in the Spatial-Class object
# a very simple use of histomap :
densitymap(columbus,"CRIME")

######
# data on price indices of real estate in France
data(immob)
row.names(immob)<-immob$Nom

# immob is a data.frame object. We have to create
# a Spatial object, by using first the longitude and latitude
# to create Spatial Points object ...
immob.sp = SpatialPoints(cbind(immob$longitude,immob$latitude))
# ... and then by integrating other variables to create SpatialPointsDataFrame
immob.spdf = SpatialPointsDataFrame(immob.sp, immob)
# For more details, see vignette('sp', package="sp")

# optional : we add some contours that don't correspond to the spatial unit
# but are nice for mapping
midiP <- readShapePoly(system.file("shapes/region.shp", package="GeoXp")[1])
cont_midiP<-spdf2list(midiP[-c(22,23),])$poly

# A basic call of densitymap function
densitymap(immob.spdf,"prix.vente", carte= cont_midiP, identify=TRUE,
xlab="housing price by square meter", cex.lab=0.6)




cleanEx()
nameEx("driftmap")
### * driftmap

flush(stderr()); flush(stdout())

### Name: driftmap
### Title: Interactive driftplot and map
### Aliases: driftmap
### Keywords: spatial

### ** Examples

######
# data on price indices of real estate in France
data(immob)
row.names(immob)<-immob$Nom

# immob is a data.frame object. We have to create
# a Spatial object, by using first the longitude and latitude
# to create Spatial Points object ...
immob.sp = SpatialPoints(cbind(immob$longitude,immob$latitude))
# ... and then by integrating other variables to create SpatialPointsDataFrame
immob.spdf = SpatialPointsDataFrame(immob.sp, immob)
# For more details, see vignette('sp', package="sp")

# optional : we add some contours that don't correspond to the spatial unit
# but are nice for mapping
midiP <- readShapePoly(system.file("shapes/region.shp", package="GeoXp")[1])
cont_midiP<-spdf2list(midiP[-c(22,23),])$poly

# a simple use of driftmap
driftmap(immob.spdf,"prix.vente", carte= cont_midiP)

# ... with options
driftmap(immob.spdf,"prix.vente", carte= cont_midiP,
interpol=FALSE, nuage=TRUE, lty=3:4,
identify=TRUE, cex.lab=0.3, xlab="Price of sell", axes=TRUE)

######
# data eire
eire <- readShapePoly(system.file("etc/shapes/eire.shp", package="spdep")[1],
ID="names", proj4string=CRS("+proj=utm +zone=30 +units=km"))

# a basic usage ...
driftmap(eire,"pale",identify=TRUE,nuage=TRUE)



cleanEx()
nameEx("ginimap")
### * ginimap

flush(stderr()); flush(stdout())

### Name: ginimap
### Title: Lorentz curve and map
### Aliases: ginimap
### Keywords: spatial univar

### ** Examples

######
# data eire
eire <- readShapePoly(system.file("etc/shapes/eire.shp", package="spdep")[1],
ID="names", proj4string=CRS("+proj=utm +zone=30 +units=km"))

# a basic usage ...
ginimap(eire,"INCOME")

# ... with options
ginimap(eire,"INCOME",criteria=(eire$pale==1),
identify=TRUE, pch=5, col="orange",
axes=TRUE, lablong="X", lablat="Y")



cleanEx()
nameEx("histnbmap")
### * histnbmap

flush(stderr()); flush(stdout())

### Name: histnbmap
### Title: Interactive histogram of the distances between two neighbors of
###   a nb object and map
### Aliases: histnbmap
### Keywords: spatial

### ** Examples

##
# data columbus
example(columbus)

# a simple use of histnbmap
histnbmap(columbus, col.gal.nb, criteria=(columbus$CP==1),
xlab="distance of the neighbor the farest")

##
# data meuse
data(meuse)

# meuse is a data.frame object. We have to create
# a Spatial object, by using first the longitude and latitude
# to create Spatial Points object ...
meuse.sp = SpatialPoints(cbind(meuse$x,meuse$y))
# ... and then by integrating other variables to create SpatialPointsDataFrame
meuse.spdf = SpatialPointsDataFrame(meuse.sp, meuse)

# meuse.riv is used for contour plot
data(meuse.riv)

# creation of a spatial weight matrix (class nb) based
# on the Delaunay triangulation
meuse.nb <- tri2nb(coordinates(meuse.sp))

# a example with some optionswhich shows the limit of
# this kind of spatial weight matrix
histnbmap(meuse.spdf, meuse.nb, sup=TRUE, nbcol=7,
carte=meuse.riv[c(21:65,110:153),])




cleanEx()
nameEx("histobarmap")
### * histobarmap

flush(stderr()); flush(stdout())

### Name: histobarmap
### Title: Histogram, barplot and map
### Aliases: histobarmap
### Keywords: spatial multivariate

### ** Examples

###
# Data Colombus
example(columbus)

# an example of use
histobarmap(columbus,c("CP","HOVAL"),nbcol=8, type="percent",
names.arg=c("A","B"), xlab=c("CP","Hoval"), ylab=rep("percent",2))

######
# data eire
eire <- readShapePoly(system.file("etc/shapes/eire.shp", package="spdep")[1],
ID="names", proj4string=CRS("+proj=utm +zone=30 +units=km"))

# example of use
histobarmap(eire, c("pale","A"),names.arg=c("Ouside Pale","Pale"),
xlab=c("Appartenance to the region of Pale","Average number of people with blood A"),
col=colors()[101:102],identify=TRUE)



cleanEx()
nameEx("histomap")
### * histomap

flush(stderr()); flush(stdout())

### Name: histomap
### Title: Interactive Histogram and map
### Aliases: histomap
### Keywords: spatial univar

### ** Examples


######
# data oldcol
example(columbus)

# columbus is included in the Spatial-Class object
# a very simple use of histomap :
histomap(columbus,"CRIME")

######
# data on price indices of real estate in France
data(immob)

# immob is a data.frame object. We have to create
# a Spatial object, by using first the longitude and latitude
# to create Spatial Points object ...
immob.sp = SpatialPoints(cbind(immob$longitude,immob$latitude))
# ... and then by integrating other variables to create SpatialPointsDataFrame
immob.spdf = SpatialPointsDataFrame(immob.sp, immob)
# we just give names to the spatial units...
row.names(immob.spdf)<-immob$Nom
# For more details, see vignette('sp', package="sp")

# optional : we add some contours that don't correspond to the spatial unit
# but are nice for mapping
midiP <- readShapePoly(system.file("shapes/region.shp", package="GeoXp")[1])
cont_midiP<-spdf2list(midiP[-c(22,23),])$poly

# A basic call of histomap function
histomap(immob.spdf,"prix.vente", carte= cont_midiP, identify=TRUE, cex.lab=0.6)

# ... with all options
histomap(immob.spdf,7, nbcol=15, type = "percent",
names.attr=names(immob), criteria=immob$rentabilite>5, carte=cont_midiP,
identify=TRUE, cex.lab=0.5, pch=12, col="pink",
xlab="variation price", ylab="percent", axes=TRUE, lablong="x",
lablat="y")



cleanEx()
nameEx("immob")
### * immob

flush(stderr()); flush(stdout())

### Name: immob
### Title: Some price indices of real estate from biggest cities in France
### Aliases: immob
### Keywords: datasets

### ** Examples

data(immob)



cleanEx()
nameEx("makeneighborsw")
### * makeneighborsw

flush(stderr()); flush(stdout())

### Name: makeneighborsw
### Title: Spatial weight matrix
### Aliases: makeneighborsw
### Keywords: spatial

### ** Examples

##
# data auckland
data(auckland)
coords <- cbind(auckland$Easting[1:10],auckland$Northing[1:10])

# matrix based on 5 nearest neighbors
W<-makeneighborsw(coords,method="neighbor",m=3)

# matrix based on a threshold distance
W1<-makeneighborsw(coords,method="distance",d=20)

# matrix based on the two methods
W2<-makeneighborsw(coords,method="both",m=3,d=20)

# representation of the 3 spatial weight matrices
op<-par(mfrow=c(2,2))
plot(mat2listw(W),coords,col="lightblue1",main="neighbor")
plot(mat2listw(W1),coords,col="lightblue2",main="distance")
plot(mat2listw(W2),coords,col="lightblue3",main="both")
par(op)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("misolationmap")
### * misolationmap

flush(stderr()); flush(stdout())

### Name: misolationmap
### Title: Interactive multivariate isolation plot and map
### Aliases: misolationmap
### Keywords: spatial multivariate

### ** Examples

## data kola
data(moss)
data(kola.background)

# transformation of some variables
moss[,c("Ag","As","Bi","Cd","Co","Cu","Ni")]<-log10(moss[, c("Ag","As","Bi","Cd","Co","Cu","Ni")])

# creation of a SpatialClass object
kola.sp <- SpatialPoints(moss[,c("XCOO","YCOO")])
kola.spdf <- SpatialPointsDataFrame(kola.sp, moss)

# creation of a spatial weight matrix nb
kola.nb <- dnearneigh(kola.sp, 0,50000 )

# example of use of misolationmap
# The statistics are calculated by taking into account variables
# Ag,As,Bi,Cd,Co,Cu,Ni
misolationmap(kola.spdf,kola.nb,names.var=c("Ag","As","Bi","Cd","Co","Cu","Ni"),
propneighb=0.30,chisqqu=0.95, carte= kola.background,identify=TRUE, cex.lab=0.5)




cleanEx()
nameEx("moranplotmap")
### * moranplotmap

flush(stderr()); flush(stdout())

### Name: moranplotmap
### Title: Moran scatterplot and map
### Aliases: moranplotmap
### Keywords: spatial

### ** Examples

##
# data baltimore
data(baltimore)

# SpatialPoints
baltimore.sp <- SpatialPoints(cbind(baltimore$X,baltimore$Y))
# SpatialPointsDataFrame
baltimore.spdf<-SpatialPointsDataFrame(baltimore.sp,baltimore)

# Spatial Weight Matrix
W.nb <- knn2nb(knearneigh(baltimore.sp, k=4))
# We choose a row standardized spatial weight matrix :
W.listw <- nb2listw(W.nb,style="W")


# moranplotmap with some options
moranplotmap(baltimore.spdf, "PRICE", W.listw ,
flower=TRUE, locmoran=TRUE,criteria=(baltimore.spdf$AC==1),
identify=TRUE)

# comparison with the moran.test function
moran.test(baltimore.spdf$PRICE,W.listw)
 
##
# data columbus
example(columbus)

# use of moranplotmap with spatial weight matrix col.gal.nb :
# 1. row-standardized
moranplotmap(columbus,"HOVAL",nb2listw(col.gal.nb,style="W"))

# 2. basic binary
moranplotmap(columbus,"HOVAL",nb2listw(col.gal.nb,style="B"))