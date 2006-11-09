##
## loading specific library to process images
library(rgdal)
library(pixmap)
par(mfrow=c(2,2))

##
## open sample image and get raster data
image1 <- GDAL.open("star1.png") 
image1.description <- getDescription(image1)
image1.driver <- getDriverLongName(getDriver(image1))
image1.dim <- dim(image1)
image1.metadata <- getMetadata(image1)
image1.raster <- getRasterData(image1, band = 1, offset = c(0, 0),
dim(image1), dim(image1), interleave = c(0, 0), as.is = FALSE)
image1.raster.flat <- t(getRasterData(image1, band=1))

##
## plot image and raster density
displayDataset(image1, band=1, reset = FALSE)
plot(density(image1.raster))

##
## close image
GDAL.close(image1)

##
##
## MOVE 1
plot(pixmapGrey(image1.raster.flat))
raster.beta <- 2.5
raster.bigK <- 200
raster.temperature <- 6.0
raster.image <- image1.raster.flat
raster.labels<- as.mtrix()
raster.labels_num <- 2
raster.weights <- as.vector(rep(0,raster.l))
raster.mus <- as.matrix(cbind( rep(0.2, raster.l) , rep(0.7, raster.l)))
matr.sigmas <- c(0.05, 0.00001, 0.05, 0.00001)
raster.sigma <-cbind( c(matr.sigma, matr.sigma))
