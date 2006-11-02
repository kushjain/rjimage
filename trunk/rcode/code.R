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
#image1.raster.flat[image1.raster.flat <40] <- 1
#image1.raster.flat[image1.raster.flat >210] <- 3
x <- pixmapRGB(image1.raster.flat)
plot(t(t(x)))

?rnorm