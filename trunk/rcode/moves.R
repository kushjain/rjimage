##
## Takes mu and sigma vectors as gaussians for 
## classical image segmentation along with raster
## that is (MxN) matrix that contains pixel intensity
## returns (MxN) matrix of labels
##
segmentImage <- function(classes, raster){
 ## extract useful data at first
 size_x <-dim(raster)[1]
 size_y <-dim(raster)[2]
 pixel_num <- size_x*size_y
 labels_dim <- dim(classes)[1]
 ## initialize segmentation
 segmentation <- matrix(rep(0,pixel_num),nrow=size_x, ncol=size_y, byrow=TRUE)
 segmentation_t <- segmentation
 for(i in 1:labels_dim){
  c_mu <- classes[i][1]
  c_s <- classes[i][2]
  segmentation_draft <- dnorm(raster, c_mu, c_s)
  segmentation[segmentation_t[,] < segmentation_draft[,]] <- i
  for(k in 1:size_x){
   for(l in 1:size_y){
    if(segmentation_t[k,l] < segmentation_draft[k,l]) {
      segmentation_t[k,l] <- segmentation_draft[k,l]
    }
   }
  }
 }
 segmentation
}

singleton <- function(raster_value, mean, variance)
{
  log(sqrt(2.0*3.141592653589793*variance))+
    ((raster_value-mean)^2)/(2.0*variance)
}


doubleton <- function(i, j, label, raster, labels, beta)
{
  energy <- 0.0;
  height <- dim(raster)[1]
  widtht <- dim(raster)[2]

  if (i!=height-1) # south
    {
      if (label == labels[i+1][j]) energy <- energy - beta
      else energy <- energy + beta
    }
  if (j!=width-1) # east
    {
      if (label == labels[i][j+1]) energy <- energy - beta
      else energy <- energy + beta
    }
  if (i!=0) # nord
    {
      if (label == labels[i-1][j]) energy <- energy - beta
      else energy <- energy + beta
    }
  if (j!=0) # west
    {
      if (label == labels[i][j-1]) energy <- energy - beta
      else energy <- energy + beta
    }
  energy;
}

local_energy <- function(i, j, raster, labels, classes, label)
{
  ## singleton singleton <- function(raster_value, mean, variance)
  ## doubleton doubleton <- function(i, j, label, raster, labels, beta)
  singleton(raster[i][j], classes[label][1], classes[label][2]) + doubleton(i,j,label,raster,labels,beta)
}

energy <- function(raster, labels, classes, beta)
{
  singletons <- 0.0;
  doubletons <- 0.0;
  height <- dim(raster)[1]
  widtht <- dim(raster)[2]
  
  for (i in 1:height){
    for (j in 1:width){
	  k <- labels[i][j]
	  ## singleton singleton <- function(raster_value, mean, variance)
	  singletons <- singletons + singleton(raster[i][j], classes[k][1], classes[k][2])
	  ## doubleton doubleton <- function(i, j, label, raster, labels, beta)
	  doubletons <- doubletons + doubleton(i,j,k,raster,labels, beta)
      }
  }    
  singletons + doubletons / 2
}
