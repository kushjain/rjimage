##
## Takes mu and sigma vectors as gaussians for 
## classical image segmentation along with raster
## that is (MxN) matrix that contains pixel intensity
## returns (MxN) matrix of labels
segmentImage <- function(mu, sigma, raster){
 ## extract useful data at first
 size_x <-dim(raster)[1]
 size_y <-dim(raster)[2]
 pixel_num <- size_x*size_y
 labels_dim <- length(mu)
 ## initialize segmentation
 segmentation <- matrix(rep(0,pixel_num),nrow=size_x, ncol=size_y, byrow=TRUE)
 segmentation_t <- segmentation
 for(i in 1:labels_dim){
  c_mu <- mu[i]
  c_s <- sigma[i]
  segmentation_draft <- dnorm(raster, c_mu, c_s)
  segmentation[segmentation_t[,] < segmentation_draft[,]] <- i
  for(k in 1:size_x){
   for(l in 1:size_y){
    if(segmentation_t[k,l] < segmentation_draft[k,l]) segmentation_t[k,l] <- segmentation_draft[k,l]
   }
  }
 }
 segmentation
}
