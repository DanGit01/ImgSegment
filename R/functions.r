
#' Calculate shannon index
#'
#' @param species vector of species abundances
#'
#' @return shannon index
#' @export
#'
#'  function(species)
#'  shannon(species)
#'
#' @examples
shannon <- function(species)
{
  prop <- species/sum(species)
  s <- - sum(prop * log(prop))
  return (s)
}



#' Calculate length vector species
#'
#' @param species 
#'
#' @return
#' @export
#'
#' @examples
rtchness <- function(species)
{
  return (length(species))
}





buildpathFileValid <- function(pathFile, nameFileImag){
  
  posic <- 1
  existe <- FALSE
  listFile <- dir(path = pathFile)
  
  if (length(listFile) > 0){

     for (indi in 1:length(listFile)) {
       if (nameFileImag == listFile[indi]){
         posic <- indi
         existe <- TRUE
       }
     }
  }


  if (existe)
    pathFile <- paste(pathFile,listFile[posic],sep='/')
  else{
    pathFile <- ''
    warning("The file don't exists")
  }
  
  return(pathFile)

}



loadImagSelect <- function(newPathFile, pathFile){
  
  if(nchar(newPathFile)>nchar(pathFile))
    image01 <- imager::load.image(newPathFile)
  else{
    warning("the path is wrong")
    return(NULL)    
  }

  # numChannel <- dim(image01)[4]
  
  imag01 <- imager::grayscale(image01, method = "Luma", drop = TRUE)
  return(image01)
}
    



