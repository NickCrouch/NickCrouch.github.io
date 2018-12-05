

characters.supporting.clade <- function(species, alignment){
  
  matrix <- alignment$Matrix_1$Matrix
  
  for(i in 1:length(species)){
    test <- species[i] %in% rownames(matrix)
    if(test==FALSE){stop(paste(species[i],"is not in the matrix"))}
  }
  
  sub.matrix <- matrix[rownames(matrix) %in% species,]
  
  is.unequal.res <- apply(sub.matrix, 2, is.unequal)
  
  tb <- table(is.unequal.res)
  
  # Number of supporting chars is where unequal == FALSE
  n.supp <- tb[names(tb)==FALSE] %>% as.numeric
  
  output <- vector(mode = "list", length=2)
  names(output) <- c("Number of supporting chars", "supporting chars")
  output[[1]] <- n.supp
  output[[2]] <- grep("FALSE", is.unequal.res)
  return(output)
  
}


is.unequal <- function(x){
  require(dplyr)
 ll <-  x %>% unique %>% length
 return(ll > 1)

}
