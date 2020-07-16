

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


#############

is.unequal <- function(x){
  require(dplyr)
  x <- na.omit(x) %>% as.character
 ll <-  x %>% unique %>% length
 return(ll > 1)

}

#############

plot.num.supporting.chars <- function(phy, alignment, show.tip.label=FALSE, viridis.col="D",
                                      node.pch=15, node.cex=2){
  
  require(viridis)
  require(phytools)
  require(kableExtra)
  require(raster)
  nodes <- seq(from=Ntip(phy)+1, length.out = Nnode(phy))
  all.node.vals <- sapply(nodes, run.supp.chars, alignment=alignment, phy=phy) %>% unlist
  cols <- spec_color(all.node.vals, option = viridis.col)
 
  
  # legend
  mn <- min(all.node.vals); mx <- max(all.node.vals)
  s <- seq(mn, mx, length.out = 100)
  legend <- matrix(spec_color(s,option = viridis.col), ncol=1) %>% as.raster

  ### plotting ###
  
  l <-  matrix(c(1,1,2),ncol=3,nrow=1)
  layout(l)
  #par(mfrow=c(1,2))
  par(mai=rep(0.1,4))
  
  plot(phy, show.tip.label = show.tip.label)
  nodelabels(node=nodes, pch=node.pch, cex=node.cex, col=cols)
  
  plot(c(0,.9),c(0,mx),type = 'n', axes = F,xlab = '', ylab = '', main = "Number of supporting characters", xlim=c(0,.2))
  rasterImage(legend, 0, 0, .08,mx)
  text(x=0.11, y = seq(0,mx,l=5), labels = seq(0,mx,l=5))
  
  
}



#############

run.supp.chars <- function(node, alignment,  phy){
  
  all.d <- getDescendants(tree=phy, node =node )
  
  all.d <- all.d[all.d<=Ntip(phy)]
  
  spp <- phy$tip.label[all.d]
  
  val <- characters.supporting.clade(species=spp, alignment=alignment)[[1]]
  return(val)
}

