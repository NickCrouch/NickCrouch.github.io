

boxplot.by.node <- function(phy, dat, label.nodes=TRUE, show.tip.labels=TRUE, node.cols, color.boxplot=FALSE, ...){
  
  par(mfrow=c(1,2))
  
  # nodes
  node.nums <- sapply(colnames(dat), .get.node.numbers.from.col.names)
  
  # find where those nodes are in the plot
  plot(phy, show.tip.label = show.tip.labels)
  if(label.nodes==TRUE){
    nodelabels(node=node.nums, pch=21, bg=node.cols, cex=1.5)
  } 
  
  # get the order of the nodes top to bottom
  plot.obj<-get("last_plot.phylo",envir=.PlotPhyloEnv)
  
  y.pos <- plot.obj$yy[node.nums]
  y.order <- order(y.pos) 
   
  dat.sort <- dat[,y.order]
  
  
  if(length(node.cols)>1){
    node.nums.sort <- node.nums[y.order]
    nodelabels(node=node.nums.sort, pch=21, bg=node.cols, cex=1.5)
  }
  
  # generate boxplot
  if(color.boxplot==FALSE){
  boxplot(dat.sort, horizontal = TRUE, col="white", ...)
  } else {
  boxplot(dat.sort, horizontal = TRUE, col=node.cols, ...)
    
  }
}



.get.node.numbers.from.col.names <- function(colname){
  spl <- strsplit(colname, "_")[[1]][2]
  num <- as.numeric(spl)
  return(num)
}