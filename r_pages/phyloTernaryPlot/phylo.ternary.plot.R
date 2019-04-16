


phylo.ternary.plot <- function(source.phy, targetPhy1, targetPhy2, targetPhy3, names){
  
  require(tricolore)
  require(ape)
  require(dplyr)
  require(grid)
  require(gridExtra)
  require(ggplot2)
  
  edge.df <- cbind(targetPhy1$edge.length, targetPhy2$edge.length, targetPhy3$edge.length)
  edge.df <- scale(edge.df)

  # make values positive
  edge.df <- apply(edge.df, 2, .make.pos) %>% as.data.frame
  colnames(edge.df) <- names
  
  colors_and_legend <- Tricolore(edge.df, names[1], names[2], names[3],breaks = Inf)
  
  # plot(source.phy, edge.color = colors_and_legend$rgb, edge.width = 2)
  
  edge=data.frame(source.phy$edge, edge_num=1:nrow(source.phy$edge))
  colnames(edge)=c("parent", "node", "edge_num")
  edge$color <- colors_and_legend$rgb

  p1 <- ggtree(source.phy) + geom_tiplab(size=5) + ggplot2::xlim(0, 90)
  p1 <- p1 %<+% edge + aes(color=I(color))

res <- vector(mode="list", length=2)
res[[1]] <- p1
res[[2]] <- colors_and_legend$key

#  grid.newpage()
#  grid.draw(cbind(ggplotGrob(p1), ggplotGrob(colors_and_legend$key), size = "last"))
  
#  vp <- viewport(width = 90*0.25, height = 90*0.25, x = 5, y = 90)
#  print(p1)
#  print(colors_and_legend$key, vp = vp)
  
  return(res)
}


.make.pos <- function(x){
  require(dplyr)
  mn <- x %>% min %>% abs
  x <- x + mn + 0.01
  return(x)
}



