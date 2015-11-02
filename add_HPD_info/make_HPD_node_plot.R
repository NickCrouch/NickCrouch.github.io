
make_HPD_node_plot <- function(phy, data, line.width=5, line.col="purple", node.col = "red", node.cex=2, node.pch=19, ladderize=FALSE, geo.units=c("Epoch","Period")){

require(phyloch)
require(ape)
require(phytools)
require(strap)

# identify locations of columns in the supplied data
min.column <- grep("min", colnames(data), ignore.case=TRUE)
max.column <- grep("max", colnames(data), ignore.case=TRUE)
node.column <- grep("node", colnames(data), ignore.case=TRUE)


# Age data supplied by user needs to be corrected by: maximum age of phylogeny - specified age
# to accomodate how R plots the data

max.tree.height <- max(nodeHeights(phy))

data[,min.column] <- max.tree.height - data[,min.column]
data[,max.column] <- max.tree.height - data[,max.column]


# Ladderize tree?
if(ladderize==TRUE){

phy <- ladderize(phy)

}

phy$root.time <- max.tree.height

geoscalePhylo(phy,  units=geo.units, cex.age=1.25, cex.ts=1.1, x.lim=c(0,max.tree.height+5), quat.rm = T)

lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)

nodes <- data[,node.column]

y.coords <- lastPP$yy[nodes]

for(i in 1:nrow(data)){

segments(x0=data[i,min.column], x1=data[i,max.column], y0=y.coords[i], y1=y.coords[i], lwd=line.width, col=line.col)

}

abline(v=(max.tree.height- 66), lwd=2, lty=2)

nodelabels(node=data[,node.column], pch=node.pch, cex=node.cex, bg=node.col)

}

######

find.nodes <- function(data, phy, spp_group_list, colname="Order" ){

require(phytools)

data$Node <- NA

for(i in 1:nrow(data)){

taxa <- as.character(data[i,colname])


if((taxa %in% names(spp_group_list)) == TRUE){

where <- grep(taxa, names(spp_group_list))

node <- findMRCA(phy, spp_group_list[[where]], type="node")

data[i,"Node"] <- node

}

}

return(data)

}
