
add_HPD_info <- function(phy, data){

require(phyloch)
require(ape)

# need number of nodes
num.nodes <- phy$Nnode

# identify locations of columns in the supplied data
min.column <- grep("min", colnames(data), ignore.case=TRUE)
max.column <- grep("max", colnames(data), ignore.case=TRUE)
node.column <- grep("node", colnames(data), ignore.case=TRUE)


# generate vectors to put into phylogeny structure for height mins and maxs
# length equal to number of nodes, named by the names of the nodes as seen in object of class 'phylo'
min.vals <- vector(mode="numeric", length=num.nodes)
names(min.vals) <- seq((num.nodes+2), ((num.nodes*2)+1), 1)
max.vals <- vector(mode="numeric", length=num.nodes)
names(max.vals) <- seq((num.nodes+2), ((num.nodes*2)+1), 1)

# add values into vectors
for(i in 1:nrow(data)){

node <- data[i,node.column]

min.vals[grep(node, names(min.vals))] <- data[i,min.column]
max.vals[grep(node, names(max.vals))] <- data[i,max.column]


}


# find which nodes have no data
all.nodes <- data[,node.column]
v <- names(min.vals) %in% all.nodes

min.vals[v==FALSE] <- NA
max.vals[v==FALSE] <- NA

# convoluted procedure for adding in data with correct names
n <- length(phy)

phy$min <- as.numeric(min.vals)

min.txt <- "height_95%_HPD_MIN"

names(phy)[n+1] <- min.txt

phy$max <- as.numeric(max.vals)

names(phy)[n+2] <- "height_95%_HPD_MAX"

return(phy)

}


