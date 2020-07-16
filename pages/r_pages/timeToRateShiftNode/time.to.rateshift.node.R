

time.to.rateshift.node <- function(getBestShiftConfiguration.result, phy){

node.data <- getBestShiftConfiguration.result$eventData[[1]]

# Total amount of time between shift and node
time.diffs <- vector(length=nrow(node.data), mode="numeric")
for(i in 1:nrow(node.data)){
time.diffs[i] <- .single.age.diff(node.data$node[i], node.data$time[i], phy)
}

# Proportion of branch leading to node
parent.edge.lengths <- unlist(sapply(node.data$node, .get.parent.branch.length, phy))

prop.parent <- time.diffs / parent.edge.lengths

# output

output <- as.data.frame(matrix(NA, ncol=3, nrow=nrow(node.data)))
colnames(output) <-c("index", "time.before.node", "proportion.parent.branch.length")

output[,1] <- seq(1, nrow(node.data), 1)
output[,2] <- time.diffs
output[,3] <- prop.parent

return(output)

}


##

.single.age.diff <- function(node, time, phy){
require(phytools)
node.height <- nodeheight(phy, node)
time.diff <- abs(time - node.height)
return(time.diff)
}



##

.get.parent.branch.length <- function(node, phy){
root <- Ntip(phy) + 1
if(node == root){
parent.edge.length <- NA
} else {
v <- phy$edge[,2] == node
parent.edge.length <- phy$edge.length[v==TRUE]
}
return(parent.edge.length)
}
