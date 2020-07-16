
# face-value extinction rate is calculated as XL/Xtot, where XL is the number of last
# appearances and Xtot is the total observed diversity in the stage. 


# bin.age in results is midpoint for bin
# value can be "extinction" or "origination"
# bin.size in millions of years

base.line.values <- function(phy, bin.size=10, value="extinction"){

require(geiger)
require(phytools)



### Find which species are extinct automatically ###
if((class(phy) == "phylo") == TRUE){
no.ext.spp <- drop.extinct(phy)
v <- phy$tip.label %in% no.ext.spp$tip.label
extinct.spp <- phy$tip.label[v==FALSE]
} else {
no.ext.spp <- drop.extinct(phy[[1]])
v <- phy[[1]]$tip.label %in% no.ext.spp$tip.label
extinct.spp <- phy[[1]]$tip.label[v==FALSE]
}


### Create storage for results ###
storage <- as.data.frame(matrix(NA, ncol=3, nrow=length(extinct.spp)))
colnames(storage) <- c("Species","Start","End")


### Get origination and extinction data for each extinct species ###

if((class(phy) == "phylo") == TRUE){

# Get phy edge data, re-ordered by second column
edge.data <- as.data.frame(phy$edge)
colnames(edge.data) <- c("start","end")
edge.data <- edge.data[order(edge.data$end),]

for(i in 1:length(extinct.spp)){

storage[i,"Species"] <- as.character(extinct.spp[i])

n <- grep(extinct.spp[i], phy$tip.label)

start.node <- as.numeric(edge.data[n,][1])

storage[i,"Start"] <- nodeheight(phy, start.node)

storage[i,"End"] <- nodeheight(phy, n)

}

tree.height <- max(nodeHeights(phy))

# Correct the times by tree height
storage$Start <- tree.height - storage$Start
storage$End <- tree.height - storage$End

} else {

for(i in 1:length(extinct.spp)){

storage[i,"Species"] <- as.character(extinct.spp[i])

start.vals <- vector()
end.vals <- vector()

	for(j in 1:length(phy)){

	p <- phy[[j]]

	edge.data <- as.data.frame(p$edge)
	colnames(edge.data) <- c("start","end")
	edge.data <- edge.data[order(edge.data$end),]

	tree.height <- max(nodeHeights(p))

	n <- grep(extinct.spp[i], p$tip.label)

	start.node <- as.numeric(edge.data[n,][1])
	
	start.height <- tree.height - (nodeheight(p, start.node))

	start.vals <- c(start.vals, start.height)

	end.height <- tree.height - (nodeheight(p, n))

	end.vals <- c(end.vals, end.height)

	}

storage[i,"Start"] <- mean(start.vals, na.rm=T)

storage[i,"End"] <- mean(end.vals, na.rm=T)

}
}


# divide coords up by bin.size <- make first size of data frame

s <- seq(0,tree.height, bin.size)

# Create new storage for results #
results <- as.data.frame(matrix(NA, ncol=3, nrow=length(s)))
if(value=="extinction"){
colnames(results) <- c("bin.age","number.last.appearances","total.lineages")
} else {
colnames(results) <- c("bin.age","number.first.appearances","total.lineages")
}


# bin.age will be the midpoint of each interval
mid.bin <- bin.size/2

results[1,"bin.age"] <- 0+mid.bin

for(k in 2:nrow(results)){
results[k,"bin.age"] <- results[(k-1),"bin.age"] + bin.size
}


# total lineages in each bin
for(l in 1:length(s)){
v <- s[l]
d <- storage[storage$End < v & storage$Start > v,]
d <- na.omit(d)
results[l,"total.lineages"] <- nrow(d)
}

# origination/extinction

if(value=="extinction"){

for(j in 1:length(s)){
v.s <- s[j]
v.e <- s[j+1]
d <- storage[storage$End > v.s & storage$End <= v.e,]
d <- na.omit(d)
results[j,"number.last.appearances"] <- nrow(d)
}

} else {

for(j in 1:length(s)){
v.s <- s[j]
v.e <- s[j+1]
d <- storage[storage$Start > v.s & storage$Start <= v.e,]
d <- na.omit(d)
results[j,"number.first.appearances"] <- nrow(d)
}

}
#

text <- paste("Face.value.",value)
text <- gsub(" ","",text)

results[,4] <- results[,2] / results[,3]
colnames(results)[4] <- text

return(results)

}






