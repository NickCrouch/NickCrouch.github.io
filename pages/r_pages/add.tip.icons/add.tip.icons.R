

# phy:        Phylogeny of class 'phylo'
# data:       'data.frame' containing data, must have a 'Species' column
# plot.data:  column in data to be plotted. Must be numeric, starting at 1
# pch:        plotting symbol
# cols:       vector of cols?

add.tip.icons <- function(phy, data, plot.data="Location", grouping="Species", pch=21, cols=c("red","blue","green"), cex=2){

require(ape)

if( (plot.data %in% colnames(data)) == FALSE){
stop("Specified data to plot does not appear in the column names of the data")
}

v <- data[,grep(grouping, colnames(data))] %in% phy$tip.label

if((FALSE %in% v) == TRUE){
stop("Tip labels of phylogeny do not match grouping vector specified")
}


d <- data[,grep(plot.data, colnames(data))]

# does number of unique entries in plot.data equal number of colors provided?
num.unique.vals <- length(unique(d))

if(num.unique.vals != length(cols)){

stop("Number of states does not equal number of colors provided")

}

names(d) <- data[,grep(grouping, colnames(data))]

names <- phy$tip.label

m <- match(names, names(d))

r <- d[c(m)]

plot.col <- vector(length=length(phy$tip.label))

	for(i in 1:length(cols)){

	plot.col[as.numeric(r)==i] <- cols[i]

	}


# Plot

plot(phy, show.tip.label=FALSE)

if(pch < 21){

tiplabels(col=plot.col, pch=pch, cex=cex)

} else if(pch >= 21 & pch <= 25){

tiplabels(bg=plot.col, pch=pch, cex=cex)

} else {

stop("Invalid plotting symbol value provided")

}


}
