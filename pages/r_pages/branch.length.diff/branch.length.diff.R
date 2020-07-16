

branch.length.diff <- function(multiPhy, multiPhy1, plot.top=NULL, show.top=NULL,
                               marPar = c(5,4,4,2), taxon.label.cex=0.5, pt.cex=1,
                               bar.width=1, label.horiz=FALSE){

require(ape)

if( Ntip(multiPhy[[1]]) != Ntip(multiPhy1[[1]])){
	stop("The sets of trees do not have the same number of terminal taxa")
}
  
  ## PUT IN NAME CHECK ERROR CATCH ##


multiPhy.lengths <- lapply(multiPhy, terminal.branch.length)

multiPhy1.lengths <- lapply(multiPhy1, terminal.branch.length)

# Compare the value for each species in each iteration of list
length.diffs <- mapply(length.diff, multiPhy.lengths, multiPhy1.lengths)

# calculate mean and standard error
mean.diff<- apply(length.diffs, 1, mean)

std.diff<- apply(length.diffs, 1, std)


# order
mean.diff<- sort(mean.diff, decreasing = T)

m <- pmatch(names(mean.diff), names(std.diff))

std.diff <- std.diff[m]

x.s <- seq(1, Ntip(multiPhy[[1]]), 1)

## subset - only show top n values?

if(!is.null(plot.top)){
  mean.diff <- mean.diff[1:plot.top]
  std.diff <- std.diff[1:plot.top]
  x.s <- x.s[1:plot.top]
}



# plot
rbPal <- colorRampPalette(c("black","purple","magenta","yellow"))

#This adds a column of color values
# based on the y values
cols <- rbPal(100)[as.numeric(cut(mean.diff,breaks = 100))]


upper.y.lim <- mean.diff[1] + std.diff[1]
lower.y.lim <- mean.diff[length(mean.diff)] - std.diff[length(std.diff)]

par(mar=marPar)

plot(x.s, mean.diff, ylim=c(lower.y.lim, upper.y.lim),
     xlab="",
     ylab="Mean Absolute Branch Length Difference",
     xaxt='n')

arrows(x0=x.s, x1=x.s, y0=mean.diff-std.diff, y1=mean.diff+std.diff, 
       length=0.05, 
       angle=90,
       code=3,
       lwd=bar.width)

points(x.s, mean.diff, pch=21,bg=cols, cex=pt.cex)

if(label.horiz == FALSE){
  ll <- 2
} else {
  ll <- 0
}


axis(side=1, at=x.s, labels=names(mean.diff),las=ll, cex.axis=taxon.label.cex)

# add legend?

if(!is.null(show.top)){
  legend.nam <- paste(names(mean.diff[1:show.top]), round(mean.diff[1:show.top],2),sep = " " )
  legend("topright", legend.nam)
}

}



# revell ref?
terminal.branch.length <- function(phy){
terms <- phy$edge[, 2] <= Ntip(phy)
terminal.edges <- phy$edge.length[terms]
names(terminal.edges) <- phy$tip.label[phy$edge[terms, 2]]
return(terminal.edges)
}


# compare 
# Each iteration of the lists contains the same taxa, so they can just be ordered 
# and then the difference taken

length.diff <- function(x, y){
  x <- x[sort(names(x))]
  y <- y[sort(names(y))]
  diff <- abs(x - y)
  return(diff)
}

# standard error
std <- function(x) sd(x)/sqrt(length(x))




