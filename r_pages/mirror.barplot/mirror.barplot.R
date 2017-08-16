


mirror.barplot <- function(lower.vals, upper.vals, cols=c("red", "blue"), bar.width=2, bar.spacing=1, upper.y.text="upper_label", lower.y.text = "lower_label", y.lab.offset = 2.5, y.lab.cex=1, margins=c(0.5,0.75,0.25,0.25) ){

par(mai=margins)

# A quick error check
if(length(lower.vals) != length(upper.vals)){
stop("lower.vals and upper.vals are not of the same length")
}

x.start.diff <- bar.width+bar.spacing

total.plot.width <- x.start.diff * length(lower.vals)


x.start <- 0

# Create a blank plot
plot(1:total.plot.width, seq(-max(lower.vals),max(upper.vals),length.out=length(1:total.plot.width)), type="n", xlab="",ylab="" ,xaxt="n")

# Plot increase in maximum values
for(i in 1:length(lower.vals)){

polygon(x=c(x.start, x.start+bar.width, x.start+bar.width, x.start, x.start), y=c(0, 0, upper.vals[i], upper.vals[i], 0), col=cols[1])

x.start <- x.start + x.start.diff

}

# Plot increase in minimum values
x.start <- 0

for(i in 1:length(lower.vals)){

polygon(x=c(x.start, x.start+bar.width, x.start+bar.width, x.start, x.start), y=c(0, 0, -lower.vals[i], -lower.vals[i], 0), col=cols[2])

x.start <- x.start + x.start.diff

} 

mtext(upper.y.text, side=2, at=mean(seq(0:max(upper.vals))), line=y.lab.offset, cex=y.lab.cex)


mtext(lower.y.text, side=2, at=-mean(seq(0:max(lower.vals))), line=y.lab.offset, cex=y.lab.cex)

} 




