Phylo diversity - calculating diversity through time from phylogenetic trees
========================================================

This function will calculate diversity indices for groups of species through time, given a phylogenetic tree and groupings for the species.
The supporting data are: function code, phylogeny and species groups.


```r
# Required packages
library(geiger)
library(phytools)
library(vegan)

source("phylo.diversity.R")
phy <- read.nexus("example.phy.70.spp.nex")
data <- read.csv("example.group.data.csv")
output <- phylo.diversity(phy=phy,groups=data)

# Look at the produced data file
tail(output)
```

```
##             Age  shannon   simpson Family_A Family_B Family_C Family_D
## 96  -0.22626883 1.496697 0.7256236        0        6       18        2
## 97  -0.18101506 1.481235 0.7182261        0        6       19        2
## 98  -0.13576130 1.405943 0.6911157        0        6       21        1
## 99  -0.09050753 1.406913 0.6853780        0        7       23        2
## 100 -0.04525377 1.351333 0.6701389        0        7       24        1
## 101  0.00000000 1.353600 0.6768000        0        7       24        1
##     Family_E Family_F Family_G Family_H
## 96        10        0        4        2
## 97        10        0        4        2
## 98        10        0        4        2
## 99        10        0        3        2
## 100       11        0        3        2
## 101       13        0        3        2
```

To plot an example, we first need to generate overall lineage through time data for the phylogeny


```r
attach(output)

ltt <- as.data.frame(ltt.plot.coords(phy))

root.time <- ltt[1,1]
```

We can now plot this using the results generated above


```r
par(mar=c(5,12,4,4)+0.1)

plot(ltt$time, ltt$N, axes=F, ylim=c(0,max(ltt$N)), xlab="", ylab="",type="l",col="black", main="",xlim=c(root.time,0))
# points(ltt$time, ltt$N,pch=1,col="black")
axis(2, ylim=c(0,max(ltt$N)),col="black",lwd=2)
mtext(2, text="Number of Species", line=2)

par(new=T)

col <- grep("simpson", colnames(output))

r <- range(output[,col])
r[1] <- r[1]-0.1
r[2] <- r[2]+0.1

plot(Age,output[,col], axes=F,ylim=r,
xlab="", ylab="", type="l",lty=2, main="",xlim=c(root.time,0),lwd=2)
axis(2,ylim=c(0,max(output[,col])),lwd=2,line=3.5)
# points(output[,"Age"], output[,col],pch=2)
mtext(2,text="Simpson's Diversity Index",line=5.5)

# add in the x axis
axis(1, pretty(range(ltt$time),10))
mtext("Age (Mya)",side=1,col="black",line=2)

legend("top",legend=c("Number of Species","Simpson Diversity Index"),lty=c(1,2))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 
