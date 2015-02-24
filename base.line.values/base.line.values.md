base.line.values - Calculate face extinction and origination values from fossil species
========================================================

This function will calculate the face extinction and origination values for extinct species following [Foote (2005)](http://paleobiol.geoscienceworld.org/content/31/1/6.abstract)

First we will load required packages, and simulate a phylogeny with lots of extinct species:


```r
library(diversitree)
```

```
## Loading required package: deSolve
## Loading required package: ape
## Loading required package: subplex
## Loading required package: Rcpp
```

```r
library(geiger)
library(phytools)
```

```
## Loading required package: maps
## Loading required package: rgl
```

```r
 pars <- c(0.2, 0.2, 0.07, 0.05, 0.03, 0.03)
 set.seed(12)
 phy <- tree.bisse(pars, max.taxa=150, x0=0, include.extinct=TRUE)
```

The function expects species names that can be distinguished from each other. Due to the way which `diversitree` labels extinct taxa, we will generate some arbitrary (nonsense) names for our taxa. 


```r
for(i in 1:length(phy$tip.label)){
text <- paste("Species_",sample(letters[1:26],1),sample(letters[1:26],1),sample(letters[1:26],1),sample(letters[1:26],1),sample(letters[1:26],1))
text <- gsub(" ","",text)
phy$tip.label[i] <- text
}
```

Now it is possible to load and run the function. Function arguments are:
* phy - An object of class `phylo` or `multiphylo`. If of class `multiphylo` average values for origination and extinction will be calculated across from all phylogenies.
* bin.size - Interval size in millions of years to estimate origination and extinction values
* value - Can either be `"origination"` or `"extinction"` respectively


```r
source("base.line.values.R")

extinction.results <- base.line.values(phy, bin.size=2, value="extinction")

origination.results <- base.line.values(phy, bin.size=2, value="origination")
```



Visualizing the results:


```r
plot(extinction.results$bin.age, extinction.results$Face.value.extinction, pch=19, xlab="Age (Mya)",ylab="Relative Rate")
lines(extinction.results$bin.age, extinction.results$Face.value.extinction)
points(origination.results$bin.age, origination.results$Face.value.origination, col="red",pch=19)
lines(origination.results$bin.age, origination.results$Face.value.origination, col="red")
legend("topright",c("Origination Rate","Extinction Rate"), lwd=rep(2,2),col=c("red","black"))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

Gaps in the plot are due to periods of time where there are no fossils in the simulated phylogeny.
