
calculate.morpho.poly.area <- function(morpho.data, group.info, conf.interval, scale.pca =TRUE){

require(pracma)
require(car)

if( (any(is.na(morpho.data))) == TRUE){
stop("The data frame contains missing value(s)")
}

prin.comp <- prcomp(morpho.data, scale=scale.pca)

pcScores.all <- prin.comp$x[,1:2]

pcScores <- vector(mode="list", length=length(unique(group.info)))
names(pcScores) <- unique(group.info)

unique.groups <- unique(group.info)

for(i in 1:length(unique.groups)){

	grp <- unique.groups[i]

	group.scores <- pcScores.all[group.info==grp,]
	
	pcScores[[i]] <- group.scores

}


ellipse.coords <- lapply(pcScores, ellipse.data , conf.interval)

areaVals <- lapply(ellipse.coords, getArea)

results <- as.data.frame(unlist(areaVals))
colnames(results) <- "PolygonArea"

results[,"PolygonArea"] <- abs(results[,"PolygonArea"])

# Number of each group present
results$NumObservations <- NA

tt <- table(group.info)

locations <- match(rownames(results),names(tt))

for(k in 1:length(tt)){
results[k,"NumObservations"] <- as.numeric(tt[locations[k]])
}

return(results)

}


##


ellipse.data <- function(pc.scores, conf.interval){
ellipse.coord.data <- dataEllipse(pc.scores[,1], pc.scores[,2], levels=conf.interval, draw=F)
return(ellipse.coord.data)
}

##

getArea <- function(ellipse.data){
area <- polyarea(ellipse.data[,1], ellipse.data[,2])
return(area)
}









