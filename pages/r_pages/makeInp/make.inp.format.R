

# Takes a PBDB data.frame and converts it to .inp format

# 'target.orders' is a character string specifying those orders of interest, this will be
# a 1 in the first grouping column, everything else will be a 1 in the second grouping 
# column


make.inp.format <- function(pbdb.data, age.interval=2, max.age=66, file.out="myfile", target.orders){

require(dplyr)

# unique species in data
unique.spp <- pbdb.data$tna %>% unique

# where species will be sampled
time.bins <- seq(0, max.age, by=age.interval) %>% rev

# where the data will be stored before output
output.d <- matrix(0, ncol=length(time.bins), nrow=length(unique.spp)) %>% as.data.frame
rownames(output.d) <- unique.spp

for(k in 1:ncol(output.d)){
nm <- paste(time.bins[k],"-",time.bins[k+1])
colnames(output.d)[k] <- nm
}

nn <- ncol(output.d)
output.d <- output.d[,1:nn-1]


# create a list that will be used to identify where in output.d species will go

placement.list <- vector(mode="list", length=ncol(output.d))
for(i in 1:ncol(output.d)){
vals <- strsplit(colnames(output.d)[i], "-")[[1]] %>% as.numeric
vals[2] <- vals[2] + 1e-5
placement.list[[i]] <- vals
}

placement.list[[ncol(output.d)]][2] <- 0

# make group column info

group.info <- data.frame(gp1 = rep("0", length(unique.spp)), gp2 = rep("0", length(unique.spp)))

group.info[,1] <- as.character(group.info[,1])
group.info[,2] <- as.character(group.info[,2])

# fill in output.d

for(j in 1:length(unique.spp)){

	spp <- unique.spp[j] %>% as.character

	dd <- pbdb.data[pbdb.data$tna == spp,]

	col.locations <- .where1(dd, placement.list)

	if(length(col.locations)==2){
	output.d[j,col.locations[1]:col.locations[2]] <- 1
	} else {
	output.d[j,col.locations[1]] <- 1
	}	

	order <- unique(dd$odl) %>% as.character
	if(order %in% target.orders){
	group.info[j,1] <- "1"
	} else {
	group.info[j,2] <- "1"
	}

}


# make character strings to export
lines <- vector(mode="character", length=nrow(output.d))

for(t in 1:nrow(output.d)){
pt1 <- paste(as.character(output.d[t,]), collapse="")
pt2 <- paste(as.character(group.info[t,]), collapse=" ")
lines[t] <- paste(pt1,pt2, ";", collapse=" ")
}

# export the lines

fileConn<-paste0(file.out,".inp", sep="")
writeLines(lines, fileConn)

}




##


.where1 <- function(dd, placement.list){

v1 <- v2 <- vector(length=length(placement.list))

eag <- max(dd$eag, na.rm=T)

na.test <- is.na(dd$lag)

if(FALSE %in% na.test){
lag <- min(dd$lag, na.rm=T)
} else {
lag <- 0
}

for(i in 1:length(placement.list)){

v1[i] <- eag > placement.list[[i]][2] & eag <= placement.list[[i]][1]
v2[i] <- lag > placement.list[[i]][2] & lag <= placement.list[[i]][1]

}

start.col <- grep(TRUE, v1)
end.col <- grep(TRUE, v2)

return(c(start.col, end.col))

}



