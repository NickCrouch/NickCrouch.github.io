phylo.diversity <- function(phy, groups, number.of.partitions=100, diversity.index=c("shannon","simpson")){

require(geiger)
require(phytools)
require(vegan)

## stop conditions ##

#
if(("Species" %in% colnames(groups)) == FALSE | ("Group" %in% colnames(groups)) == FALSE){
	stop("'groups' missing 'Species' and/or 'Group' column")
}

#
if(length(phy$tip.label) != nrow(groups)){
	stop("Number of tips in the phylogeny must equal the number of rows in 'groups'")
}

#
tip.label.in.data <- phy$tip.label %in% groups$Species
if(("FALSE" %in% tip.label.in.data) == TRUE){

tips <- phy$tip.label[tip.label.in.data==FALSE]

stop("the following species are present on the phylogeny, but not in the data:",tips)

}

#
data.in.tip.label <- groups$Species %in% phy$tip.label
if(("FALSE" %in% data.in.tip.label) == TRUE){

species <- groups$Species[data.in.tip.label==FALSE]

stop("the following species are in the data, but not on the phylogeny:",species)

}

## Start of working function ##
overall.ltt.data <- ltt.plot.coords(phy)

#identify groups present
groups.present <- unique(groups$Group)


#### -- Data storage bits --- ###

# divide oldest time in overall.ltt.data by number of partitions chosen
root.time <- as.numeric(overall.ltt.data[1,1])

bin.iteration <- abs(root.time) / number.of.partitions

# storage for results
diversity.results <- as.data.frame(matrix(NA, ncol=length(diversity.index)+1,nrow=number.of.partitions+1))
col.names <- c("Age",diversity.index)
colnames(diversity.results) <- col.names

diversity.results[1,1] <- root.time

for(k in 2:nrow(diversity.results)){
  diversity.results[k,1] <- diversity.results[(k-1),1] + bin.iteration
}

n <- nrow(diversity.results)
diversity.results[n,1] <- 0

### --- End Data storage bits --- ###




# identify single species groups
t <- table(groups$Group)
single.spp.gps <- t[t==1]


# if there are groups with single species present, record and identify when they arise
if(length(single.spp.gps) > 0){

   # get height for those species
   single.spp.group.heights <- as.data.frame(matrix(NA, ncol=2,nrow=length(single.spp.gps)))
   colnames(single.spp.group.heights) <- c("Group","Height")

   # vector of terminal branch lengths
   n <- length(phy$tip.label)
   terminal.branch.lengths<-setNames(phy$edge.length[sapply(1:n,function(x,y)   which(y==x),y=phy$edge[,2])],phy$tip.label)
   

	for(l in 1:length(single.spp.gps)){
	
	gp <- names(single.spp.gps)[l]

	single.spp.group.heights[l,1] <- gp

	r <- grep(gp,groups[,"Group"])

	spp <- as.character(groups[r,"Species"])

	spp.height <- as.numeric(terminal.branch.lengths[names(terminal.branch.lengths)==spp])

	single.spp.group.heights[l,2] <- spp.height

	}
}


nodes <- vector()

# need common ancestor node for those groups of 2 or more species
for(i in 1:length(groups.present)){
  gp <- as.character(groups.present[i])
  
  if((gp %in% names(single.spp.gps)) == FALSE){

	  r <- grep(gp, groups[,"Group"])
	  spp <- groups[r,"Species"]
  
  	  nd <- findMRCA(phy,spp)
  
	  nodes <- c(nodes,nd)
	  ll <- length(nodes)
 	  names(nodes)[ll] <- gp
      }
}

## Creating list where each element is a data frame of ltt data for each group 

   # list of extinct species
   no.extinct <- drop.extinct(phy)
   name.check <- phy$tip.label %in% no.extinct$tip.label
   extinct.species <- phy$tip.label[name.check==FALSE]
  

# make sure namespace for getAncestors is right
getAncestors<-phytools:::getAncestors

# branching times gives distances from tip to node
br.times <- branching.times(phy)


ltt.data <- vector(mode="list",length=length(groups.present))
names(ltt.data) <- groups.present



for(k in 1:length(groups.present)){
  
  gp <- groups.present[k]

  if((gp %in% names(single.spp.gps)) == FALSE){
  
  ancestor <- nodes[names(nodes)==gp]
  
  sub.clade <- extract.clade(phy, ancestor)
  
  clade.data <- ltt.plot.coords(sub.clade)
  
  list.location <- grep(gp,names(ltt.data))

  ltt.data[[list.location]] <- clade.data

  } else {

  # create storage using template of diversity results
  single.spp.data <- as.data.frame(matrix(NA, ncol=2,nrow=number.of.partitions+1))
  colnames(single.spp.data) <- c("time","N")

  single.spp.data[,1] <- diversity.results[,1]

  # single.spp.group.heights gives JUST BRANCH LENGTH LEADING TO TERMINAL TAXON

  r <- grep(gp,single.spp.group.heights$Group)
  
  spp <- single.spp.group.heights[r,1]

  height <- single.spp.group.heights[r,2]

  if((spp %in% extinct.species) ==FALSE){

	# make sure number is negative
	height <- -abs(height)

	r <- single.spp.data[,"time"] > height

  	single.spp.data[r==TRUE, "N"] <- 0
  	single.spp.data[r==FALSE,"N"] <- 1

 	 list.location <- grep(gp, names(ltt.data))

 	 ltt.data[[list.location]] <- single.spp.data

   }  else {

   # extinct species need height of parent node + branch length for beggining and end

	spp.node <- getAncestors(phy,node=which(phy$tip.label==spp),type="parent")

	start <- as.numeric(br.times[names(br.times)==spp.node])

	start <- -abs(start)

	end <- start - height

	r <- single.spp.data[,"time"] > start &  single.spp.data[,"time"] < end

	single.spp.data[r == TRUE,"N"] <- 1
	single.spp.data[r == FALSE,"N"] <- 0

	list.location <- grep(gp, names(ltt.data))

	ltt.data[[list.location]] <- single.spp.data

   }

  }

}



# for each group, at the end of each bin, record number of species present
raw.results <- as.data.frame(matrix(NA, ncol=length(groups.present)+1,nrow=nrow(diversity.results)))
names <- c("Age",as.character(groups.present))
colnames(raw.results) <- names

raw.results[,1] <- diversity.results[,1]

for(j in 1:length(groups.present)){
  
  gp <- as.character(groups.present[j])
  
  n <- grep(gp,names(ltt.data))
  
  gp.data <- as.data.frame(ltt.data[[n]])
  
  num.rows.group <- nrow(gp.data)
  
  col <- grep(gp,colnames(raw.results))
  
  num.rows.all <- nrow(raw.results)
  
  raw.results[num.rows.all,col] <- gp.data[num.rows.group,2]
  
  start <- num.rows.all-1
  
 	 for(i in start:1){
    
   	 time.interval <- raw.results[i,1]
    
   	 num.spp.d <- gp.data[gp.data$time >= time.interval,]
    
   	 num.spp.time.interval <- num.spp.d[1,2]
    
    	 raw.results[i,col] <- num.spp.time.interval
    
 	 }
  
}

# run specified diversity indices on the data

for(k in 1:length(diversity.index)){
  
  index <- diversity.index[k]
  
  index.column <- grep(index, colnames(diversity.results))
  
    for(i in 1:nrow(raw.results)){
  
    r <- raw.results[i,-1]
    div <- diversity(r, index=index)
    diversity.results[i,index.column] <- div
  
  }

}


storage.to.output <- cbind(diversity.results,raw.results[,-1])

return(storage.to.output)

} 

















