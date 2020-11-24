


plot.shared.nodes <- function(phy.list, pie.cols, cex, ...){
	require(ape)
	require(dplyr)
	require(phytools)

	all.nodes <- seq(from = Ntip(phy.list[[1]])+1, by = 1, length.out = Nnode(phy.list[[1]]))

	test.tag.sets <- lapply(phy.list[2:length(phy.list)], .generate.tags)

	node.res <- lapply(all.nodes, .run.node, phy.list=phy.list, test.tag.sets=test.tag.sets) 
	node.res <- do.call(rbind, node.res)

	plot.prop <- lapply(2:ncol(node.res), function(ii){
		.make.cols.dat(node.res[,ii])
	} ) 
	
	plot.prop <- do.call(cbind, plot.prop)

	plot.prop.fin <- apply(plot.prop, 1, .make.prop) %>% t

	plot.cols <- rep("white", ncol(plot.prop.fin))
	where.new.cols <- seq(2, ncol(plot.prop.fin), by=2)
	num.comps <- length(phy.list)-1
	for(i in 1:num.comps){
		plot.cols[where.new.cols[i]] <- pie.cols[i]
	}

	plot.phylo(phy.list[[1]], ...)
	nodelabels(node = node.res[,1], pie = plot.prop.fin, piecol=plot.cols, cex=cex)

}

.run.node <- function(node, phy.list, test.tag.sets){
	node.daughters <- .daughters(node, phy.list[[1]])
	daughter.sets <- lapply(node.daughters, .run.getDesc, phy.list[[1]])
	target.id <- .node.tag(daughter.sets)
	out <- matrix(0, nrow = 1, ncol=length(test.tag.sets))
	out[,grep(target.id, test.tag.sets)] <- 1 
	colnames(out) <- paste0("t",seq(1:length(test.tag.sets)))
	out <- cbind(node, out)
	return(out)
}

.daughters <- function(node, phy){
  where.node <- phy$edge[,1]==node
  daughters <- phy$edge[where.node==TRUE,2] %>% unlist
  return(daughters)
}

.run.getDesc <- function(node, phy){
	all.desc <- getDescendants(phy, node)
	tip.desc <- all.desc[all.desc<=Ntip(phy)]
	tip.names <- phy$tip.label[tip.desc]
	return(tip.names)
}

.node.tag <- function(x){
	part.one <- paste(sort(x[[1]]), collapse="-")
	part.two <- paste(sort(x[[2]]), collapse="-")
	out <- paste(part.one, "/", part.two, collapse="")
	return(out)
}

.generate.tags <- function(x){
	all.nodes <- seq(from = Ntip(x)+1, by = 1, length.out = Nnode(x))
	res <- lapply(1:length(all.nodes), function(ii){
		daughter.nodes <- .daughters(all.nodes[ii], x)
		daughter.sets <- lapply(daughter.nodes, .run.getDesc, x)
		out <- .node.tag(daughter.sets)
	})
	return(res)
}


.make.cols.dat <- function(x){
	res <- matrix(0, ncol = 2, nrow = length(x))
	for(i in 1:length(x)){
		if(x[i]==1){
			res[i,2] <- 1
		} else {
			res[i,1] <- 1
		}
	}
	return(res)
}


.make.prop <- function(x){
	x / sum(x) %>% return
}