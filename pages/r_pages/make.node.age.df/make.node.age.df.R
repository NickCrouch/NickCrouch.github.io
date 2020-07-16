


make.node.age.df <- function(phy){

annotations <- phy$annotations

raw.heights <- lapply(annotations, get_height)

heights <- do.call(rbind, raw.heights)

heights$node <- phy$edge[,2]

is.tip <- phy$edge[,2] <= Ntip(phy)

node.heights <- heights[is.tip==FALSE,]

# get the root
root <- as.data.frame(rbind(unlist(phy$root.annotation$`height_95%_HPD`)))
root$node <- Ntip(phy) + 1
colnames(root)[1:2] <- c("Lower95%", "Upper95%")

# all results
export <- rbind(root, node.heights)

return(export)

}


###


get_height <- function(x){
h <- x$`height_95%_HPD`
df <- as.data.frame(rbind(unlist(h)))
colnames(df) <- c("Lower95%", "Upper95%")
return(df)
}

