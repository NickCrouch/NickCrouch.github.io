

fixLineBreaks <- function(x, outputname){

all.lines <- readLines(x)

# how many species and how many characters
where.dim <- grep("dimensions", all.lines, ignore.case = TRUE)
info.string <- all.lines[where.dim]
info.string <- gsub(" = |= | =", "=", info.string)
info.string.split <- strsplit(info.string, "=")[[1]]
info.string.split <- strsplit(info.string.split, " ") %>% unlist
num.taxa <- info.string.split[grep("ntax", info.string.split, ignore.case = TRUE)+1] %>% as.numeric
# remove semi colons
info.string.split <- gsub(";","",info.string.split)
num.char <- info.string.split[grep("nchar", info.string.split, ignore.case = TRUE)+1] %>% as.numeric


# the characters we want to keep
good.chars <-c("?", "-", "0", "1", "2","3","4","5","6","7","8","9", "(", ")")


out <- vector(mode="list", length=num.taxa)
store.location <- 1
for(i in 1:length(all.lines)){
  txt <- all.lines[[i]]
  if(is.blank.line(txt)==FALSE){
  split.txt <- strsplit(txt, "")[[1]] %>% fix.naming
    if(length(split.txt)>5){
  chars <- grep("[A-Za-z]", split.txt) 
  num.chars <- length(chars)
  
  if(num.chars > 0){
    out[[store.location]] <- "temp"
    name <- paste0(split.txt[1:max(chars)], collapse="")
    names(out)[store.location] <-name
    
    keep.scores <- split.txt[max(chars)+1 : length(split.txt)]
    keep.scores <- keep.scores[keep.scores %in% good.chars]
    
    polymorphism.presence <- "(" %in% keep.scores
    if(polymorphism.presence==TRUE){
      keep.scores <- fix.polymorphism(keep.scores)
    }
    out[[store.location]] <- keep.scores
  } else {
    keep.scores <- split.txt[split.txt %in% good.chars]
    polymorphism.presence <- "(" %in% keep.scores
    if(polymorphism.presence==TRUE){
      keep.scores <- fix.polymorphism(keep.scores)
    }
    out[[store.location]] <- c(out[[store.location]], keep.scores)
  }
  
  length.test <- length(out[[store.location]])
  
  if(length.test > num.char){ 
    error.text <- paste("iteration",i," has more characters than the number of specified characters, check species/specimen names")
    stop(print(error.text))
    }
  
  if(length.test == num.char){
    store.location <- store.location + 1
  }
    }
  }
}

file.text <- paste0(outputname,".nex", collapse = "")

write.nexus.data(out, file=file.text)
}

## a function called by the loop ##

# sometimes polymorphisms are coded with {}. Any cases of that are standardized to
# parentheses here
fix.naming <- function(x){
  if("{" %in% x){
    g <- grep("\\{", x)
    x[g] <- "("
  }
  
  if("}" %in% x){
    g <- grep("\\}", x)
    x[g] <- ")"
  }
  return(x)
}


##  a function called by the loop ##
fix.polymorphism <- function(x){
  where.start <- grep("\\(", x)
  where.ends <- grep("\\)", x)
  drop.list <- vector(mode="numeric")
  
  for(j in 1:length(where.start)){
    st <- where.start[j]
    ed <- where.ends[j]
    all.poly.txt <- paste0(x[st:ed], collapse = "")
    x[st] <- all.poly.txt
    drop.adds <- seq(st+1, ed, by=1)
    drop.list <- c(drop.list, drop.adds)
  }
  x <- x[-drop.list]
  return(x)
}


#####

is.blank.line <- function(x){
  if(length(x)==1 & x==""){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

