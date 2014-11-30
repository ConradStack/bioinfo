
require(igraph)

kdict <- function(dna,ksize){
	nout = nchar(dna)-ksize+1
	nvec = character(nout)
	for(ii in seq( nout ))
		nvec[ii] <-substring(dna,ii,ii+ksize-1)
	nvec
}


test = sort(scan("input_strings.txt",what=character(),skip=0))
#test = c("ATGCG","GCATG","CATGC","AGGCA","GGCAT")

ksize = nchar(test[1])
kminusone = ksize-1
prefix = sapply(test, substring,1,kminusone)
suffix = sapply(test, substring,2,ksize)

gr = graph.empty(length(test))
V(gr)$name <- test


for(ii in seq(length(test))){
	finds = which(suffix[ii] == prefix)
	if(length(finds)>0)
		gr[from=ii,to=finds] <- 1

	rinds = which(prefix[ii] == suffix)
	if(length(rinds)>0)
		gr[from=rinds,to=ii] <- 1
		
}

cat(apply(get.edgelist(gr),1, paste,collapse=" -> "),sep="\n", file="output_strings.txt")
plot(gr)


