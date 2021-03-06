



require(igraph)

kdict <- function(dna,ksize){
	nout = nchar(dna)-ksize+1
	nvec = character(nout)
	for(ii in seq( nout ))
		nvec[ii] <-substring(dna,ii,ii+ksize-1)
	nvec
}


#test = sort(scan("input_strings.txt",what=character(),skip=0))
#test = c("ATGCG","GCATG","CATGC","AGGCA","GGCAT")
test = apply(expand.grid(0:1,0:1,0:1,0:1),1,paste,collapse="")
test = tail(head(test,-1),-1)

ksize = nchar(test[1])
kminusone = ksize-1
prefix = sapply(test, substring,1,kminusone)
suffix = sapply(test, substring,2,ksize)

gr = graph.empty(length(test))
V(gr)$name <- test


for(ii in seq(length(test))){
	finds = which(suffix[ii] == prefix)
	if(length(finds)>0)
		gr[from=rep(ii,length(finds)),to=finds] <- 1

	rinds = which(prefix[ii] == suffix)
	if(length(rinds)>0)
		gr[from=rinds,to=rep(ii,length(rinds))] <- 1
		
}

cat(apply(get.edgelist(gr),1, paste,collapse=" -> "),sep="\n", file="output_strings.txt")
#plot(gr)

# find a hamiltonian path:
starts = which(degree(gr,mode="in") == 0)  # assembly k-mer starting points (candidates)
if(length(starts)==0)
	starts = 1
	#starts = which(V(gr)$name == sample(V(gr),1)$name)
stopifnot(length(starts)>0)
stopifnot(is.connected(gr))

start = starts[1]
dfs.path = graph.dfs(gr,start,unreachable=F, dist=F, order.out=T)
stopifnot(length(dfs.path$order) == vcount(gr))

neword = dfs.path$order
assembly = V(gr)$name[neword]
collapsed = paste(c(assembly[1],sapply(assembly[2:length(assembly)],substring,ksize,ksize)),collapse="")



