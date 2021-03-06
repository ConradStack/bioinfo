
convert2indices = function(str){
	tmp = strsplit(str,"")[[1]]
	sapply(tmp,switch,"A"=1,"C"=2,"G"=3,"T"=4)
}

indices2dna = function(inds){
	sapply(inds,switch,"1"="A","2"="C","3"="G","4"="T")
} # indices2dna(c(1,2,3,4)) -> "A" "C" "G" "T"


kdict <- function(dna,ksize){
	nout = nchar(dna)-ksize+1
	nvec = character(nout)
	for(ii in seq( nout ))
		nvec[ii] <-substring(dna,ii,ii+ksize-1)
	nvec
}

tokenize = function(xx){
	strsplit(xx,"")[[1]]
}

getprofile <- function(mm){
	apply(mm,2,function(x) table(factor(x,levels=c("A","C","G","T"))) / length(x) )
}


bestmotif <- function(inds,profile,k){

	maxprob = -1
	maxmer = indices2dna(inds[1:k])
	nout = length(inds)-k+1
	for(ii in seq( nout )){
		slice = ii:(ii+k-1)
		tmpinds = inds[slice]
		thisprob = prod(profile[cbind(tmpinds,1:ksize)])
		if(thisprob > maxprob){
			#maxmer = substring(test,slice[1],tail(slice,1))
			maxmer = indices2dna(tmpinds)
			maxprob = thisprob
		}
	}
	maxmer

}

# ksize = 3
# test = c("GGCGTTCAGGCA",
# "AAGAATCAGTCA",
# "CAAGGAGTTCGC",
# "CACGTCAATCAC",
# "CAATAATATTCG")

ksize = 7
test = 
c("CTCGATGAGTAGGAAAGTAGTTTCACTGGGCGAACCACCCCGGCGCTAATCCTAGTGCCC",
"GCAATCCTACCCGAGGCCACATATCAGTAGGAACTAGAACCACCACGGGTGGCTAGTTTC",
"GGTGTTGAACCACGGGGTTAGTTTCATCTATTGTAGGAATCGGCTTCAAATCCTACACAG")

kseeds = kdict(test[1],ksize)
motifs = matrix("",nrow=length(test),ncol=ksize)
indices = sapply(test,convert2indices)
maxscore = Inf
maxset = motifs
tsize = length(test)
for(kseed in kseeds){
	motifs[1,] <- tokenize(kseed)
	for(jj in 2:tsize){
		prof = getprofile(motifs[1:(jj-1),,drop=F])
		motifs[jj,] <- bestmotif(indices[,jj], prof, ksize)
	}
	score = sum(apply(motifs,2,function(x) { 
		tmptab = table(x)
		#major = names(tmptab)[which.max(tmptab)] # consensus base
		sum(tmptab) - tmptab[which.max(tmptab)]
	}))
	if(score < maxscore){
		maxscore = score
		maxset = motifs
	}
}
maxscore
cat(sprintf("%s\n",apply(maxset,1,paste,collapse="")))





