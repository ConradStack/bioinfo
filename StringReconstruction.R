test = c("ACCGA",
"CCGAA",
"CGAAG",
"GAAGC",
"AAGCT")



test = scan("input_strings.txt",what=character(),skip=0)


ksize = nchar(test[1])
kminusone = ksize-1

starts = sapply(test, substring,1,kminusone)
stops = sapply(test, substring,2,ksize)

assembly = test[1]
current = stops[1]

while(length(assembly) < length(test)){
	nextmer = which(starts == current)
	assembly = c(assembly, substring(stops[nextmer],kminusone,kminusone))
	current = stops[nextmer]
}

paste(assembly,collapse="")
