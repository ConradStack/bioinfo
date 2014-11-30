


function extend( kmers::Array{ASCIIString,1}, d::Integer )
	ext = ASCIIString[]
	for x in kmers
		tmp = unique(extend(x,d))
		append!(ext,tmp)
	end
	return unique(ext)
end


function extend(kmer::ASCIIString,d::Integer)
	alpha = ['G','C','A','T']
	aset = Set(alpha)
	alpha_try = reduce(vcat,map(x-> collect(permutations(x)),(combinations(repmat(alpha,d),d))))
	ksize = length(kmer)
	#matsize = binomial(4,d)*factorial(d)*binomial(ksize,d)
	matsize = length(alpha_try)*binomial(ksize,d)
	ksep = collect(kmer)
	#ktmp = deepcopy(ksep)
	outmat = repmat(ksep',matsize)
	positions = combinations(range(1,ksize),d)
	count = 1
	for pos in positions
		for atry in alpha_try
			#println(pos," ", atry)
			outmat[count,pos] = atry
			count += 1
		end
	end
	#println(count)
	#println( binomial(4,d)*factorial(d)*binomial(ksize,d) )
	#return outmat
	return vec(reducedim(*,outmat,2,""))
end
