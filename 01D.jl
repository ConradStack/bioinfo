

require("/Users/conrad/courses/BioinformaticsOne/kmer.jl")
#fname = "/Users/conrad/courses/BioinformaticsOne/01D_samp.txt"
fname = "/Users/conrad/courses/BioinformaticsOne/rosalind_1d.txt"
fout = splitext(fname)[1]*".out"


fin = open(fname,"r")
dna = chomp(readline(fin))
varstr = chomp(readline(fin))
(k,L,t) = int64(split(varstr," "))
seekstart(fin)
kmerdict = buildmerdict(fin,k,true)
close(fin)


kN = collect(Int64,values(kmerdict))
usemers = find( x -> x >= t, kN )
filter!( (x,y)-> y >= t   , kmerdict )

keep = ASCIIString[]
for i in keys(kmerdict)
	println(i)
	mit = eachmatch(Regex(i),dna,true)
	pos = map(x->x.offset,mit)
	if length(pos) >= t
		#spreads = map( x -> (pos[(x+t-1)]-pos[x])+1 ,[1:t:length(pos)] )
		spreads = map( x -> (pos[(x+t-1)]-pos[x])+1 ,[1:(length(pos)-t)] )
		if any(x -> x<=L ,spreads)
			push!(keep,i)
		end
	end
end

tmp = open(fout,"w+")
write(tmp,join(keep,' '))
close(tmp)


