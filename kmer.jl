############################## 
# k-mer related functions
#


### K-mers from string arrays (e.g., DNA or RNA string streams)

# Read kmer from a filestream (or in-memory stream::IOBuffer)
readkmer(ff::IO,kk) = join(map(char,readbytes(ff,kk)))

# Methods that build dictionary of kmer values
function buildmerdict(fin::IO, k::Integer, oneline::Bool=false)
	merdict = Dict{ASCIIString,Int64}()
	kmer = readkmer(fin,k)
	merdict[kmer] = 1
	#while (nxchar = read(fin,Char)) != '\n'
	while !eof(fin)
		nxchar = read(fin,Char)
		if nxchar != '\n'
			kmer = kmer[2:end]*string(nxchar)
			merdict[kmer] = (get(merdict,kmer,0)+1)
		elseif oneline
			break
		end
	end
	merdict
end
buildmerdict(str::ASCIIString, k::Integer) = buildmerdict(IOBuffer(str),k)

# Approximate pattern count:
function count_d(kdict::Dict{ASCIIString,Int64}, kquery::ASCIIString, dmax::Integer)
	mapreduce(x-> dist(x[1],kquery)<=dmax ? x[2] : 0, + ,kdict)
end

# Approximate pattern count, including reverse complement
function count_d_rc(kdict::Dict{ASCIIString,Int64}, kquery::ASCIIString, dmax::Integer)
	mapreduce(x-> (dist(x[1],kquery)<=dmax ? x[2] : 0) + (dist(x[1],rc(kquery))<=dmax ? x[2] : 0), + ,kdict)
end








