############################## 
# k-mer related functions
#

# Read kmer from a filestream (or in-memory stream::IOBuffer)
readkmer(ff::IO,kk) = join(map(char,readbytes(ff,kk)))

# Methods that build dictionary of kmer values
function buildmerdict(fin::IO, k::Integer, oneline::Bool=false)
	merdict = Dict()
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



