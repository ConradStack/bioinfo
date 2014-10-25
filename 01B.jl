
#using DataFrames

require("/Users/conrad/courses/BioinformaticsOne/kmer.jl")


# Functions
# NB - Strings are immutable in julia
isnotdigit(xx) = !isdigit(xx)
function maxfn(mm)
	function maxer(xx)
		xx == mm
	end
	maxer
end

# /Functions


#fname = "/Users/conrad/courses/BioinformaticsOne/01A_samp.txt"
fname = "/Users/conrad/courses/BioinformaticsOne/rosalind_1a.txt"
#fname = "/Users/conrad/courses/BioinformaticsOne/01C_samp.txt"
#fname = "/Users/conrad/scratch/MatinaHaplotype_v3.fasta copy"
#fname = "/Users/conrad/courses/BioinformaticsOne/dataset_2_6.txt"


# read the file
fin = open(fname,"r")

skipchars(fin, isnotdigit) # this function DOES have side-effects (despite the lack of trailing '!' on the function name)
k = int(readline(fin))
#k = 64
seekstart(fin)
kmerdict = buildmerdict(fin,k,true)
close(fin)
# close the filestream

# Look up largest kmers and write to a file
kmax = maximum(values(kmerdict))
maxx = maxfn(kmax)
maxinds = find(map(maxx,values(kmerdict)))
maxkmers = (collect(ASCIIString,keys(kmerdict)))[maxinds]
fout = splitext(fname)[1]*".maxmers"
tmp = open(fout,"w+")
write(tmp,join(maxkmers,' '))
close(tmp)

# readline(fin)
# readdlm
# skipchars
# get(filled_dict,"four",4)


