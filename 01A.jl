
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

fname = "/Users/conrad/Downloads/dataset_2_6.txt"


# read the file
fin = open(fname,"r")
k = 9
seekstart(fin)
kmerdict = buildmerdict(fin,k)
query = readline(fin)
query = query[1:(end-1)]
close(fin)
# close the filestream

print(kmerdict[query])

