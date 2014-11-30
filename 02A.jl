

###########################################################################
#  
# Translate an RNA string in to an amino acid
#

require("src/seq.jl")
aa_tab = getAAtable()


genome = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"

frame = 1

starts = 1:3:length(genome)
aastr = ""
for ii in starts
	aastr *= aa_tab[genome[ii:(ii+2)]]
end


# additional exercise break:

tyrocidine = "VKLFPWFNQY"
aacounts = getAAcounts(aa_tab)
mapreduce(x-> aacounts[x] ,*, tyrocidine)


test = "LEADER"
aacounts = getAAcounts(aatab)
mapreduce(x-> aacounts[x] ,*, test)




