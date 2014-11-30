

require("src/seq.jl")
require("src/kmer.jl")
const mass = get_mass_table()
aa2mass(aa::Char) = integer_mass(aa,mass)
const aatab = getAAtable()


#l1 = mass_spectrum("ATND",mass)
l2 = mass_spectrum("NQEL",mass)
#score(l1,l2)

im = map(aa2mass, collect("NQEL") )
linmass = mass_spectrum(im,true)

score(l2,linmass)