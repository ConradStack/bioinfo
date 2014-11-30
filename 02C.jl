
require("src/seq.jl")
require("src/kmer.jl")

#aa="LEQN"
aa="RFQNYTVLCWFNIY"
mass = get_mass_table()
aa2mass(aa::Char) = integer_mass(aa,mass)
effect = map(aa2mass,collect(aa))

shit = sort(mass_spectrum(effect))
println( join(shit," ") )



aa= "TLAM"
