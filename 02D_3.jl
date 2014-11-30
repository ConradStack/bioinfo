
require("src/seq.jl")

theo = [0,113,128,186,241,299,314,427]


mass = get_mass_table()
aa2mass(aa::Char) = integer_mass(aa,mass)
masses = unique(values(mass))
#effect = map(aa2mass,collect(aa))
#mass_spectrum(masses[randperm(length(masses))[1:3]]) # random example, for debugging

for x=masses,y=masses,z=masses
	spec = mass_spectrum([x,y,z])
	if all(map(x-> in(x,theo) ,spec))
		#println(join(spec,"-"))
		println(join([x,y,z],"-"))
	end
end

example = rand(1:10,1000)

function table(example::Array{Int64,1})
	uexample = unique(example)
	counts = map(x->count(y->x==y,example),uexample)
	counts
end

@time table(example)
@time countmap(example)

