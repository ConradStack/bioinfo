
require("src/seq.jl")

theo = [0,113,128,186,241,299,314,427]

mass = get_mass_table()
aa2mass(aa::Char) = integer_mass(aa,mass)
#effect = map(aa2mass,collect(aa))

masses = unique(values(mass))


for x=masses,y=masses,z=masses
	spec = mass_spectrum([x,y,z])
	if all(map(x-> in(theo,x) ,spec))
		println(join(spec,"-"))
	end
end



