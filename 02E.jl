
require("src/seq.jl")
require("src/kmer.jl")
const mass = get_mass_table()
aa2mass(aa::Char) = integer_mass(aa,mass)
const aatab = getAAtable()


import Base.Collections
#const spec = sort(vec([0 113 128 186 241 299 314 427]))
const spec = sort(vec([0 101 113 113 113 113 128 128 137 147 147 214 226 226 229 241 260 260 265 275 284 327 339 342 354 366 388 388 397 407 412 440 455 467 479 501 501 513 525 535 544 568 568 592 614 614 626 626 648 672 672 696 705 715 727 739 739 761 773 785 800 828 833 843 852 852 874 886 898 901 913 956 965 975 980 980 999 1011 1014 1014 1026 1093 1093 1103 1112 1112 1127 1127 1127 1127 1139 1240]))
const maxspec = maximum(spec)
const minaa = minimum(values(mass))
const maxsize = int(floor(maxspec/minaa))
#msub = filter( y -> in(y,spec),  unique(values(mass)))
msub = filter( (x,y) -> in(y,spec),  mass)

allaa = collect(keys(msub)) # remove from this collection....
#testqueue = deepcopy(msub)
testqueue = map(string,allaa)
testout = []


#minaa = 2
#for i in range(1,length(testqueue))
while length(testqueue) != 0
	pep = shift!(testqueue)
	if length(pep) > minaa
		break
	end
	pepmass = integer_mass(pep,mass)
	if !in(pepmass,spec)
		#@printf("removing %s",pep)
	else
		if pepmass == maxspec
			#println(pep)
			aaspec = sort(mass_spectrum(pep, mass))
			if aaspec == spec
				#@printf("GOOD! %s",pep)
				println(pep)
				push!(testout,pep)
			end 
		else 
			append!(testqueue, pep*allaa)	
		end
	end
end


for j in testout
	println(join(map(aa2mass,collect(j)),"-"))
	push!(testqueue,join(map(aa2mass,collect(j)),"-"))
end
println(join(unique(testqueue)," "))






