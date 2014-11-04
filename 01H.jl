
##########################################
# Most frequent k-mers, with up to d mismatches
#

require("/Users/conrad/courses/BioinformaticsOne/src/kmer.jl")
require("/Users/conrad/courses/BioinformaticsOne/src/seq.jl")


genome = "ACGTTGCATGTCGCATGATGCATGAGAGCT"
kmer = repeat("N",4)
dmax = 1


genome = "CTTGCCGGCGCCGATTATACGATCGCGGCCGCTTGCCTTCTTTATAATGCATCGGCGCCGCGATCTTGCTATATACGTACGCTTCGCTTGCATCTTGCGCGCATTACGTACTTATCGATTACTTATCTTCGATGCCGGCCGGCATATGCCGCTTTAGCATCGATCGATCGTACTTTACGCGTATAGCCGCTTCGCTTGCCGTACGCGATGCTAGCATATGCTAGCGCTAATTACTTAT"
kmer = repeat("N",9)
dmax = 3


genome = "CCAGCTAGCCCAACAACACAGCAGACAACATACCACCAACAACAGCGCGCACAGCGCCAGCAGTATATAACAACATAGCACACAGTAGCACACAGTACCACAGGCCAGCCATACAGCCACAGTACCAGCACACCACCAGCGCCAGACAGCCAGTAACACAGACATACCATAACACAGCCAACACCACCACCACAGCCATACAGACAACAACAGCGCTAGCCAGGCCAGCAGCCATACCA"
kmer = repeat("N",9)
dmax = 3



dict = buildmerdict(genome,length(kmer));
napprox = map( y-> count_d(dict,y[1],dmax) + count_d(dict,rc(y[1]),dmax), dict);
apmax = maximum(napprox);
kmax = collect(keys(dict))[find((x)->x==apmax,napprox)];



#join(kmax," ")

# Other shit
alpha = ['A','G','T','C']
alpha_try = reduce(vcat,map(x-> collect(permutations(x)),(combinations(alpha,dmax))))
nsize = length(kmer)
positions = combinations(range(1,nsize),dmax)
kext = deepcopy(kmax)
ktried = vcat(collect(keys(dict)),map(rc,keys(dict)))


all_keys = collect(keys(dict))
add_keys = all_keys[find(x->x>=quantile(napprox,0.75),napprox)]
new_keys = ASCIIString[]

for xN in positions
	println(xN)
	for ktest in add_keys
		ktmp = collect(ktest)
		for perm in alpha_try
			ktmp[xN] = perm
			kt = join(ktmp,"")
			if !haskey(dict,kt)
				push!(new_keys,kt)
			end
		end
	end
end

new_keys = unique(new_keys)
#new_keys = setdiff(new_keys,map(rc,new_keys))
new_count = map( y-> count_d(dict,y,dmax) + count_d(dict,rc(y),dmax), new_keys);
new_max = maximum(new_count);
new_maxkeys = new_keys[find(x->x==new_max,new_count)]

if new_max == apmax
	push!(kext,new_maxkeys)
elseif new_max > apmax
	kext=new_maxkeys
else
	print("First time worked best")
end

#kext = unique(vcat(kext,map(rc,kext)))
join(kext," ")

# tmp = open(fout,"w+")
# write(tmp,join(keep,' '))
# close(tmp)




