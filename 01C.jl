

#fname = "/Users/conrad/courses/BioinformaticsOne/01C_samp.txt"
fname = "/Users/conrad/courses/BioinformaticsOne/rosalind_1c.txt"
fout = splitext(fname)[1]*".out"

fin = open(fname,"r")
pat = chomp(readline(fin))
dna = chomp(readline(fin))
mit = eachmatch(Regex(pat),dna,true)

close(fin)

a = Int64[]
for x = mit
	push!(a,x.offset-1)
end

tmp = open(fout,"w+")
write(tmp,join(a,' '))
close(tmp)

