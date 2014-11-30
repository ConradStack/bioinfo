import Base.complement
using StatsBase: countmap


##### Type declarations:
# type EqualLengthError <: ArgumentException
# 	var::Symbol
# end
# Base.showerror(io::IO, e::EqualLengthError) = print(io, e.var, " not defined");

#### General Utilities:

# overload string concatenation operator
function *(c1::Char,c2::Char)
	*(string(c1),string(c2))
end
function *(s::String,c::Char)
	*(s,string(c))
end
*(s::String, carr::Array{Char,1}) = map(x->s*x, carr )



#### RNA and Amino-acid specific functions:

# read and return Codon to Amino-Acid conversion table
function getAAtable(ff::ASCIIString="AA_translation_table.txt",keycol=3,valcol=2)
	aa_raw = readdlm(ff,'\t')
	aa_tab = Dict{ASCIIString,Char}(zip(aa_raw[:,3],map(x->x[1],aa_raw[:,2])))
	return aa_tab
end

function reverseAAtable(aatable::Dict{ASCIIString,Char})
	aakeys = collect(keys(aatable))
	aavals = collect(values(aatable))
	aarev = Dict{Char,Array{ASCIIString,1}}()
	for xx in unique(aavals)
		inds = findin(aavals,xx)
		aarev[xx] = aakeys[inds]
	end
	aarev
end

# read and return the counts of each Amino-acid in a conversion table:
function getAAcounts(aatable::Dict{ASCIIString,Char})
	aavals = values(aatable)
	uaavals = unique(aavals)
	aacounts = Dict{Char,Int64}( zip(uaavals, zeros(Int64,length(uaavals))) )
	for xx in uaavals
		aacounts[xx] = count(x->x==xx,aavals)
	end
	aacounts
end

# String Transcription and reverse-Transcription
transcribe(dna::ASCIIString) = replace(dna,'T','U')
reverse_transcribe(rna::ASCIIString) = replace(rna,'U','T')

# String Translation and reverse-Translation
function translate(rna::ASCIIString,aatable::Dict{ASCIIString,Char},frame=1)
	nchop = (length(rna[frame:end])%3)
	if nchop != 0
		warn(@sprintf("RNA string length not a multiple of 3, removing %d character(s) from end",nchop))
	end
	starts = frame:3:length(rna)
	aastr = ""::ASCIIString
	for ii in starts
		aastr *= aatable[rna[ii:(ii+2)]]
	end
	return aastr
end


#### Peptide specific functions:

#  Combinatorics:  count the number of subpeptides
count_peptides_linear(N::Int) = 1+sum(map(x->N-x,0:N))
count_peptides_cyclic(N::Int) = binomial(N,2)*2  # rationale:  Binomial(N,2) -> all pairs of AA-AA bond cuts, |> *2 -> two subpeptides result from each cut


# Read integer mass table (assumes a specific format):
function get_mass_table(ff::ASCIIString="integer_mass_table.txt")
	tmpin = readdlm(ff,' ')
	Dict{Char,Int64}( zip(map(x->x[1],tmpin[:,1]),tmpin[:,2] ))
end

# Computing integer mass of a peptide
integer_mass(aa::Char, masses::Dict{Char,Int64}) = getindex(masses,aa)
integer_mass(aa::ASCIIString, masses::Dict{Char,Int64}) = mapreduce(x-> getindex(masses,x),+, aa)

# Calculate the mass spectrum from an array of Int64, which represent AA masses
function mass_spectrum(intmass::Array{Int64,1}, k::Integer, linear::Bool)
	N = length(intmass)
	if N==k
		return Int64[sum(intmass)]
	elseif k == 1
		return intmass
	else
		lin = 1:ifelse(linear,N-k+1,N)
		retval = zeros(Int64,length(lin))
		iters = linear ? [1:4] : vcat(lin,lin[1:(k-1)])
		for i in lin
			retval[i] = sum(intmass[iters[i:(i+k-1)]])
		end
		return retval
	end
end

function mass_spectrum(masses::Array{Int64,1}, linear=false)
	spectrum = Int64[0]
	for ii in length(masses):-1:1
		spectrum = vcat(spectrum, mass_spectrum(masses,ii,linear))
	end
	return spectrum
end

function mass_spectrum(str::ASCIIString, mass::Dict{Char,Int64})
	allints=map(x->integer_mass(x,mass),collect(str))
	mass_spectrum(allints)
end


function score(aa::Array{Int64,1}, bb::Array{Int64,1})
	acounts = countmap(aa)
	bcounts = countmap(bb)
	uboth = intersect( keys(acounts) , keys(bcounts) )
	retval = 0
	if length(uboth) > 0
		countsintersect = [ min(acounts[x],bcounts[x]) for x in uboth ]
		retval = sum(countsintersect)
	end
	retval
end


# step 4 - generating spectra


#### DNA specific functions:

# Reverse Complement
function compchar(cc::Char, ambi::Char='?', extended=true)
	retval = ambi
	c1 = uppercase(cc)
	if c1 == 'A'  		
		retval = 'T' # A->T
	elseif c1 == 'T' || c1 == 'U' 	
		retval = 'A' # T->A
	elseif c1 == 'C' 	
		retval = 'G' # C->G
	elseif c1 == 'G'
		retval = 'C' # G->C
	elseif c1 == 'N'
		retval = 'N' # N->N
	elseif c1 == 'Y' && extended
		retval = 'R' # Y->R
	elseif c1 == 'R' && extended
		retval = 'Y' # R->Y
	elseif c1 == 'W' && extended
		retval = 'W' # W->W	
	elseif c1 == 'S' && extended
		retval = 'S' # s->s
	elseif c1 == 'K' && extended
		retval = 'M' # K->M
	elseif c1 == 'M' && extended
		retval = 'K' # M->k
	elseif c1 == 'D' && extended
		retval = 'H' # D->H
	elseif c1 == 'H' && extended
		retval = 'D' # H->D
	elseif c1 == 'V' && extended
		retval = 'B' # V->B
	elseif c1 == 'B' && extended
		retval = 'V' # B->V
	end
	
	isupper(cc) ? retval : lowercase(retval)
end

# function that tries to complements every base in str
function complement(str::ASCIIString)
	map(compchar,str)
end

# Reverse Complement function (shortened)
rc(xx::ASCIIString) = reverse(complement(xx))

# Compute the Hamming distance (no. of mismatches) between two strings of equal length
function dist(aa::ASCIIString, bb::ASCIIString)
	compared = (length(aa)==length(bb)) ? map(==,aa,bb) : throw(ArgumentError("aa and bb arguments should be of the same length"))
	length(aa) - sum(int(compared))
end

# Locate all instances of a pattern in text (including overlapping patterns)
function locate(pattern::ASCIIString, text::ASCIIString, adjust::Integer=0)
	mit = eachmatch(Regex(pattern),text,true)
	map(x->x.offset+adjust, mit)
end







