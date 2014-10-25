import Base.complement


##### Type declarations:
# type EqualLengthError <: ArgumentException
# 	var::Symbol
# end
# Base.showerror(io::IO, e::EqualLengthError) = print(io, e.var, " not defined");


# Reverse Complement
function compchar(cc::Char, ambi::Char='?', extended=true)
	retval = ambi
	c1 = uppercase(cc)
	if c1 == 'A'  		
		retval = 'T' # A->T
	elseif c1 == 'T' 	
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







