###############################
#
# General utilities 
#
###############################


# clipping off its newline character from (anywhere really) in a string
# NOTE:  this is functionally equalivalent to the readchomp(x) method
# ismatch(r"\n",xx)
function clip(xx::ASCIIString) 
	return (xx[end]=='\n') ? xx[1:end-1] : xx
end



