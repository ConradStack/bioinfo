
require("/Users/conrad/courses/BioinformaticsOne/seq.jl")
fname = "/Users/conrad/Downloads/dataset_9_3.txt"
fin = open(fname,"r")
str1=readline(fin)[1:end-1]
str2=readline(fin)[1:end-1]
dist(str1,str2)
close(fin)


