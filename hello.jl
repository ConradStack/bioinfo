
### hello.c:
#include<stdio.h>
#
# void Print_Squares(void)
# {
#   int i;
#   for(i=1; i <=5; i++)
#   {
#     printf("%d ", i*i);
#   }
#   printf("\n");
# }

### compile into shared library:
# gcc -Wall -fPIC -shared -O2 hello.c -o hello.dylib

### call from julia:
ccall((:Print_Squares, "hello"),Void,())

