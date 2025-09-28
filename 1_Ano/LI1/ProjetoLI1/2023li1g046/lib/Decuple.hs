
module Decuple where

import Functions

get0 (a,b,c,d,e,f,g,h,i,j) = a
get1 (a,b,c,d,e,f,g,h,i,j) = b
get2 (a,b,c,d,e,f,g,h,i,j) = c
get3 (a,b,c,d,e,f,g,h,i,j) = d
get4 (a,b,c,d,e,f,g,h,i,j) = e
get5 (a,b,c,d,e,f,g,h,i,j) = f
get6 (a,b,c,d,e,f,g,h,i,j) = g
get7 (a,b,c,d,e,f,g,h,i,j) = h
get8 (a,b,c,d,e,f,g,h,i,j) = i
get9 (a,b,c,d,e,f,g,h,i,j) = j

set0 x (a,b,c,d,e,f,g,h,i,j) = (x,b,c,d,e,f,g,h,i,j)
set1 x (a,b,c,d,e,f,g,h,i,j) = (a,x,c,d,e,f,g,h,i,j)
set2 x (a,b,c,d,e,f,g,h,i,j) = (a,b,x,d,e,f,g,h,i,j)
set3 x (a,b,c,d,e,f,g,h,i,j) = (a,b,c,x,e,f,g,h,i,j)
set4 x (a,b,c,d,e,f,g,h,i,j) = (a,b,c,d,x,f,g,h,i,j)
set5 x (a,b,c,d,e,f,g,h,i,j) = (a,b,c,d,e,x,g,h,i,j)
set6 x (a,b,c,d,e,f,g,h,i,j) = (a,b,c,d,e,f,x,h,i,j)
set7 x (a,b,c,d,e,f,g,h,i,j) = (a,b,c,d,e,f,g,x,i,j)
set8 x (a,b,c,d,e,f,g,h,i,j) = (a,b,c,d,e,f,g,h,x,j)
set9 x (a,b,c,d,e,f,g,h,i,j) = (a,b,c,d,e,f,g,h,i,x)


ap0 f tuple = set0 (f tuple) tuple
ap1 f tuple = set1 (f tuple) tuple
ap2 f tuple = set2 (f tuple) tuple
ap3 f tuple = set3 (f tuple) tuple
ap4 f tuple = set4 (f tuple) tuple
ap5 f tuple = set5 (f tuple) tuple
ap6 f tuple = set6 (f tuple) tuple
ap7 f tuple = set7 (f tuple) tuple
ap8 f tuple = set8 (f tuple) tuple
ap9 f tuple = set9 (f tuple) tuple