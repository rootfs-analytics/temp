library(quantmod)
library(BreakoutDetection)
nor <- function(x, max, min)
{
   return ((x-min)/(max-min))
}
bo <- function (x, y, len = 1000, size = 30)
{
   a = getSymbols(x,auto.assign=FALSE) 
   b = getSymbols(y,auto.assign=FALSE) 
   aa = as.vector(Ad(a))
   bb = as.vector(Ad(b))
   l1 = length (aa)
   l2 = length (bb)
   l = min (c(l1, l2, len))
   aaa = tail (aa, l)
   bbb = tail (bb, l)
   c = aaa - bbb
   res = breakout(c, min.size=size, method='multi', beta=.001, degree=1, plot=TRUE)
   res
}