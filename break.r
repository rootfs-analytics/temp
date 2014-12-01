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
   return (res)
}

wbo <- function (x, y = "SPY", len = 500, size = 30, ...)
{
   a = tail(getSymbols(x,auto.assign=FALSE),len)
   ar = (dailyReturn(a))
   
   #b = tail(getSymbols(y,auto.assign=FALSE),len)
   #br = dailyReturn(b)
   c = as.vector(ar)#/as.vector(br)
   #print(c)
   argList = list(...)
   rsize = argList[['rsize']];
   if (is.null(rsize)) {
	res = breakout(c, min.size=size, method='multi', beta=.001, degree=0, plot=FALSE)
   } else {
	res = breakout(c, min.size=size, min.rsize = rsize, method='multi', beta=.001, degree=0, plot=FALSE)
   }
   plot(a,type='candles',main = x)
   points(Cl(a)[res$loc+1],col="red")
   #points(Cl(a)[res$loc - res$start], col = "blue")
   #points(Cl(a)[res$loc + res$end], col = "green")
 
   return (res)
}

test_bo = function ()
{
p1 <- rnorm(100)
p2 <- rnorm(100,0,3)
p3 <- rnorm(100,2,1)
p4 <- rnorm(100,2,4)
x <- matrix(c(p1,p2,p3,p4),ncol=1)
breakout(x,method="multi",min.size=99,min.rsize=50, degree=0,plot=TRUE)
}