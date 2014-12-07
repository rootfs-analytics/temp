library(quantmod)
library(BreakoutDetection)
library(ecp)
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
   #ar = Hi(a) - Lo(a)
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
   for (i in size:len)
   {
      if (res$F[i] > max(res$F[(i-size):(i-1)])){
	    points(Cl(a)[i],col="blue")
	  }
   }
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

test_ecp = function (a = 1)
{
p1 <- rnorm(100)
p2 <- rnorm(100,0,3)
p3 <- rnorm(100,2,1)
p4 <- rnorm(100,2,4)
p <- c(p1,p2,p3,p4)
x <- matrix(p,ncol=1)
plot(x,type="l")
ret = e.divisive(x, alpha = a)
abline(v=ret$order.found, col="red")
return (ret)
}

wbo_ecp <- function (x="^VIX", len = 500, size = 30, ...)
{
   a = tail(getSymbols(x,auto.assign=FALSE),len)
   ar = Ad(a)
   c = as.vector(ar)
   m = matrix(c, ncol = 1)
   res = e.divisive(m, alpha = 1, sig.lvl = 0.05, R = 499, min.size = 10)
   plot(a,type='candles',main = x)
   points(Cl(a)[res$order.found - 1],col="red")
 
   return (res)
}

