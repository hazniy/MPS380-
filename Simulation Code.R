pairs.trade.sim <- function(n, # length 
                            k, # change point
                            priorA=10, # first value of A
                            VRW=1, # variance of the RW
                            phi=0.5, # AR(1) coefficient
                            V=1, # variance of stationary increment 
                            W=1, # variance
                            a=1, 
                            b=0.8 
                            ){
  
  # random walk for asset A 
  pA <- pB <- rep(0, n)
  pA[1] <- priorA
  for(t in 2:n)
  {
    pA[t] <- pA[t-1] + rnorm(1, 0, sd=sqrt(VRW))
  }
  
  # AR(1) for asset B 
  X <- arima.sim(k, model=list(ar=phi), sd=sqrt(V) )
  # 
  pB[1:k] <- pA[1:k] + X 
  pB[(k+1):n] <- a+b*((k+1):n) + rnorm(n-k, 0, sd=sqrt(W))
  
  return(list(pA=pA, pB=pB))
}
