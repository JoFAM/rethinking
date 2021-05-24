# tests copied from compare.R
# TO DO: Check where to place these tests, or
# simply remove them as they shouldn't be included
# in the package in this way.
if (FALSE) {
  
  library(rethinking)
  data(chimpanzees)
  
  d <- list( 
    pulled_left = chimpanzees$pulled_left ,
    prosoc_left = chimpanzees$prosoc_left ,
    condition = chimpanzees$condition ,
    actor = as.integer( chimpanzees$actor )
  )
  
  m0 <- map(
    alist(
      pulled_left ~ dbinom(1,theta),
      logit(theta) <- a,
      a ~ dnorm(0,1)
    ) ,
    data=d,
    start=list(a=0)
  )
  
  m1 <- map(
    alist(
      pulled_left ~ dbinom(1,theta),
      logit(theta) <- a + bp*prosoc_left,
      a ~ dnorm(0,1),
      bp ~ dnorm(0,1)
    ) ,
    data=d,
    start=list(a=0,bp=0)
  )
  
  m2 <- map(
    alist(
      pulled_left ~ dbinom(1,theta),
      logit(theta) <- a + bp*prosoc_left + bpc*condition*prosoc_left,
      a ~ dnorm(0,1),
      bp ~ dnorm(0,1),
      bpc ~ dnorm(0,1)
    ) ,
    data=d,
    start=list(a=0,bp=0,bpc=0)
  )
  
  m3 <- map(
    alist(
      pulled_left ~ dbinom(1,theta),
      logit(theta) <- a + bp*prosoc_left + bc*condition + bpc*condition*prosoc_left,
      a ~ dnorm(0,1),
      bp ~ dnorm(0,1),
      bc ~ dnorm(0,1),
      bpc ~ dnorm(0,1)
    ) ,
    data=d,
    start=list(a=0,bp=0,bc=0,bpc=0)
  )
  
  ( x <- compare(m0,m1,m2,m3) )
  
  plot(x)
  
  # now map2stan
  
  m0 <- map2stan(
    alist(
      pulled_left ~ dbinom(1,theta),
      logit(theta) <- a,
      a ~ dnorm(0,1)
    ) ,
    data=d,
    start=list(a=0)
  )
  
  m1 <- map2stan(
    alist(
      pulled_left ~ dbinom(1,theta),
      logit(theta) <- a + bp*prosoc_left,
      a ~ dnorm(0,1),
      bp ~ dnorm(0,1)
    ) ,
    data=d,
    start=list(a=0,bp=0)
  )
  
  m2 <- map2stan(
    alist(
      pulled_left ~ dbinom(1,theta),
      logit(theta) <- a + bp*prosoc_left + bpc*condition*prosoc_left,
      a ~ dnorm(0,1),
      bp ~ dnorm(0,1),
      bpc ~ dnorm(0,1)
    ) ,
    data=d,
    start=list(a=0,bp=0,bpc=0)
  )
  
  m3 <- map2stan(
    alist(
      pulled_left ~ dbinom(1,theta),
      logit(theta) <- a + bp*prosoc_left + bc*condition + bpc*condition*prosoc_left,
      a ~ dnorm(0,1),
      bp ~ dnorm(0,1),
      bc ~ dnorm(0,1),
      bpc ~ dnorm(0,1)
    ) ,
    data=d,
    start=list(a=0,bp=0,bc=0,bpc=0)
  )
  
  m4 <- map2stan(
    alist(
      pulled_left ~ dbinom(1,theta),
      logit(theta) <- a + aj + bp*prosoc_left + bc*condition + bpc*condition*prosoc_left,
      a ~ dnorm(0,1),
      aj[actor] ~ dnorm(0,sigma_actor),
      bp ~ dnorm(0,1),
      bc ~ dnorm(0,1),
      bpc ~ dnorm(0,1),
      sigma_actor ~ dcauchy(0,1)
    ) ,
    data=d,
    start=list(a=0,bp=0,bc=0,bpc=0,sigma_actor=1,aj=rep(0,7))
  )
  
  ( x1 <- compare(m0,m1,m2,m3,m4,WAIC=FALSE) )
  
  ( x2 <- compare(m0,m1,m2,m3,m4,WAIC=TRUE) )
  
  plot(x1)
  
  plot(x2)
  
  
}
