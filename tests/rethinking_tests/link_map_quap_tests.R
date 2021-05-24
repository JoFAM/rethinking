# TESTS from link-map-quap
if (FALSE) {
  
  library(rethinking)
  data(chimpanzees)
  
  fit <- map(
    alist(
      pulled.left ~ dbinom( 1 , p ),
      logit(p) <- a + b*prosoc.left,
      c(a,b) ~ dnorm(0,1)
    ),
    data=chimpanzees,
    start=list(a=0,b=0)
  )
  
  pred <- link(fit)
  
  pred2 <- link(fit,data=list(prosoc.left=0:1),flatten=FALSE)
  
  sim.pulls <- sim(fit,data=list(prosoc.left=0:1))
  
  fit2 <- map2stan(
    alist(
      pulled.left ~ dbinom( 1 , p ),
      logit(p) <- a + b*prosoc.left,
      c(a,b) ~ dnorm(0,1)
    ),
    data=list(
      pulled.left=chimpanzees$pulled.left,
      prosoc.left=chimpanzees$prosoc.left
    ),
    start=list(a=0,b=0)
  )
  
  preds <- link(fit2)
  
  preds2 <- link(fit2,data=list(prosoc_left=0:1))
  
}
