#EXAMPLES/TESTS from plp_plots.R
if (FALSE) {
  
  library(rethinking)
  data(chimpanzees)
  d <- chimpanzees
  
  flist4 <- alist(
    pulled.left ~ dbinom( 1 , p ),
    logit(p) <- a + b*prosoc.left ,
    a ~ dnorm(0,1),
    b ~ dnorm(0,1)
  )
  
  m <- map( flist4 , data=d , start=list(a=0,b=0) )
  
  
  
}
