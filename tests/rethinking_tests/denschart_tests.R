# test code coming from denschart.R
if (FALSE) {
  
  library(rethinking)
  data(chimpanzees)
  
  # don't want any variables with NAs
  d <- list( 
    pulled_left = chimpanzees$pulled_left ,
    prosoc_left = chimpanzees$prosoc_left ,
    condition = chimpanzees$condition ,
    actor = as.integer( chimpanzees$actor ) ,
    blockid = as.integer( chimpanzees$block )
  )
  
  # RStan fit
  m2 <- map2stan(
    alist(
      pulled_left ~ dbinom(1,theta),
      logit(theta) <- a + bp*prosoc_left + bpc*condition*prosoc_left ,
      a ~ dnorm(0,10),
      bp ~ dnorm(0,10),
      bpc ~ dnorm(0,10)
    ) ,
    data=d, chains=2, cores=1 )
  
  post <- extract.samples(m2)
  
  denschart( post , adjust=1 , color="slateblue" , border=NA )
  
  
  # now RStan fit of model with varying intercepts on actor
  m3 <- map2stan(
    alist(
      pulled_left ~ dbinom(1,theta),
      logit(theta) <- a + aj[actor] + bp*prosoc_left + bpc*condition*prosoc_left,
      aj[actor] ~ dnorm( 0 , sigma_actor ),
      a ~ dnorm(0,10),
      bp ~ dnorm(0,10),
      bpc ~ dnorm(0,10),
      sigma_actor ~ dcauchy(0,1)
    ) ,
    data=d,
    iter=5000 , warmup=1000 , chains=2 , cores=1 )
  
  post2 <- extract.samples(m3)
  
  denschart( post2 , xlim=c(-4,9) , color="slateblue" , alpha=0.3 )
  
  
  # now random slopes
  
  m6 <- map2stan(
    alist(
      # likeliood
      pulled_left ~ dbinom(1,p),
      
      # linear models
      logit(p) <- A + (BP + BPC*condition)*prosoc_left,
      A <- a + a_actor[actor],
      BP <- bp + bp_actor[actor],
      BPC <- bpc + bpc_actor[actor],
      
      # adaptive prior - non-centered
      c(a_actor,bp_actor,bpc_actor)[actor] ~
        dmvnormNC(sigma_actor,Rho_actor),
      
      # fixed priors
      c(a,bp,bpc) ~ dnorm(0,1),
      sigma_actor ~ dcauchy(0,2),
      Rho_actor ~ dlkjcorr(4)
    ) , data=d , iter=5000 , warmup=1000 , chains=3 , cores=3 )
  
  post6 <- extract.samples(m6)
  
  denschart( post6 , xlim=c(-4,7) , color="orange" , alpha=0.5 , drop_matrices=FALSE , label_vectors=FALSE )
  
  # post-process intercepts to show correlation in uncertainty
  
  post6$a_actor_T <- post6$a_actor
  for ( i in 1:7 ) post6$a_actor_T[,i] <- post6$a_actor[,i] + post6$a
  
  denschart( post6 , pars=c("a","a_actor","a_actor_T") , xlim=c(-4,7) , color="orange" , alpha=0.5 , drop_matrices=FALSE )
  
}
