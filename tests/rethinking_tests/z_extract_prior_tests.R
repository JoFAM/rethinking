#### TESTS from z_extract_prior.R #############
if ( FALSE ) {
  
  # simple
  library(rethinking)
  data(cars)
  flist <- alist(
    dist ~ dnorm( mu , sigma ) ,
    mu <- a+b*speed ,
    a ~ dnorm(0,0.1) , 
    b ~ dlnorm(-1,1) ,
    sigma ~ dunif(0,40)
  )
  fit <- map( flist , start=list(a=40,b=0.1,sigma=20) , data=cars )
  
  prior <- extract.prior(fit,1e4)
  
  par(mfrow=c(1,2))
  
  mu <- link( fit , data=list(speed=0:25) , post=prior )
  plot( 0:25 , apply(mu,2,mean) , ylim=c(0,max(cars$dist)) )
  muci <- apply( mu , 2, PI )
  shade( muci , 0:25 )
  mtext("prior")
  
  mu <- link(fit,data=list(speed=0:25))
  plot( 0:25 , apply(mu,2,mean) , ylim=c(0,max(cars$dist)) )
  muci <- apply( mu , 2, PI )
  shade( muci , 0:25 )
  mtext("posterior")
  
  # test vectorized parameters
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
  
  m <- map(
    alist(
      pulled_left ~ dbinom(1,theta),
      logit(theta) <- a + aj[actor] + bp*prosoc_left + bpc*condition*prosoc_left,
      aj[actor] ~ dnorm( 0 , 1 ),
      a ~ dnorm(0,2),
      bp ~ dnorm(0,1),
      bpc ~ dnorm(0,1)
    ) ,
    data=d )
  
  prior <- extract.prior(m)
  
  p <- link( m , data=data.frame(actor=1,prosoc_left=0,condition=0) , post=prior )
  
  dens(p[,1])
  
  prior <- extract.prior(m , pars=c("a","bp") )
  
  prior <- extract.prior(m , pars=c("a","aj") )
  
  # map2stan tests
  
  m <- map2stan(
    alist(
      pulled_left ~ dbinom(1,theta),
      logit(theta) <- a + aj[actor] + bp*prosoc_left + bpc*condition*prosoc_left,
      aj[actor] ~ dnorm( 0 , sigma_actor ),
      a ~ dnorm(0,10),
      bp ~ dnorm(0,10),
      bpc ~ dnorm(0,10),
      #sigma_actor ~ dcauchy(0,1)
      sigma_actor ~ dexp(1)
    ) ,
    data=d,
    iter=1000 , chains=2 , cores=1 , sample=FALSE )
  
  prior <- extract.prior( m , n=1e4 )
  precis(prior,depth=2)
  post <- extract.samples( m )
  precis(post,depth=2)
  
  par_list <- c("a","bp","bpc","sigma_actor","aj")
  plot( precis(prior,2,pars=par_list),col.ci="gray")
  plot( precis(post,2,pars=par_list),add=TRUE,pch=16)
  
  m2 <- map2stan(
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
      #sigma_actor ~ dcauchy(0,1),
      sigma_actor ~ dexp(1),
      Rho_actor ~ dlkjcorr(4)
    ) , data=d , iter=1000 , chains=2 , cores=1 )
  
  prior <- extract.prior( m2 , n=1e3 )
  precis(prior,depth=2)
  post <- extract.samples( m2 )
  precis(post,depth=2)
  
  par_list <- c("a","bp","bpc","sigma_actor","a_actor","bp_actor","bpc_actor")
  plot( precis(prior,2,pars=par_list),col.ci="gray")
  plot( precis(post,2,pars=par_list),add=TRUE,pch=16)
  
  
}
