# test code from z_link_map2stan.R
if ( FALSE ) {
  
  library(rethinking)
  data(willowtitn)
  d <- willowtitn
  
  m <- map2stan(
    alist(
      n ~ dzipois( p , lambda ),
      log(lambda) <- an + bn*percforest ,
      logit(p) ~ ap + bp*percforest ,
      c(an,ap,bn,bp) ~ dnorm(0,10)
    ),
    data=d,
    start=list(ap=0,an=1,bn=0,bp=0)
  )
  
  pred <- link( m )
  
  d.new <- list(
    n = 1:4,
    percforest = c(10,20,30,40),
    N = 4
  )
  pred <- link( m , data=d.new )
  
  ######
  
  library(rethinking)
  data(UCBadmit)
  d <- UCBadmit
  d$male <- ifelse( d$applicant.gender=="male" , 1 , 0 )
  d$dept <- as.integer(d$dept)
  
  m <- map2stan(
    alist(
      admit ~ dbinom( applications , p ),
      logit(p) <- a + ak + b*male,
      ak[dept] ~ dnorm(0,sigma),
      a ~ dnorm(0,10),
      b ~ dnorm(0,10),
      sigma ~ dcauchy(0,2.5)
    ) ,
    data=list(admit=d$admit,applications=d$applications,male=d$male,dept=d$dept) ,
    start=list(a=0,b=0,ak=rep(0,6),sigma=1)
  )
  
  pred <- link( m )
  
  y.mean <- apply( pred$p , 2 , mean )
  y.ci <- apply( pred$p , 2 , PCI )
  
  plot( d$admit/d$applications , ylim=c(0,1) )
  lines( 1:12 , y.mean )
  shade( y.ci , 1:12 )
  
  # now with random slopes
  m2 <- map2stan(
    list(
      admit ~ dbinom( applications , p ),
      logit(p) ~ a + ak + (b+bk)*male,
      c(ak,bk)|dept ~ dmvnorm(0,Sigma),
      a ~ dnorm(0,10),
      b ~ dnorm(0,10),
      Sigma ~ inv_wishart(3,diag(2))
    ) ,
    data=list(admit=d$admit,applications=d$applications,male=d$male,dept=d$dept) ,
    start=list(a=0,b=0,ak=rep(0,6),bk=rep(0,6),Sigma=diag(2))
  )
  
  pred <- link.map2stan( m2 )
  
  y.mean <- apply( pred$p , 2 , mean )
  y.ci <- apply( pred$p , 2 , PCI )
  
  plot( d$admit/d$applications , ylim=c(0,1) )
  lines( 1:12 , y.mean )
  shade( y.ci , 1:12 )
  
  
  #####
  
  library(rethinking)
  data(cars)
  
  m <- map2stan(
    list(
      y ~ dnorm( mu , sigma ),
      mu ~ a + b*x,
      a ~ dnorm(0,10),
      b ~ dnorm(0,10),
      sigma ~ dcauchy(0,1)
    ) ,
    data=list(y=cars$dist,x=cars$speed) ,
    start=list(a=mean(cars$dist),b=0,sigma=10)
  )
  
  mu <- link.map2stan( m )
  
  y.mean <- apply( mu$mu , 2 , mean )
  y.ci <- apply( mu$mu , 2 , PCI )
  
  plot( cars$dist )
  lines( 1:50 , y.mean )
  shade( y.ci , 1:50 )
  
  # new data
  
  dn <- list( y=1:50 , x=seq(0,30,length.out=50) , N=50 )
  
  mu <- link.map2stan( m , data=dn )
  
  y.mean <- apply( mu$mu , 2 , mean )
  y.ci <- apply( mu$mu , 2 , PCI )
  
  plot( cars$speed , cars$dist )
  lines( dn$x , y.mean )
  for ( p in seq(0.1,0.99,length.out=6) ) {
    y.ci <- apply( mu$mu , 2 , PCI , prob=p )
    shade( y.ci , dn$x , col=col.alpha("slateblue",0.15) )
  }
  
}

