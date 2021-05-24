# EXAMPLES/TEST from sim.R
if ( FALSE ) {
  
  library(rethinking)
  data(cars)
  
  fit <- map(
    alist(
      dist ~ dnorm(mu,sigma),
      mu <- a + b*speed
    ),
    data=cars,
    start=list(a=30,b=0,sigma=5)
  )
  
  pred <- link(fit,data=list(speed=0:30),flatten=TRUE)
  
  sim.dist <- sim(fit,data=list(speed=0:30))
  
  plot( dist ~ speed , cars )
  mu <- apply( pred , 2 , mean )
  mu.PI <- apply( pred , 2, PI )
  y.PI <- apply( sim.dist , 2 , PI )
  lines( 0:30 , mu )
  shade( mu.PI , 0:30 )
  shade( y.PI , 0:30 )
  
  # now map2stan
  
  cars$y <- cars$dist # Stan doesn't allow variables named 'dist'
  
  m <- map2stan(
    alist(
      y ~ dnorm(mu,sigma),
      mu <- a + b*speed,
      a ~ dnorm(0,100),
      b ~ dnorm(0,10),
      sigma ~ dunif(0,100)
    ),
    data=cars,
    start=list(
      a=mean(cars$y),
      b=0,
      sigma=1
    ),
    warmup=500,iter=2500
  )
  
  pred <- link(m,data=list(speed=0:30),flatten=TRUE)
  
  sim.dist <- sim(m,data=list(speed=0:30),n=0)
  
  plot( dist ~ speed , cars )
  mu <- apply( pred , 2 , mean )
  mu.PI <- apply( pred , 2, PI )
  y.PI <- apply( sim.dist , 2 , PI )
  lines( 0:30 , mu )
  shade( mu.PI , 0:30 )
  shade( y.PI , 0:30 )
  
  
} #EXAMPLES
