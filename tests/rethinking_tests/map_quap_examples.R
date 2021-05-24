# EXAMPLES from map-quap.r
if ( FALSE ) {
  
  data(cars)
  
  flist0 <- list(
    dist ~ dnorm( mean=a+b*speed , sd=sigma )
  )
  
  flist0 <- list(
    dist ~ dnorm( mean=mu , sd=sigma ) ,
    mu ~ a+b*speed
  )
  
  flist1 <- list(
    dist ~ dnorm( mean=a+b*speed , sd=sigma ) ,
    b ~ dnorm(0,1) ,
    sigma ~ dcauchy(0,1)
  )
  
  flist1 <- list(
    dist ~ dnorm( mean=mu , sd=sigma ) ,
    mu ~ a+b*speed ,
    b ~ dnorm(0,1) ,
    sigma ~ dcauchy(0,1)
  )
  
  flist2 <- list(
    dist ~ dnorm( mean=a+b*speed , sd=sigma ) ,
    c(a,b) ~ dnorm(0,10) , 
    sigma ~ dcauchy(0,1)
  )
  
  # curve( dlaplace(x,1,0) , from=-10, to=10 )
  
  flist3 <- list(
    dist ~ dnorm( mean=a+b*speed , sd=sigma ) ,
    b ~ dlaplace(1) , 
    sigma ~ dcauchy(0,1)
  )
  
  fit <- map( flist1 , start=list(a=40,b=0.1,sigma=20) , data=cars , debug=FALSE )
  
  #########
  
  library(rethinking)
  data(chimpanzees)
  
  flist1 <- list(
    pulled.left ~ dbinom( prob=logistic( a + b*prosoc.left ) , size=1 ),
    c(a,b) ~ dnorm(0,1)
  )
  
  flist2 <- list(
    pulled.left ~ dbinom( size=1 , prob=logistic( a + b*prosoc.left ) ),
    b ~ dnorm(0,1)
  )
  
  flist4 <- alist(
    pulled.left ~ dbinom( prob=p , size=1 ),
    logit(p) <- a + b*prosoc.left ,
    c(a,b) ~ dnorm(0,1)
  )
  
  fit2 <- map( flist4 , data=chimpanzees , start=list(a=0,b=0) , debug=FALSE )
  
  ########
  # regularized logistic regression example
  y <- c( rep(0,10) , rep(1,10) )
  x <- c( rep(-1,9) , rep(1,11) )
  
  flist0 <- list(
    y ~ dbinom( prob=logistic( a + b*x ) , size=1 )
  )
  
  flist1 <- list(
    y ~ dbinom( prob=logistic( a + b*x ) , size=1 ),
    c(a,b) ~ dnorm(0,10)
  )
  
  fit3a <- map( flist0 , data=list(y=y,x=x) , start=list(a=0,b=0) )
  
  fit3b <- map( flist1 , data=list(y=y,x=x) , start=list(a=0,b=0) )
  
  plot( y ~ x )
  p <- sample.naive.posterior(fit3b)
  xseq <- seq(-1,1,length.out=20)
  pi.mu <- sapply( xseq , function(x) mean(logistic(p$a+p$b*x)) )
  pi.ci <- sapply( xseq , function(x) PCI(logistic(p$a+p$b*x)) )
  lines( xseq , pi.mu )
  shade( pi.ci , xseq )
  
} #EXAMPLES
