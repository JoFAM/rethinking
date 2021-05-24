# EXAMPLES from map2stan.R
if ( FALSE ) {
  
  library(rethinking)
  
  # simulate data
  library(MASS)
  N <- 500 # 1000 cases
  J <- 20 # 100 clusters
  J2 <- 10
  NperJ <- N/J
  sigma <- 2 # top-level standard deviation
  mu <- c(10,-0.5) # means of varying effects coefficients
  x <- runif(N,min=-2,max=2) # predictor variable
  x2 <- runif(N,min=-2,max=2)
  id <- rep( 1:J , each=NperJ ) # cluster id's
  id2 <- rep( 1:J2 , each=N/J2 )
  Sigma <- matrix( 0 , nrow=2 , ncol=2 ) # var-cov matrix
  Sigma[1,1] <- 2
  Sigma[2,2] <- 0.2
  Sigma[1,2] <- Sigma[2,1] <- -0.8 * sqrt( Sigma[1,1] * Sigma[2,2] )
  beta <- mvrnorm( J , mu=mu , Sigma=Sigma )
  y <- rnorm( N , mean=beta[id,1]+beta[id,2]*x , sd=sigma )
  y2 <- rbinom( N , size=1 , prob=logistic( y-8 ) )
  
  # fitting tests
  
  
  # cross classified
  f <- list(
    y ~ dnorm(mu,sigma),
    mu ~ a + aj1 + aj2 + b*x,
    aj1|id ~ dnorm( 0 , sigma_id ),
    aj2|id2 ~ dnorm( 0 , sigma_id2 ),
    a ~ dnorm(0,10),
    b ~ dnorm(0,1),
    sigma ~ dcauchy(0,1),
    sigma_id ~ dcauchy(0,1),
    sigma_id2 ~ dcauchy(0,1)
  )
  startlist <- list(a=10,b=0,sigma=2,sigma_id=1,sigma_id2=1,aj1=rep(0,J),aj2=rep(0,J2))
  m <- map2stan( f , data=list(y=y,x=x,id=id,id2=id2) , start=startlist , sample=TRUE , debug=FALSE )
  
  
  # random slopes with means inside multi_normal
  f <- list(
    y ~ dnorm(mu,sigma),
    mu ~ aj + bj*x,
    c(aj,bj)|id ~ dmvnorm( c(a,b) , Sigma_id ),
    a ~ dnorm(0,10),
    b ~ dnorm(0,1),
    sigma ~ dcauchy(0,1),
    Sigma_id ~ inv_wishart(3,diag(2)) # or dinvwishart
  )
  startlist <- list(a=10,b=0,sigma=2,Sigma_id=diag(2),aj=rep(0,J),bj=rep(0,J))
  m2 <- map2stan( f , data=list(y=y,x=x,id=id) , start=startlist , sample=TRUE , debug=FALSE )
  
  cat(m$model)
  
  
  # 
  f2 <- list(
    y ~ dnorm(mu,sigma),
    mu ~ a + aj + b*x,
    aj|id ~ dnorm( 0 , sigma_a ),
    a ~ dnorm(0,10),
    b ~ dnorm(0,1),
    sigma ~ dcauchy(0,1),
    sigma_a ~ dcauchy(0,1)
  )
  startlist <- list(a=10,b=0,sigma=3,sigma_a=1,aj=rep(0,J))
  m <- map2stan( f2 , data=list(y=y,x=x,id=id) , start=startlist , sample=TRUE , debug=FALSE )
  
  # now with fixed effect inside prior
  f4 <- list(
    y ~ dnorm(mu,sigma),
    mu ~ aj + b*x,
    aj|id ~ dnorm( a , sigma_a ),
    a ~ dnorm(0,10),
    b ~ dnorm(0,1),
    sigma ~ dcauchy(0,1),
    sigma_a ~ dcauchy(0,1)
  )
  startlist <- list(a=10,b=0,sigma=2,sigma_a=1,aj=rep(0,J))
  m2 <- map2stan( f4 , data=list(y=y,x=x,id=id) , start=startlist , sample=TRUE , debug=FALSE )
  
  # random slopes
  f <- list(
    y ~ dnorm(mu,sigma),
    mu ~ a + aj + (b+bj)*x,
    c(aj,bj)|id ~ dmvnorm( 0 , Sigma_id ),
    a ~ dnorm(0,10),
    b ~ dnorm(0,1),
    sigma ~ dcauchy(0,1),
    Sigma_id ~ inv_wishart(3,diag(2)) # or dinvwishart
  )
  startlist <- list(a=10,b=0,sigma=2,Sigma_id=diag(2),aj=rep(0,J),bj=rep(0,J))
  m <- map2stan( f , data=list(y=y,x=x,id=id) , start=startlist , sample=TRUE , debug=FALSE )
  
  
  f3 <- list(
    y ~ dbinom(1,theta),
    logit(theta) ~ a + aj + b*x,
    aj|id ~ dnorm( 0 , sigma_a ),
    a ~ dnorm(0,10),
    b ~ dnorm(0,1),
    sigma_a ~ dcauchy(0,1)
  )
  
  
  
  f5 <- list(
    y ~ dnorm(mu,sigma),
    mu ~ aj + bj*x,
    c(aj,bj)|id ~ dmvnorm( c(a,b) , Sigma_id ),
    a ~ dnorm(0,10),
    b ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  )
  
  
} #EXAMPLES
