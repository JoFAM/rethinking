# test code from drawdag.R
if (FALSE) {
  
  plot( NULL , xlim=c(-1,1) , ylim=c(-1,1) )
  circle( 0 , 0 , 1 )
  
  library(rethinking)
  library(dagitty)
  plant_dag <- dagitty( "dag {
    h0 -> h1
    f -> h1
    t -> f
}")
  coordinates( plant_dag ) <- list( x=c(h0=0,t=2,f=1,h1=1) , y=c(h0=0,t=0,f=1,h1=2) )
  
  drawdag( plant_dag , cex=1.2 , col_labels=c("red","black","red","red") , col_arrow=c("red","black","red") , goodarrow=TRUE )
  
  exdag <- dagitty( "dag {
    U [unobserved]
    z -> x -> y
    x <- U -> y
}")
  coordinates( exdag ) <- list( x=c(z=0,x=1,y=2,U=1.5) , y=c(z=0,x=0,y=0,U=-1) )
  drawdag( exdag , radius=3.8 )
  
  # drawing paths
  
  g <- dagitty( "dag { 
    x -> y
    a -> x
    b -> y
    a -> z <- b
    x [exposure]
    y [outcome] 
}" , layout=TRUE )
  
  drawdag( g , col_arrow="gray" )
  
  drawopenpaths( g , col_arrow="black" )
  drawopenpaths( g , Z=list("z") , col_arrow="black" )
  
  # xkcd example
  exdag <- dagitty( "dag {
    z -> x -> y
    x <- U -> y
}")
  coordinates( exdag ) <- list( x=c(z=0,x=1,y=2,U=1.5) , y=c(z=0,x=0,y=0,U=-1) )
  drawdag( exdag , xkcd=TRUE , lwd=1.5 )
  
  
  
}

