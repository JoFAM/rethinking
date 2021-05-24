####### TEST CODE from glimmer.R ########
if ( FALSE ) {
  
  library(rethinking)
  
  data(chimpanzees)
  f0 <- pulled.left ~ prosoc.left*condition - condition
  m0 <- glimmer( f0 , chimpanzees , family=binomial )
  
  f1 <- pulled.left ~ (1|actor) + prosoc.left*condition - condition
  m1 <- glimmer( f1 , chimpanzees , family=binomial )
  # m1s <- map2stan( m1$f , data=m1$d , sample=TRUE )
  
  f2 <- pulled.left ~ (1+prosoc.left|actor) + prosoc.left*condition - condition
  m2 <- glimmer( f2 , chimpanzees , family=binomial )
  
  data(UCBadmit)
  f3 <- cbind(admit,reject) ~ (1|dept) + applicant.gender
  m3 <- glimmer( f3 , UCBadmit , binomial )
  m3s <- map2stan( m3$f , data=m3$d )
  
  f4 <- cbind(admit,reject) ~ (1+applicant.gender|dept) + applicant.gender
  m4 <- glimmer( f4 , UCBadmit , binomial )
  m4s <- map2stan( m4$f , data=m4$d )
  
  data(Trolley)
  f5 <- response ~ (1|id) + (1|story) + action + intention + contact
  m5 <- glimmer( f5 , Trolley )
  m5s <- map2stan( m5$f , m5$d , sample=FALSE )
  
  f6 <- response ~ (1+action+intention|id) + (1+action+intention|story) + action + intention + contact
  m6 <- glimmer( f6 , Trolley )
  m6s <- map2stan( m6$f , m6$d , sample=TRUE )
  
}

