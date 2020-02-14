library(rethinking)

data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

flist <- alist(
  height ~ dnorm( mu , sigma ) ,
  mu ~ dnorm( 178 , 0.1 ) ,
  sigma ~ dunif( 0 , 50 )
)

start <- list(
  mu=mean(d2$height),
  sigma=sd(d2$height)
)


m4.2 <- rethinking::map(flist , data=d2, start = start)
precis(m4.2)