library(rethinking)
library(magrittr)
library(tidyverse)

data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

flist <- alist(
  height ~ dnorm( mu , sigma ) ,
  mu ~ dnorm( 178 , 20 ) ,
  sigma ~ dunif( 0 , 50 )
)

m4.1 <- rethinking::map( flist , data=d2 )
precis(m4.1)
vcov(m4.1)

m4.1 %>% vcov %>% diag %>% sqrt
m4.1 %>% vcov %>% cov2cor

# Sampling from the map distribution
post <- extract.samples( m4.1 , n=1e4 )
head(post)
precis(post)

post %>% 
  as_tibble %>% 
  ggplot(aes(x=mu, y=sigma))+
  geom_point(alpha=0.1)

# How to sample multivariate gaussian distribution directly (extract.samples uses this)
library(MASS)
post <- mvrnorm( n=1e4 , mu=coef(m4.1) , Sigma=vcov(m4.1) )
post %>% 
  as_tibble %>% 
  ggplot(aes(x=mu, y=sigma))+
  geom_point(alpha=0.1)

m4.1_logsigma <- rethinking::map(
  alist(
    height ~ dnorm( mu , exp(log_sigma) ) ,
    mu ~ dnorm( 178 , 20 ) ,
    log_sigma ~ dnorm( 2 , 10 )
  ) , data=d2 )

m4.1_logsigma %>% precis
m4.1 %>% precis

post <- extract.samples( m4.1_logsigma )

post %>% 
  as_tibble %>% 
  mutate(sigma = exp(log_sigma)) %>% 
  ggplot(aes(x=mu, y=sigma))+
  geom_point(alpha=0.1)

