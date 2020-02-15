library(rethinking)
library(magrittr)
library(tidyverse)

data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]
d2_tibble <- as_tibble(d2)

flist <- alist(
  height ~ dnorm( mu , sigma ) ,
  mu <-  a + b * weight,
  a ~ dnorm( 178 , 100 ) ,
  b ~ dnorm( 0 , 10 ) ,
  sigma ~ dunif( 0 , 50 )
)

m4.3 <- rethinking::map( flist , data=d2 )
precis(m4.3, corr = T)

d2_tibble %>% 
  mutate(weight.c = weight - mean(weight))

# Sampling from the map distribution
post <- extract.samples( m4.3 , n=1e4 )
head(post)
precis(post)

post %>% 
  as_tibble %>% 
  ggplot(aes(x=a, y=b))+
  geom_point(alpha=0.1)

