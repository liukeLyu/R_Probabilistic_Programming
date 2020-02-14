library(rethinking)
library(tidyverse)
library(magrittr)
library(purrr)

pos <- replicate(1000, sum(runif(16,-1,1)))
hist(pos)
plot(density(pos))

growth <- replicate(1e4, prod( 1 + runif(12,0,0.1) ))
dens(growth)

big <- replicate(1e4, prod( 1 + runif(12,0,0.5) ))
dens(big)

small <- replicate(1e4, prod( 1 + runif(12,0,0.01) ))
dens(small)

log.big <- replicate( 10000 , log(prod(1 + runif(12,0,0.5))) )
dens(log.big)

dnorm(0,0,0.1)

data("Howell1")
d <- as_tibble(Howell1)
d2 <- d %>% filter(age >= 18)
dens(d2$height)

curve(dnorm(x, 178, 20), from = 100, to = 250)

tibble(
  sample_mu = rnorm(1e4, 178, 20),
  sample_sigma = runif(1e4, 0, 50),
  prior_h = rnorm(1e4, sample_mu, sample_sigma)
) %$%
  dens(prior_h)

grid_approx <- function(df, mu.list, sigma.list){
  post <- expand.grid( mu=mu.list , sigma=sigma.list ) %>% 
    as_tibble %>% 
    mutate(
      LL = modify2(mu, sigma, ~sum(dnorm(df$height, .x, .y, log=T))),
      prod = LL + dnorm(mu , 178 , 20 , log=TRUE) + dunif(sigma , 0 , 50 , log=TRUE),
      prob = exp(prod - max(prod)))
  
  samples <- tibble(
    rows = sample(1:nrow(post) , size=1e4 , replace=TRUE ,prob=post$prob ),
    mu = post$mu[rows],
    sigma = post$sigma[rows])
  samples
}

mu.list <- seq( from=140, to=160 , length.out=200 )
sigma.list <- seq( from=4 , to=9 , length.out=200 )
samples <- grid_approx(d2, mu.list, sigma.list)
samples %>% 
  ggplot(aes(x = mu, y = sigma))+
  geom_jitter(alpha = 0.1, color="blue")

samples$mu %>% dens
samples$sigma %>% dens

samples$mu %>% HPDI
samples$sigma %>% HPDI

d3 <- tibble(height = sample(d2$height, size=20))
mu.list <- seq( from=140, to=170 , length.out=200 )
sigma.list <- seq( from=4 , to=20 , length.out=200 )
samples <- grid_approx(d3, mu.list, sigma.list)
samples %>% 
  ggplot(aes(x = mu, y = sigma))+
  geom_jitter(alpha = 0.1, color="blue")

samples$sigma %>% dens(norm.comp = T)

