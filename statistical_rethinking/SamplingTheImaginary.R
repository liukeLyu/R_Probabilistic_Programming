library(rethinking)
library(magrittr)

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
plot(samples)

plot(p_grid, posterior, type="b")
dens(samples)

sum( posterior[ p_grid < 0.5 ] )

sum(samples < 0.5) / 1e4

sum( samples > 0.5 & samples < 0.75 ) / 1e4

quantile(samples, 0.8)
quantile( samples , c( 0.1 , 0.9 ) )

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep(1,1000)
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )

plot(p_grid, posterior, type="l")

dens(samples)
PI(samples, prob = 0.5)
HPDI(samples, prob = 0.5)

p_grid[which.max(posterior)]
chainmode(samples, adj=0.01)
mean(samples)
median(samples)

plot(p_grid, posterior, type="l")
abline(v = mean(samples), col="blue")
abline(v = median(samples), col="purple")
abline(v = chainmode(samples, adj=0.01), col="red")

sum( posterior*abs( 0.5 - p_grid ) )
loss <- sapply( p_grid , function(d) sum( posterior*( d - p_grid )^2 ) )
plot(loss, add=T, type="l")

p_grid[ which.min(loss)]
median(samples)
mean(samples)

dbinom( 0:2 , size=2 , prob=0.7 )
rbinom( 10 , size=2 , prob=0.7 )
dummy_w <- rbinom( 1e5 , size=2 , prob=0.7 )
table(dummy_w)/1e5

dummy_w <- rbinom( 1e5 , size=9 , prob=0.7 )
table(dummy_w)/1e5
simplehist(dummy_w, xlab="dummy water count")
w <- rbinom( 1e4 , size=9 , prob=0.6 )
w <- rbinom( 1e4 , size=9 , prob=samples )

simplehist(w)

# Practice
## Easy
sum(samples < 0.2)/length(samples)
sum(samples > 0.8)/length(samples)
sum(samples > 0.2 & samples < 0.8)/length(samples)
quantile(samples, 0.2)
quantile(samples, 0.8)
HPDI(samples, prob = 0.66)
PI(samples, prob = 0.66)

## Medium
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
HPDI(samples, prob = 0.9)
w <- rbinom(1e4, size=15, prob=samples)
simplehist(w)
samples9 <- rbinom(1e4, size=9, prob=samples)
sum(samples9 == 6) / 1e4

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- ifelse( p_grid < 0.5 , 0 , 1 )
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
HPDI(samples, prob = 0.9)
w <- rbinom(1e4, size=15, prob=samples)
simplehist(w)
samples9 <- rbinom(1e4, size=9, prob=samples)
sum(samples9 == 6) / 1e4

## Hard
data(homeworkch3)
sum(birth1)+sum(birth2)

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( sum(birth1)+sum(birth2) , size=length(birth1)+length(birth2) , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
hist(samples)
chainmode(samples)
p_grid[which.max(posterior)]
HPDI(samples, prob=0.5)
HPDI(samples, prob=0.89)
HPDI(samples, prob=0.97)

w <- rbinom(1e4, size=49, prob = samples)
simplehist(w)

follow_girl = birth2[birth1 == 0]
sum(follow_girl) / length(follow_girl)

