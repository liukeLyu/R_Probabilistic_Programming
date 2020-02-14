# Install rethinking package
# install.packages(c("coda","mvtnorm","devtools","loo"))
# library(devtools)
# devtools::install_github("rmcelreath/rethinking")

# library(tidyverse)

ways <- c(0,3,8,9,0)
ways / sum(ways)

dbinom(6 , size=9 , prob=0.5)

grid_method <- function(w, n, Ngrid=20, prior_type="uniform"){
# define grid
p_grid <- seq( from=0 , to=1 , length.out=Ngrid )
# define prior
prior <- switch(prior_type,
                uniform = rep( 1 , Ngrid ),
                half = ifelse( p_grid < 0.5 , 0 , 1 ),
                peak = exp( -5*abs( p_grid - 0.5 ) ))
# prior <- rep( 1 , Ngrid )
# prior <- ifelse( p_grid < 0.5 , 0 , 1 )
# prior <- exp( -5*abs( p_grid - 0.5 ) )
# compute likelihood at each value in grid
likelihood <- dbinom( w , size=n , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )
}

grid_method(6,10,30)

library(rethinking)

globe.qa <- map(
  list(
    w ~ dbinom(9,p) , # binomial likelihood
    p ~ dunif(0,1) # uniform prior
  ) ,
  data=list(w=6) )
# display summary of quadratic approximation
precis( globe.qa )


# analytical calculation
w <- 6
n <- 9
curve( dbeta( x , w+1 , n-w+1 ) , from=0 , to=1 )
# quadratic approximation
curve( dnorm( x , 0.67 , 0.16 ) , lty=2 , add=T )

# try curve function
# curve(sin(x*log(x)),0,1)

grid_method(3,3, prior_type = "half", Ngrid = 50)
grid_method(3,4, prior_type = "half", Ngrid = 50)
grid_method(5,7, prior_type = "half", Ngrid = 50)

