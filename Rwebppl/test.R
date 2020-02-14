library(rwebppl)
library(tidyverse)

my_model <- '
   var model = function () {
      var a = flip(0.3)
      var b = flip(0.6)
      return a + b
   }
   model()
'
webppl(my_model)

my_model <- '
    var model = function(){
        var p = uniform(0, 1)
        map(function(d){
           observe(Binomial({n: d.n, p: p}), d.k)
        }, myDF)
	  return p
    }
    Infer({model: model, method: "MCMC", samples: 10000})
'

df <- tibble(
  n = c(9,5,3),
  k = c(6,3,2)
)

norm_dist <- function(result){
  df <- result %>%
    mutate(p_hypo = value, 
           unsd_prob = exp(score), 
           prob = unsd_prob / sum(unsd_prob)) %>% 
    select(p_hypo, prob)
  
  df %>% 
    ggplot(aes(x=p_hypo, y=prob))+ 
    geom_line()  
}

webppl(my_model, data = df, data_var = "myDF") %>% norm_dist()


webppl(program_file = "./particle_decay.wppl", 
       data = c(1.5, 2, 3, 4, 5, 12), data_var = "data") %>%
  norm_dist()


