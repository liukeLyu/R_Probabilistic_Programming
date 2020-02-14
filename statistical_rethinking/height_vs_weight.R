library(rethinking)
library(magrittr)
library(tidyverse)

data("Howell1")
Howell1 %>% 
  as_tibble %>%
  ggplot(aes(x = weight, y = height))+
  geom_point()
