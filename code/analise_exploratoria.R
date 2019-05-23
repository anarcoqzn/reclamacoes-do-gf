install.packages("tidyverse")
install.packages("ggplot2")
install.packages("magrittr")

library(tidyverse)
library(ggplot2)
library(magrittr)

avaliacoes <- read.csv(file = "data/3-avaliacao-humana/avaliacoes20190515.csv")

avaliacoes <- avaliacoes %>% 
    select(avaliador = `Matricula`, 
           id = `ID da reclamação`, 
           insatisfacao = `Grau de insatisfação`)

avaliacoes %>% 
    filter((id %in% 1:5 ))