library(tidyverse)
library(ggplot2)
library(magrittr)

avaliacoes <- read.csv(file = "data/3-avaliacao-humana/avaliacoes20190515.csv")

avaliacoes <- avaliacoes %>% 
    select(avaliador = `Matricula`, 
           id = `ID.da.reclamação`, 
           insatisfacao = `Grau.de.insatisfação`)

avaliacoes %>% 
    filter((id %in% 1:5 ))

moda <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

avaliacao <- avaliacoes %>% select(id, insatisfacao) %>% 
    group_by(id) %>% 
    summarise(grau_insatisfacao = moda(insatisfacao), 
              avaliadores = n(),
              range.avaliacoes = (max(insatisfacao) - min(insatisfacao)))
