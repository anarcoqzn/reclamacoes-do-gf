library(tidyverse)
library(ggplot2)
library(magrittr)

avaliacoes <- read.csv(file = "./data/3-avaliacao-humana/avaliacoes20190515.csv")
reclamacoes <-  read_csv(file = "data/1-reclamacoes-selecionadas/reclamacoes-avaliadas.csv")

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

reclamacoes <- reclamacoes %>% mutate(reclamacao.length = str_length(reclamacao),
                                      titulo.length = str_length(titulo))

# insere coluna com número de letras em capslock
reclamacoes$numero.de.capslock <- str_count(reclamacoes$reclamacao, "\\b[A-Z]{2,}\\b")

reclamacoes_avaliacoes <- left_join(reclamacoes, avaliacao, 
                         by = c("id" = "id"))
# A partir desses dados podemos fazer algumas perguntas que podem ser respondidas através
# da análise mais específica, considerando que o nível de concordãncia das avaliações
# é dado pela moda das avaliações.

# Existe alguma elação entre o tamanho da reclamação e o seu grau de insatisfação?
# Existe alguma relação entre o número de letras em caps lock e o seu grau de insatisfação?
# Existe relação entre o tamanho do título e o grau de insatisfação?

# Para cada uma dessas perguntas, caso exista uma relação, qual o grau de significância da
# variável predicada na variável resposta?
# É possível quantificar a relação? 
# É possível definir um modelo de regressão linear que explique essa relação?
