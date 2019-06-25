library(pillar)
library(tibble)
library(tidyverse)
library(tidytext)
library(here)
library(lexiconPT)
theme_set(theme_bw())

#Selecionar arquivos
avaliacoes <- read.csv(file = "./data/3-avaliacao-humana/avaliacoes20190515.csv")
reclamacoes <-  read_csv(file = "./data/1-reclamacoes-selecionadas/reclamacoes-avaliadas.csv")

#Mudar nome de colunas de avaliacoes
avaliacoes <- avaliacoes %>% 
  select(avaliador = `Matricula`, 
         id = `ID.da.reclamação`, 
         insatisfacao = `Grau.de.insatisfação`)

#calcular insatifação com Moda
avaliacoes <- avaliacoes %>% 
  filter((insatisfacao %in% 1:5 ))

moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

avaliacao <- avaliacoes %>% select(id, insatisfacao) %>% 
  group_by(id) %>% 
  summarise(grau_insatisfacao = moda(insatisfacao), 
            avaliadores = n(),
            range.avaliacoes = (max(insatisfacao) - min(insatisfacao)))

#calcular tamanho das reclamacoes e do titulo
reclamacoes <- reclamacoes %>% mutate(reclamacao.length = str_length(reclamacao),
                                      titulo.length = str_length(titulo))

# insere coluna com número de letras em capslock
reclamacoes$numero.de.capslock <- str_count(reclamacoes$reclamacao, "\\b[A-Z]{2,}\\b")

reclamacoes_avaliacoes <- left_join(reclamacoes, avaliacao, 
                                    by = c("id" = "id"))

#Calculo de OP30 E SENT

data("oplexicon_v3.0")
data("sentiLex_lem_PT02")

op30 <- oplexicon_v3.0
sent <- sentiLex_lem_PT02

glimpse(op30)

palavra_a_palavra = reclamacoes %>% 
  select(id, reclamacao) %>% 
  unnest_tokens(termo, reclamacao)

palavra_a_palavra %>%
  select(id, termo) %>%
  head(20)

palavras_com_sentimento = palavra_a_palavra %>% 
  left_join(op30 %>% select(term, op30 = polarity), by = c("termo" = "term")) %>% 
  left_join(sent %>% select(term, sent = polarity), by = c("termo" = "term")) 

sentimentos = palavras_com_sentimento %>% 
  group_by(id) %>%
  summarise(sentimento_op30 = sum(op30, na.rm = TRUE),
            palavras_op30 = sum(!is.na(op30)),
            sentimento_sent = sum(sent, na.rm = TRUE), 
            palavras_sent = sum(!is.na(sent)), 
            palavras = n())

sentimentos %>% 
  write_csv(here("data/5-sentimentos/sentimento.csv"))

#Converter em um intervalo de [1:5]
sentimentos["conv_op_30"] <- 
  ((sentimentos$sentimento_op30 - max(sentimentos$sentimento_op30))/(min(sentimentos$sentimento_op30)-max(sentimentos$sentimento_op30)))*4 + 1 

sentimentos["conv_sent"] <- 
  ((sentimentos$sentimento_sent - max(sentimentos$sentimento_sent))/(min(sentimentos$sentimento_sent)-max(sentimentos$sentimento_sent)))*4 + 1 


#Pergunta 1: Existe alguma relação entre o tamanho da reclamação e o seu grau de insatisfação?
reclamacoes_avaliacoes["insatisfacao_op_30"] <- sentimentos["conv_op_30"]
reclamacoes_avaliacoes["insatisfacao_sent"] <- sentimentos["conv_sent"]

reclamacoes_avaliacoes %>% ggplot(aes(x=insatisfacao_op_30, y=reclamacao.length)) + geom_point()
reclamacoes_avaliacoes %>% ggplot(aes(x=insatisfacao_sent, y=reclamacao.length)) + geom_point()

comparativo <- cbind(reclamacoes_avaliacoes$grau_insatisfacao,reclamacoes_avaliacoes$insatisfacao_op_30,reclamacoes_avaliacoes$insatisfacao_sent)
comparativo <- cbind(comparativo, 0,0)
for (i in c(1:60)){
    v1 <- comparativo[i,1]
    v2 <- comparativo[i,2]
    
    comparativo[i,4] <- (v2 - v1)^2
    
    v2 <- comparativo[i,3]
    
    comparativo[i,5] <- (v2 - v1)^2
}
