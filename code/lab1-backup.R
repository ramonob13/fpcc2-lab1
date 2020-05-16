library(dplyr)
library(readr)
library(here)
library(ggplot2)
library(forcats)

# Código base -------------------------------------------------------------

# lendo arquivo de dados
turma_fpcc2 <- read_csv(here("data/dados-fpcc2.csv"))

# visão inicial
glimpse(turma_fpcc2)
head(turma_fpcc2)

# Questão 1
# Calcular a média e o desvio padrão da idade e nível de
#interesse contendo as métricas calculadas para a idade e 
#nível de interesse (devem ser apresentadas de forma ordenada)

media_idades <- mean(turma_fpcc2$Idade)
desvio_idades<- sd(turma_fpcc2$Idade)
media_interesses <- mean(turma_fpcc2$`Nível de interesse na pós-gradução`)
desvio_interesses<- sd(turma_fpcc2$`Nível de interesse na pós-gradução`)

dados_ordenados <-
  summarise(turma_fpcc2,media_idades,media_interesses,desvio_idades,desvio_interesses)%>%
  arrange(desc(media_idades,media_interesses,desvio_idades,desvio_interesses))
dados_ordenados
write.csv(dados_ordenados,"dados_ordenados.csv")

#Questão 2 - Qual o curso com o maior valor médio de idade? Discutir os resultados com base
#nos dados disponíveis em termos de representatividade

cursoMaiorIdade <- turma_fpcc2 %>% 
  group_by(`Curso no PPGCC / UFCG`) %>%
  summarise(media_idade=mean(Idade)) %>%
  arrange(desc(media_idade))
cursoMaiorIdade

# Questão 3 - Crie um gráfico que mostra a idade média para cada curso no formato png
alunos_curso <- turma_fpcc2 %>%group_by(curso=turma_fpcc2$`Curso no PPGCC / UFCG`) %>%
  summarise(media_idade = mean(Idade),frequência = n())
alunos_curso

alunos_curso_idade <-alunos_curso %>%
  mutate(curso = fct_reorder(curso,media_idade)) 
alunos_curso_idade

grafico3<- ggplot(data=alunos_curso_idade)+
  geom_bar(mapping = aes(x = curso, y = media_idade), stat = "identity", 
           colour = "orange", fill = "light blue")
#+geom_text(aes(label=media_idade), vjust=-0.3, size=3.5)

grafico3 #impressão da figura
ggsave("grafico3.pdf", width = 3, height = 2) # ggsave precisa que a figura tenha sido impressa em um dispositivo de saída antes


#grafico+geom_text(size=6)
#grafico+geom_text(aes(label=media_idade), vjust=-0.3, size=3.5)

#geom_text(aes(label=round(perc*100,2)), vjust=-0.3, size=3.5)



#Questão 4 - Crie um gráfico que mostra o percentual de alunos para cada estado de origem.
# Apresente o gráfico e discuta os resultados no relatório. O gráfico também deve ser salvo
# em uma imagem no formato png

tabela_por_estado <- turma_fpcc2 %>%
  mutate(`Estado de nascimento (abreviado)` = ifelse(`Estado de nascimento (abreviado)` == "Paraíba", "PB", 
                                                     ifelse(`Estado de nascimento (abreviado)` == "Ceará", "CE",
                                                            ifelse(`Estado de nascimento (abreviado)` == "Santa Catarina", "SC", `Estado de nascimento (abreviado)`)))) %>%
  mutate(total_alunos = n()) %>% group_by(`Estado de nascimento (abreviado)`) %>%
  summarise(Percentual = (n() / first(total_alunos)) * 100)

ggplot(data = tabela_por_estado) + 
  geom_bar(mapping = aes(x = `Estado de nascimento (abreviado)`, y = Percentual), stat = "identity",
           colour = "white", fill = "orange")

ggsave("grafico4.pdf", width = 3, height = 2) # ggsave precisa que a figura tenha sido impressa em um dispositivo de saída antes

#Questão 5 
top_5_idade <- top_n(turma_fpcc2, 5, Idade)
botton_5_idade <- top_n(turma_fpcc2, -5, Idade)

media_normal_idade <- summarise(turma_fpcc2, valor=mean(Idade))
media_top_5_idade <- summarise(top_5_idade, valor = mean(Idade))
media_botton_5_idade <- summarise(botton_5_idade, valor = mean(Idade))

tabela_q5 <- data.frame(Tipo = c("Média Total", "Média Top 5", "Média Botton 5"), Media = c(media_normal_idade$valor, media_top_5_idade$valor, media_botton_5_idade$valor))

tabela_q5


##Questão  6 - Você acredita que existe uma relação entre idade e nível de interesse?
regressao <-lm(turma_fpcc2$Idade~turma_fpcc2$`Nível de interesse na pós-gradução`, data=dados_fpcc2)
regressao

grafico6<-ggplot(data = turma_fpcc2) + 
  geom_point(mapping = aes(x = `Nível de interesse na pós-gradução`, y = Idade), 
             colour = "dark blue", fill = "dark blue")
grafico6 + theme(panel.border = element_rect(linetype = "dashed", fill = NA))
#grafico6 +theme(axis.text.x=elemente_text(size=12))
ggsave("grafico6.pdf", width = 3, height = 2) # ggsave precisa que a figura tenha sido impressa em um dispositivo de saída antes

