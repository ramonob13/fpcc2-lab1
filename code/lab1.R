library(dplyr)
library(readr)
library(here)

# Código base -------------------------------------------------------------

# lendo arquivo de dados
turma_fpcc2 <- read_csv(here("data/dados-fpcc2.csv"))

# visão inicial
glimpse(turma_fpcc2)
head(turma_fpcc2)