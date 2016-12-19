##Formatando os testes

teste_1_2_periodo <- read_csv("~/Documents/ADII/Lab2-Parte3/teste_1_2_periodo.csv")

treino <- read_csv("teste_1_2_periodo.csv", col_types = cols(ALU_NOVAMATRICULA = col_character())) %>%
  mutate(ALU_NOVAMATRICULA = as.factor(ALU_NOVAMATRICULA))


str(treino)
summary(treino)
View(treino)
treino <- treino %>%
  arrange(ALU_NOVAMATRICULA)

#remover NAs e remover nota de reprovacoes
treino.clean <- treino %>%
  filter(!is.na(MAT_MEDIA_FINAL))
summary(treino.clean)
View(treino.clean)

#calcular cra
treino.cra <- treino.clean %>%
  group_by(ALU_NOVAMATRICULA) %>%
  mutate(cra.contrib = MAT_MEDIA_FINAL*CREDITOS) %>%
  summarise(cra = sum(cra.contrib)/sum(CREDITOS))

head(treino.cra)




treino.model.input <- treino.clean %>%
  group_by(ALU_NOVAMATRICULA,DISCIPLINA)  %>%
  filter(MAT_MEDIA_FINAL == max(MAT_MEDIA_FINAL)) %>%
  ungroup() %>%
  select(ALU_NOVAMATRICULA,DISCIPLINA,MAT_MEDIA_FINAL) %>% 
  mutate(DISCIPLINA = as.factor(gsub(" ",".",DISCIPLINA))) %>%
  dcast(ALU_NOVAMATRICULA ~ DISCIPLINA, mean) %>%
  merge(treino.cra)

head(treino.model.input)

View(treino.model.input)
##---------------------------------------------------