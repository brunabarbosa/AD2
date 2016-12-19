install.packages("rmarkdown")

library(readr)
library(dplyr)
graduados <- read_csv("graduados.csv", col_types = cols(matricula = col_character())) %>%
  mutate(matricula = as.factor(matricula))
head(graduados)

str(graduados)
summary(graduados)
View(graduados)
graduados <- graduados %>%
  arrange(matricula)

graduados.clean <- graduados %>%
  filter(!is.na(media))

summary(graduados.clean)
View(graduados.clean)

graduados.cra <- graduados.clean %>%
  group_by(matricula) %>%
  mutate(cra.contrib = media*creditos) %>%
  summarise(cra = sum(cra.contrib)/sum(creditos))

head(graduados.cra)

library(reshape2)

graduados.model.input <- graduados.clean %>%
  group_by(matricula,disciplina)  %>%
  filter(media == max(media)) %>%
  ungroup() %>%
  select(matricula,disciplina,media) %>% 
  mutate(disciplina = as.factor(gsub(" ",".",disciplina))) %>%
  dcast(matricula ~ disciplina, mean) %>%
  merge(graduados.cra)

head(graduados.model.input)
View(graduados.model.input)

############

disciplinas <- data.frame(calc1 = graduados.model.input$Cálculo.Diferencial.e.Integral.I, 
                     vetorial = graduados.model.input$Álgebra.Vetorial.e.Geometria.Analítica,
                     lpt = graduados.model.input$Leitura.e.Produção.de.Textos,
                     prog1 = graduados.model.input$Programação.I,
                     ic = graduados.model.input$Introdução.à.Computação,
                     lp1 = graduados.model.input$Laboratório.de.Programação.I,
                     calc2 = graduados.model.input$Cálculo.Diferencial.e.Integral.II,
                     md = graduados.model.input$Matemática.Discreta,
                     p2 = graduados.model.input$Programação.II,
                     classica = graduados.model.input$Fundamentos.de.Física.Clássica,
                     lp2 = graduados.model.input$Laboratório.de.Programação.II,
                     grafos = graduados.model.input$Teoria.dos.Grafos,
                     cra = graduados.model.input$cra)

disciplinas <- disciplinas %>% mutate(loglp2 = sqrt(lp2))
disciplinas <- disciplinas %>% mutate(logcal2 = log(calc2))

ggpairs(disciplinas)

disciplinas.clean <- disciplinas %>%
  filter(!is.na(calc1) & !is.na(calc2) & !is.na(vetorial) & !is.na(lpt) & !is.na(grafos) & !is.na(md)& !is.na(classica)) 
  
library('GGally')

disciplinas.clean <- disciplinas.clean %>% mutate(loglp2 = log(lp2))
disciplinas.clean <- disciplinas.clean %>% mutate(logcal2 = log(calc2))


ggpairs(disciplinas.clean)

p = ggplot(disciplinas.clean, aes(x=ic, y=cra)) +
  geom_point(shape=1)
p

fit.lm = lm(cra ~ ic, data=disciplinas.clean)
print(coef(fit.lm))

#disciplinas.clean <- disciplinas.clean %>% mutate(loglp2 = log(lp2))

fit <- lm(cra  ~ ., data=disciplinas.clean)
summary(fit)
print(coef(fit))
print(mean(fit$residuals))


require(ggplot2)

p1 = ggplot(fit, aes(.fitted, .resid)) +
  geom_point()
p1 = p1 + geom_hline(yintercept=0, col="red", linetype="dashed")
p1 = p1 + xlab("Valores ajustados") + ylab("Resíduos")
p1 = p1 + ggtitle("Gráfico de Resíduos vs Ajustamento") + 
  theme_bw()
p1


qqnorm(fit$residuals)
qqline(fit$residuals, col = 2,lwd=2,lty=2)



##olhar os residuos para o primeiro periodo e segundo isoladamente
##tirei p1
##olha a correlação e retira o maior p valor

#primeiro periodo
fit <- lm(cra  ~ calc1 +
            vetorial +
            prog1 +
            lpt +
            ic +
            lp1, 
          data=disciplinas.clean)

fit <- lm(cra  ~ calc1 +
          vetorial +
          lpt +
          ic +
          lp1, 
          data=disciplinas.clean)
summary(fit)
print(coef(fit))
print(mean(fit$residuals))

ggpairs(disciplinas.clean %>% select(calc1, vetorial, lpt, ic, lp1))

fit <- lm(cra  ~ calc1 +
            vetorial +
            lpt +
            ic +
            lp1, 
          data=disciplinas.clean)

##olha a maior correlação
##tira uma das variaveis
##retira a que tenha o maior p-value

#segundo periodo
ggpairs(disciplinas.clean %>% select(calc2, md, lpt, lpt, p2, classica, lp2))

fit <- lm(cra  ~ calc2 +
            md +
            lpt +
            p2 +
            classica +
            lp2, 
          data=disciplinas.clean)

summary(fit)
print(coef(fit))
print(mean(fit$residuals))
