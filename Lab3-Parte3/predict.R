# Fill missing values in lpt
predicted_lpt <- rpart( Leitura.e.Produção.de.Textos ~ Álgebra.Vetorial.e.Geometria.Analítica 
                        + Cálculo.Diferencial.e.Integral.I 
                        + Introdução.à.Computação 
                        + Programação.I 
                        + Laboratório.de.Programação.I, 
                        data = all_data_reshaped[!is.na(all_data_reshaped$Leitura.e.Produção.de.Textos),], method = "anova")


all_data_reshaped$Leitura.e.Produção.de.Textos[is.na(all_data_reshaped$Leitura.e.Produção.de.Textos)] <- 
  predict(predicted_lpt, all_data_reshaped[is.na(all_data_reshaped$Leitura.e.Produção.de.Textos),])

summary(all_data_reshaped)

# Fill missing values in cal1
predicted_calc1 <- rpart( Cálculo.Diferencial.e.Integral.I ~ Álgebra.Vetorial.e.Geometria.Analítica 
                          + Leitura.e.Produção.de.Textos
                          + Introdução.à.Computação 
                          + Programação.I 
                          + Laboratório.de.Programação.I, 
                          data = all_data_reshaped[!is.na(all_data_reshaped$Cálculo.Diferencial.e.Integral.I),], method = "anova")


all_data_reshaped$Cálculo.Diferencial.e.Integral.I[is.na(all_data_reshaped$Cálculo.Diferencial.e.Integral.I)] <- 
  predict(predicted_calc1, all_data_reshaped[is.na(all_data_reshaped$Cálculo.Diferencial.e.Integral.I),])

summary(all_data_reshaped)

# Fill missing values in vetorial
predicted_calc1 <- rpart( Álgebra.Vetorial.e.Geometria.Analítica ~ Cálculo.Diferencial.e.Integral.I
                         + Leitura.e.Produção.de.Textos
                         + Introdução.à.Computação 
                         + Programação.I 
                         + Laboratório.de.Programação.I, 
                         data = all_data_reshaped[!is.na(all_data_reshaped$Álgebra.Vetorial.e.Geometria.Analítica),], 
                         method = "anova")


all_data_reshaped$Álgebra.Vetorial.e.Geometria.Analítica[is.na(all_data_reshaped$Álgebra.Vetorial.e.Geometria.Analítica)] <- 
  predict(predicted_calc1, all_data_reshaped[is.na(all_data_reshaped$Álgebra.Vetorial.e.Geometria.Analítica),])

summary(all_data_reshaped)

# Fill missing values in ic
predicted_ic <- rpart( Introdução.à.Computação ~ Cálculo.Diferencial.e.Integral.I
                       + Leitura.e.Produção.de.Textos
                       +  Laboratório.de.Programação.I
                       + Programação.I 
                       + Álgebra.Vetorial.e.Geometria.Analítica, 
                       data = all_data_reshaped[!is.na(all_data_reshaped$Introdução.à.Computação),], 
                       method = "anova")


all_data_reshaped$Introdução.à.Computação[is.na(all_data_reshaped$Introdução.à.Computação)] <- 
  predict(predicted_calc1, all_data_reshaped[is.na(all_data_reshaped$Introdução.à.Computação),])

summary(all_data_reshaped)

# Fill missing values in lab1
predicted_lab1 <- rpart( Laboratório.de.Programação.I ~ Cálculo.Diferencial.e.Integral.I
                         + Leitura.e.Produção.de.Textos
                         + Introdução.à.Computação 
                         + Programação.I 
                         + Álgebra.Vetorial.e.Geometria.Analítica, 
                         data = all_data_reshaped[!is.na(all_data_reshaped$Laboratório.de.Programação.I),], 
                         method = "anova")


all_data_reshaped$Laboratório.de.Programação.I[is.na(all_data_reshaped$Laboratório.de.Programação.I)] <- 
  predict(predicted_calc1, all_data_reshaped[is.na(all_data_reshaped$Laboratório.de.Programação.I),])

summary(all_data_reshaped)

# Fill missing values in p1
predicted_p1 <- rpart(  Programação.I ~ Cálculo.Diferencial.e.Integral.I
                        + Leitura.e.Produção.de.Textos
                        + Introdução.à.Computação 
                        + Laboratório.de.Programação.I  
                        + Álgebra.Vetorial.e.Geometria.Analítica, 
                        data = all_data_reshaped[!is.na(all_data_reshaped$Programação.I),], 
                        method = "anova")


all_data_reshaped$Programação.I[is.na(all_data_reshaped$Programação.I)] <- 
  predict(predicted_calc1, all_data_reshaped[is.na(all_data_reshaped$Programação.I),])

summary(all_data_reshaped)


# Fill missing values in cra
all_data_reshaped$cra <- (all_data_reshaped$Cálculo.Diferencial.e.Integral.I * 4
                          + all_data_reshaped$Leitura.e.Produção.de.Textos * 4
                          + all_data_reshaped$Introdução.à.Computação * 4
                          + all_data_reshaped$Laboratório.de.Programação.I  * 4
                          + all_data_reshaped$Álgebra.Vetorial.e.Geometria.Analítica * 4
                          + all_data_reshaped$Programação.I * 4
) / 24
