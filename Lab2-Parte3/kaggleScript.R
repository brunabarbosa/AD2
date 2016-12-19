testKaggle <- read_csv("~/Documents/ADII/Lab2-Parte3/test.csv")
trainKaggle <- read_csv("~/Documents/ADII/Lab2-Parte3/train.csv")

summary(trainKaggle)
head(trainKaggle)
head(testKaggle)
colnames(trainKaggle) <- c("matricula",
                          "calc1",
                          "vetorial",
                          "lpt",
                          "p1",
                          "ic",
                          "lp1",
                          "calc2",
                          "md",
                          "p2",
                          "grafos",
                          "classica",
                          "lp2",
                          "cra")



colnames(treino.model.input) <- c("matricula",
                          "calc1",
                          "vetorial",
                          "lpt",
                          "p1",
                          "ic",
                          "lp1",
                          "calc2",
                          "md",
                          "p2",
                          "grafos",
                          "classica",
                          "lp2")


trainKaggle <- na.omit(trainKaggle)

fit.test <- lm(cra  ~ ic +
               md +
               p2 +
               grafos, 
             data=trainKaggle)

summary(fit.test)
summary(trainKaggle)

print(coef(fit.test))
print(mean(fit.test$residuals))
###making a bet


submit <- data.frame(matricula = testKaggle$matricula, cra = predict(fit.test, treino.model.input))
media <- mean(na.omit(submit$cra))
submit[is.na(submit)] <- media
submit
write.csv(submit, file = "submission.csv", row.names = FALSE)




submission <- read_csv("~/Documents/ADII/Lab2-Parte3/submission.csv")
View(submission)

