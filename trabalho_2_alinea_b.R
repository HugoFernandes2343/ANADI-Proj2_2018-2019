#Trabalho 2

#Exercicio B#

#i####

#Identifique o par de vari�veis preditoras que possui, em valor absoluto, maior correla��o. Fa�a um estudo da regress�o linear entre a duas vari�veis (a escolha da vari�vel independente fica � sua escolha). Discuta a validade do modelo (coeficiente de determina��o e an�lise dos res�duos).

#import
library(readr)
PimaIndiansDiabetes <- read_csv("PimaIndiansDiabetes.csv")
View(PimaIndiansDiabetes)

# Scatterplot Matrices

#forma mais simples de fazer a matriz
#pairs(~FP+TP+FN+TN, data=Data,main="Simple Scatterplot Matrix")

#Matriz das 8 variaveis tradicional
library(car)
scatterplotMatrix(~pregnant+glucose+triceps+pressure+insulin+mass+pedigree+age, data=PimaIndiansDiabetes, main="Matriz Scatterplots")

#Representa��o grafica utilizando GGally
library(GGally)
ggcorr(PimaIndiansDiabetes, label = TRUE)

#Cor dos 3 que apresentam pelos graficos maior coorelacao
cor(PimaIndiansDiabetes$pregnant,PimaIndiansDiabetes$age)
cor(PimaIndiansDiabetes$triceps,PimaIndiansDiabetes$insulin)
cor(PimaIndiansDiabetes$triceps,PimaIndiansDiabetes$mass)

?cor
#Como vimos que a maior coorela��o � entre a idade e a gravidez temos que escolher entre elas a variavel dependente Y e a independente X, n�s escolhemos a variavel dependente ser a gravidez e a independete a idade

#Plot simples dos dois pontos
plot(PimaIndiansDiabetes$age,PimaIndiansDiabetes$pregnant)

#estudo da regress�o

#coeficientes de regressao
regress=lm(PimaIndiansDiabetes$pregnant~PimaIndiansDiabetes$age); regress
#ou
summary(regress)
#ou
regress$coefficients
#ou
coef(regress)

#h0 � rejeitado a a hipotese nula � o x n�o � significativo porque compreende o valor nulo 0 aprova se que a variavel � significativa 

#os dados ajustam se mal a curva de reggressao visto que o valor do coeficiente e bastatne de longe

# reta de regress�o: y = -1.3394071 + 0.1559663x


?cor.test

#teste de coorela��o de pearson
cor(PimaIndiansDiabetes$age,PimaIndiansDiabetes$pregnant)
cor.test(PimaIndiansDiabetes$age,PimaIndiansDiabetes$pregnant)

#p-value < 2.2e-16 logo h� evidencia estatistica que me permita dizer que existe uma correla�ao linear forte entre as duas variaveis. com um valor de 0.5443412

#grafico com a linha de regre��o
plot(PimaIndiansDiabetes$age,PimaIndiansDiabetes$pregnant,xlab= "Age",ylab = "Pregnant")
abline(regress, col="blue")

#validade de modelo, coeficientes

# apenas o coeficiente de determinacao - qualidade de ajuste da reta aos dados
# Neste caso nao e de grande qualidade por nao estar proximo de 1
summary(regress)$r.squared

# apenas o coeficiente de determinacao ajustado
# Neste caso nao e de grande qualidade por nao estar proximo de 1
summary(regress)$adj.r.squared

#analise de residuos 

#PRESUPOSTO 1: normalidade
residuos=regress$residuals;residuos
qqnorm(residuos)
qqline(residuos)
# a observacao do qqplot sugere quw se aceite a distribui�ao normal dos dados

shapiro.test(residuos)
#p-value = 2.803E-09 NEGAMOS A NORMALIDADE

t.test(residuos)
#p-value= 1 Aceitamos media zero

#PRESUPOSTO 2: homocedasticidade variancia constante
plot(regress$fitted.values, residuos)
plot(PimaIndiansDiabetes$age,residuos)
abline(h=0)
# Existe uma tendencia decrescente no grafico pelo que a variancia n�o aparenta ser constante

m=median(regress$fitted.values); m
var.test(residuos[regress$fitted.values<m],residuos[regress$fitted.values>m])
?var.test

#p-value <2.2e-16 rejeitamos H0 reconhecemos que a variancia n�o � constante


#PRESUPOSTO 3 Autocorrela�ao nula: independencnia dos residuos

library(car)
durbinWatsonTest(regress)
durbinWatsonTest

#p-value=0.54 Nao rejeitamos H0. Aceitamos independencia dos residuos

#os presupostos 1 e 2 nao sao satisfeitos o 3 � 

#podemos ver pelos coeficientes serem bastante afastados de 1 e por apenas dois dos 3 presupostos serem satisfeitos que o modelo n�o � de elevada qualidade sendo assim pouco valido

#ii####

# Considere que a vari�vel "pressure" � a vari�vel dependente (e as restantes 7 s�o as vari�veis independentes). Identifique quais as vari�veis independentes que mais influenciam a vari�vel "pressure" e encontre o modelo de regress�o multivari�vel que depende dessas vari�veis e que tenha menor �ndice de informa��o de Akaike.

#pegar na tabela de dados sobre os indios mas sem a ultima coluna 
DadosIndians= subset(PimaIndiansDiabetes, select = -c(diabetes))

#criar o modelo sendo pressure o Y
modelo=lm(PimaIndiansDiabetes$pressure~.,data=DadosIndians)
modelo

summary(modelo)

modelo$coefficients

# Y =33.51400773 + 0.10290306 *pregnant + 0.02820957 *glucose + 0.20262525 *triceps + -0.00405645 *insulin + 0.50565691 *mass + -1.60257306 *pedigree + 0.37685084 *age
                                      

#Utilizar o comando step
modelo2=step(modelo, direction="both")


#Step:  AIC=4432.2
#PimaIndiansDiabetes$pressure ~ triceps + mass + age

#Df Sum of Sq    RSS    AIC
#<none>                  243888 4432.2
#+ glucose   1     314.1 243574 4433.2
#+ pedigree  1     188.1 243700 4433.6
#+ pregnant  1      83.8 243804 4433.9
#+ insulin   1      29.8 243858 4434.1
#- triceps   1    5515.7 249404 4447.4
#- mass      1   10946.9 254835 4463.9
#- age       1   17466.9 261355 4483.3

#as variaveis que mais influenciam a variavel pressure s�o triceps mass e age, o modelo com menor indice de informa��o de Akaike � (Y=presuure), Y = 34.9501423 + 0.1845835*triceps + 0.5230561*mass + 0.4100589*age

modelo2$coefficients

# Y = 34.9501423 + 0.1845835*triceps + 0.5230561*mass + 0.4100589*age

summary(modelo2)$adj.r.squared

summary(modelo)$adj.r.squared

#ambos os coeficentes s�o muito longe de 1

