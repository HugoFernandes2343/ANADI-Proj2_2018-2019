#Ex 1

x<-c(21,24,32,47,50,59,68,74,62,50,41,30)
y<-c(185.79,214.47,288.03,424.84,454.58,539.03,621.55,675.06,562.03,452.93,369.95,273.98)

#a)
plot(x,y)
#aproximan-se de uma reta imaginaria

#b)
res.regress=lm(formula = y~x) ; res.regress

#Intercept - coeficiente que nao depende de variaveis (ordenada na origem b0)
#x - declive da reta, ou seja b1
summary(res.regress)
# Informacao dos residuos
# Coeficientes, desvio padrao, verificar se sao nulos (intervalo de confianca da estimativa dos valores)
# Coeficiente de determinacao R2: 0,999 (proximo de 1), modelo adequado ao conjunto de dados
# Coeficiente ajustado de determinacao R2: para casos com mais d que uma variavel independente

#Residuos
res.regress$residuals

#Calcular valor preditor de x=40
x0=data.frame(x=40)
predict(res.regress,x0)
#ou usar a funcao obtida anteriormente

#c)
# Calculo do coeficiente linear de pearson
cor(x,y)

# Significancia estatistica pode ser feito teste
cor.test(x,y)


#d)
#homocedasticidade para n<30
plot(fitted(res.regress),residuals(res.regress),xlab="Valores ajustados",ylab="residuos")
abline(h=0)

plot(x,residuals(res.regress),xlab="Valores ajustados",ylab="residuos")
abline(h=0)

mx=median(x);mx

#Teste para verifcar se nsao homocedastica
#H0: as variancias sao iguais
#H1: as variancias nao sao iguais
var.test(residuals(res.regress) [x>mx], residuals(res.regress) [x<mx])


# Para n > 30 e as duas outras condicoes de residuos corretas
library(lmtest)
bptest(formula = y~x)


#Normalidade
qqnorm(residuals(res.regress))
qqline(residuals(res.regress))

#H0: segue uma distribuicao normal
#H1: nao segue uma distribuicao normal
shapiro.test(residuals(res.regress))

#H0: media = 0
#H0: media != 0
t.test(residuals(res.regress))


# Independentes
library(car)

#H0: sao independentes
#H1: nao sao independentes
durbinWatsonTest(res.regress)



#EX2
Ano<-c(1986,1987,1988,1989,1990,1991,1992,1993,1994)
Prod<-c(285,270,294,279,260,262,258,272,255)

#a)
#correlacao negativa
reg.res=lm(Prod~Ano)
plot(Ano,Prod)
abline(reg.res)

#b)
#nome das colunas
names(reg.res)

summary(reg.res)

# apenas o coeficiente de determinacao - qualidade de ajuste da reta aos dados
# Neste caso nao e de grande qualidade por nao estar proximo de 1
summary(reg.res)$r.squared

# apenas o coeficiente de determinacao ajustado
summary(reg.res)$adj.r.squared

# c)
library(car)

#H0: sao independentes
#H1: nao sao independentes
durbinWatsonTest(reg.res)

#logo sao os residuos sao independentes

#EX3
Rend<-c(14,19,23,12,9,15,22,25,15,10,12,16)
Cap<-c(31,40,49,20,21,34,54,52,28,21,24,34)

#a)
ren.res=lm(Cap~Rend)
plot(Rend,Cap)
abline(ren.res)


#c)

#Homocedasticidade
plot(fitted(ren.res),residuals(ren.res),xlab="Valores ajustados",ylab="residuos")
abline(h=0)

#Testar agora a variancia
mx3 = median(Rend)
#H0: as variancias sao iguais
#H1: as variancias nao sao iguais
var.test(residuals(ren.res) [Rend>mx3], residuals(ren.res) [Rend<mx3])


# Independencia
library(car)
#H0: sao independentes
#H1: nao sao independentes
durbinWatsonTest(ren.res)


# Normalidade

qqnorm(residuals(ren.res))
qqline(residuals(ren.res))

#H0: segue uma distribuicao normal
#H1: nao segue uma distribuicao normal
shapiro.test(residuals(ren.res))

#H0: media = 0
#H1: media != 0
t.test(residuals(ren.res))


#Ex5
# n =517 >30
library(readr)
dados <- read_table2("C:/Users/Hugo/Desktop/Novo Documento de Texto.txt")
View(dados)

#a)
lm5 = lm(dados$area~. ,data=dados)

#H0: os coeficentes sao nulos (nao significantes)
#H1: os coeficentes nao sao nulos
# Agora interessa o coeficiente de determinacao ajustado
summary(lm5)

#b)
#Homo
plot(fitted(lm5),residuals(lm5),xlab="Valores ajustados",ylab="residuos")
abline(h=0)
# nao e simetrica em relacao a reta


# Normalidade

qqnorm(residuals(lm5))
qqline(residuals(lm5))

#H0: segue uma distribuicao normal
#H1: nao segue uma distribuicao normal
shapiro.test(residuals(lm5))

# Independencia
library(car)
#H0: sao independentes
#H1: nao sao independentes
durbinWatsonTest(lm5)

#Conclusao nao suportam inferencia estatistica

#c) fator de inflacao de variancia
vif(lm5)
#Conclusao: todas as variancias possuem um vif inferior a 3. Consequentemente podemos assumir que
# nao existe multicolinearidade nas variancias

#d)
lm5reduzido=step(lm5)
