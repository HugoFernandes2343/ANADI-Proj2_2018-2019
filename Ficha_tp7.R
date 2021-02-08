#Resoluçao ficha 7

#Ex1
## alinea a)

x=c(21,24,32,47,50,59,68,74,62,50,41,30)
y=c(185.79,214.47,288.03,424.84,454.58,539.03,621.55,675.06,562.03,452.93,369.95,273.98)
plot(x,y)
# Observa-se que os pontos se dispersam ao longo de uma recta, pelo que existe tal relaçao linear entre as duas variaveis

## alinea b)
regress=lm(y~x); regress
summary(regress)
regress$coefficients
coef(regress)
# reta de regressão: y = -6.335502 + x = 9.208362

# Para calcular o valor preditor de x = 40
-6.335502 + 9.208362*40
x0=data.frame(x=40); x0
predict(regress, x0)

x0=data.frame(x=c(40, 41,50,52)); x0
predict(regress, x0)


## Alinea c)

?cor
cor(x,y)
cor.test(x,y)
#p-value < 2.2e-16 logo há evidencia estatistica que me permita dizer que existe uma correlaçao linear forte entre as duas variaveis.

# --------
#coeficiente de ajustamento: da a proporçao da variavel Y que e explicada em termos lineares pela varivel independente X.
summary(regress)$r.squared
#0.9998655 muito proximo de 1, pelo que se conclui  que o ajuste e de boa qualidade.

#desenhar a reta de regresso no plot
abline(regress)

## Alinea d)

regress$residuals
#OU
y-(-6.335502 + 9.208362*x)
#OU
y-fitted(regress)
#OU
residuals(regress)

#Pressupostos
#   1. Normalidade: os residuos devem seguir uma distribuiçao normal de media 0 (independencia da variavel y em relacao a variavel residuos)
qqnorm(residuals(regress), ylab = "Residuos")
qqline(residuals(regress))
# o qqplot levanos a suspeitar que o residuos seguem uma distribuicao normal 

# teste a normalidade
shapiro.test(residuals(regress))
# p-value = 0.4066 => nao ejeitamos h0 pelo que aceitamos a normalidade dos dados

#para testarmos a media '0 , como os dados sao normais podemos fazer o t.est
t.test(residuals(regress))
#p-value = 1 => Nao rejeitamos h0 , pelo que aceitamos a media 0

#2. Homocedasticidade; variancia constante
# Em cada nivel da variavel preditora , a variancia dos respetivos residuos deve ser constante.
# Traçar o grafico da variavel preditora vs dos residuos.


plot(x, residuals(regress), xlab="x", ylab="residuos")
abline(h=0)
# Na observaçao grafica nao encontramos nenhuma tendencia do grafico, pelo que pontos aparentam estar aleatoriamente distribuidos em torno de zero.
# Como a amostra é pequena, o diagnostico nao e conclusivo ,pelo que podemos fazer um teste a variancia.
#dividimos a amostra a meio e comparammos aa variancia dos dois subconjuntos
mx=median(x); mx
var.test(residuals(regress)[x>mx], residuals(regress)[x<mx])
#p-value = 0.6865 Aceitamos a igualdade de variancias

# Existe o teste de Brauch-Pagan, que deve ser efetuado apenas para amostras grandes

#3. Independencia dos residuos
# Efetua-se o teste de Durbin-watson do package car
#H0: Os residuos sao independentes vs H1: os residuos sao dependentes

library(car)
durbinWatsonTest(regress)
# p-value =0.124 => nao rejeitamos a iindependencia de residuos 

#Os 3 pressupostos sobre os residuos sao satisfeitos

