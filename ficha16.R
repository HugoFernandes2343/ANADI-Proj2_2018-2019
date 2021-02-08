#Proposta de solução da ficha 16

#Ex1

#a)

#names(Dados)=c()
attach(Dados)
desempenho = data.frame(Instancia=Instancia, ACO.rel=abs(ACO-Optimo)/Optimo, 
                        PSO.rel=abs(PSO-Optimo)/Optimo, ABC.rel=abs(ABC-Optimo)/Optimo)
                          
            #
attach(desempenho)
sd(desempenho$ACO.rel)
sd(desempenho$PSO.rel)
sd(desempenho$ABC.rel)
boxplot(desempenho$ACO.rel, desempenho$PSO.rel, desempenho$ABC.rel)

# 2ª fase: inferencia sobre a população
#temos três grupos, amostras emparelhadas (a mesma instancia correu nos três algoritmos), logo nao podemos usar o ANOVA
#teste de friedman
#arranjar os dados 
library(reshape2)
Mdados=melt(desempenho, variable.name="MH", value.name="Erro.rel")
View(Mdados)
attach(Mdados)
friedman.test(Erro.rel, MH, Instancia)
#p-value <2.2e-16 => Rejeitar H0
# Com um nivel

#3ª fase posthoc
#Pela análise da boxplot(a) e tendo em conta o resultado do teste de friedman(b), os resultados sugerem que a tecnica ABC tem um desempenho superior pois tema menor mediana e a menor dispersao

#Para confirmar testa-se o desempenho de PSO.rel vs ABC.rel
#como a  amostra e grande, podemos usar o teste T (parametrico)
# hip test

t.test(PSO.rel, ABC.rel, alternative = "greater", paired=T)
# p-value = 5.917e-14

#OU
# posthoc de friedman
attach(Mdados)
library(PMCMR) 
posthoc.friedman.nemenyi.test(Erro.rel, MH, Instancia)


#Conclui-se assim que a tecnica ABC tem o melhor desempenho.



source("https://www.r-statistics.com/wp-content/uploads/2010/02/Friedman-Test-with-Post-Hoc.r.txt")  # loading the friedman.test.with.post.hoc function from the internet

### Comparison of three Wine ("Wine A", "Wine B", and
###  "Wine C") for rounding first base.
WineTasting <- data.frame(
  Taste = c(5.40, 5.50, 5.55,
            5.85, 5.70, 5.75,
            5.20, 5.60, 5.50,
            5.55, 5.50, 5.40,
            5.90, 5.85, 5.70,
            5.45, 5.55, 5.60,
            5.40, 5.40, 5.35,
            5.45, 5.50, 5.35,
            5.25, 5.15, 5.00,
            5.85, 5.80, 5.70,
            5.25, 5.20, 5.10,
            5.65, 5.55, 5.45,
            5.60, 5.35, 5.45,
            5.05, 5.00, 4.95,
            5.50, 5.50, 5.40,
            5.45, 5.55, 5.50,
            5.55, 5.55, 5.35,
            5.45, 5.50, 5.55,
            5.50, 5.45, 5.25,
            5.65, 5.60, 5.40,
            5.70, 5.65, 5.55,
            6.30, 6.30, 6.25),
  Wine = factor(rep(c("Wine A", "Wine B", "Wine C"), 22)),
  Taster = factor(rep(1:22, rep(3, 22))))

with(WineTasting , boxplot( Taste  ~ Wine )) # boxploting
friedman.test.with.post.hoc(Taste ~ Wine | Taster ,WineTasting)	# the same with our function. With post hoc, and cool plots

