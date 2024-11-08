
#Curso R Aula 3

#Importar os dados sobre a ideologia dos parlamentares dos EUA

congress <- read_csv("congress.csv")

#Quero olhar para 2 aspectos da ideologia: as questões econômicas e raciais

#Se quero avaliar polarização, eu quero entender a diferença de comportamento
#entre os democratas e republicanos

rep <- subset(congress, subset = (party_code == 200))
#vou criar um primeiro subset só com os dados dos republicanos

dem <- congress[congress$party_code == 100, ]
#Crio um segundo subset só com os dados dos democratas

#Eu não quero olhar os dados agregados, porque as coisas mudam ao longo dos anos
#Então eu quero comparar a polarização dos parlamentares em diferentes épocas

#Primeiro, quero olhar nos anos 1940, na 80ª Legislatura (1947-1949)

rep80 <- subset(rep, subset = (congress == 80))
dem80 <- subset(dem, subset = (congress == 80))

#Agora, quero também olhar para os parlamentares mais recentes
#Vou utilizar a 112ª Legislatura (2011-2013)

rep112 <- subset(rep, subset = (congress == 112))
dem112 <- subset(dem, subset = (congress == 112))

xlab <- "Economic liberalism/conservatism"
ylab <- "Racial liberalism/conservatism"
lim <- c(-1.5, 1.5)

summary(congress$nominate_dim1) #É a variável de economia (eixo x)
summary(congress$nominate_dim2) #É a variável racial (eixo y)

#Vou observar graficamente a polarização

#Primeiro na 80ª Legistura

plot(dem80$nominate_dim1,dem80$nominate_dim2, pch = 16, col = "blue",
     xlim = lim, ylim = lim, xlab = xlab, ylab = ylab, main = "80th Congress")

points(rep80$nominate_dim1,rep80$nominate_dim2, pch = 17, col = "red")

text(-0.75, 1, "Democrats")
text(1, -1, "Republicans")

#Agora vamos olhar para 112ª Legislatura

plot(dem112$nominate_dim1,dem112$nominate_dim2, pch = 16, col = "blue",
     xlim = lim, ylim = lim, xlab = xlab, ylab = ylab, main = "112th Congress")

points(rep112$nominate_dim1,rep112$nominate_dim2, pch = 17, col = "red")

text(-0.75, 1, "Democrats")
text(1, -1, "Republicans")

#Então vamos analisar a posição dos parlamentares como grupo
#Utilizando a mediana como medida de tendência central

dem.median <- tapply(dem$nominate_dim1, dem$congress, median) #Comportamento mediano dos democratas
#Em relação à pauta econômica, por legislatura

rep.median <- tapply(rep$nominate_dim1, rep$congress, median) #Comportamento mediano dos republicanos
#Em relação à pauta econômica, por legislatura

#Porque que o comprimento dos vetores está diferente?

table(dem$congress) #Começa na legislatura 25, e termina na 118
table(rep$congress) #Começa na legislatura 34, e termina na 118

#Vamos olhar graficamente a mediana dos parlamentares em relação a pauta econômica,
#Ao longo do tempo

plot(names(dem.median), dem.median, col = "blue", 
     type = "l", xlim = c(25, 118), ylim = c(-1, 1), xlab = "Congress",
     ylab = "DW-NOMINATE score (first dimension)")

lines(names(rep.median), rep.median, col = "red")

text(110, -0.6, "Democratic\n Party")
text(110, 0.85, "Republican\n Party")

#Lidando com os casos ausentes da mediana - inserindo na.rm

dem.median <- tapply(dem$nominate_dim1, dem$congress, median, na.rm = TRUE)
rep.median <- tapply(rep$nominate_dim1, rep$congress, median, na.rm = TRUE)

#Rodando o gráfico sem os NAs

plot(names(dem.median), dem.median, col = "blue", 
     type = "l", xlim = c(25, 118), ylim = c(-1, 1), xlab = "Congress",
     ylab = "DW-NOMINATE score (first dimension)")

lines(names(rep.median), rep.median, col = "red")

text(110, -0.6, "Democratic\n Party")
text(110, 0.85, "Republican\n Party")

#Vamos calcular o nível de polarização ao longo do tempo

#Vou eliminar as primeiras legislaturas que só tem nos democratas, mas não tem nos republicanos

dem.median[-(1:9)]

dem.median <- dem.median[-(1:9)]

dem.median
length(dem.median)

rep.median
length(rep.median)

plot(seq(from = 1855.5, to = 2023.5, by = 2),
     rep.median - dem.median, xlab = "Year",
     ylab = "Republican median - Democratic median",
     main = "Political polarization")

#Da mesma forma que olhamos para polarização, vamos olhar para desigualdade
#de renda

#Primeiro baixar o dataframe com o coeficiente de Gini por ano para os EUA

#Agora vou arrumar a base antes de usá-la

#Se eu quero que o meu vetor seja lido como um número, eu uso as.numeric
#Se eu quero que o meu vetor seja lido como caracter, eu uso as.factor

USGini$year <- as.numeric(substr(USGini$DATE, 1, 4)) #Melhorar a variável ano

USGini$gini <- as.numeric(USGini$SIPOVGINIUSA)

USGini <- USGini[,-(c(1,2))]

#Vamos plotar o gráfico para ver o desenvolvimento da desigualdade de renda
#ao longo dos anos

plot(USGini$year,
     USGini$gini, xlab = "Year",
     ylab = "Gini coefficient",
     main = "Income Inequality")

#Desigualdade de renda
length(USGini$gini[seq(from = 1, to = nrow(USGini), by = 2)]) #Tem 30 observações

#Polarização política
length((rep.median - dem.median)[seq(from = 55, to = 84, by = 1)]) #Também tem 30


#Agora posso medir o nível de correlação entre essas duas variáveis
cor(USGini$gini[seq(from = 1, to = nrow(USGini), by = 2)],
    (rep.median - dem.median)[seq(from = 55, to = 84, by = 1)])

#Como ler correlação?
#Correlação varia de -1 (inversamente proporcional) a 1 (diretamente proporcional)

#Correlação fraca: -0,3 a 0,3 ou [-0.3,0.3]
#Correlação média: de -0,7 a -0,3 ou [-0.7,-0.3) e 0,3 a 0,7 ou (0.3,0.7]
#Correlação forte: de -1 a -0,7 ou [-1,-0.7) e 0,7 a 1 ou (0.7,1]

#Quando eu quero comparar uma distribuição com outra distribuição,
#Eu uso o Q-Q Plot

#Vamos olhar para distribuição da ideologia em relação as questões raciais
#Dos democratas e republicanos

hist(dem112$nominate_dim2, freq = FALSE, main = "Democrats",
     xlim = c(-1.5, 1.5), ylim = c(0, 1.75),
     xlab = "Racial liberalism/conservatism dimension")

hist(rep112$nominate_dim2, freq = FALSE, main = "Republicans",
     xlim = c(-1.5, 1.5), ylim = c(0, 1.75),
     xlab = "Racial liberalism/conservatism dimension")

#Vamos comparar essas duas distribuições utilizando o Q-Q Plot
#Questões raciais

qqplot(dem112$nominate_dim2,
       rep112$nominate_dim2, xlab = "Democrats",
       ylab = "Republicans", xlim = c(-1.5, 1.5), ylim = c(-1.5,1.5),
       main = "Racial liberalism/conservatism dimension")
abline(0, 1)

#Vamos comparar agora questões econômicas

qqplot(dem112$nominate_dim1,
       rep112$nominate_dim1, xlab = "Democrats",
       ylab = "Republicans", xlim = c(-1.5, 1.5), ylim = c(-1.5,1.5),
       main = "Economic liberalism/conservatism dimension")
abline(0, 1)

#Regressão Linear: Prevendo o resultado das eleições com base
#Em características faciais

#Vou calcular o percentual de votos dos democratas

face$d.share <- face$d.votes / (face$d.votes + face$r.votes)

#Vou calcular o percentual de votos dos republicanos

face$r.share <- face$r.votes / (face$d.votes + face$r.votes)

#Vou calcular a diferença de votos entre esses dois candidatos
#Vou calcular o voto dos democratas menos o dos republicanos
#O que isso significa?
#Significa que quando o democrata ganhar, o valor vai ser positivo
#Então quanto maior o valor de "diff.share", maior a vitória do democrata
#Agora, quando o republicano ganhar, o valor vai ser negativo
#Quanto menor o valor de "diff.share", maior a vitória do republicano

face$diff.share <- face$d.share - face$r.share

plot(face$d.comp, face$diff.share, pch = 16,
     col = ifelse(face$w.party == "R", "red","blue"),
     xlim = c(0, 1), ylim = c(-1, 1),
     xlab = "Competence scores for Democrats",
     ylab = "Democratic margin in vote share",
     main = "Facial competence and vote share")

cor(face$d.comp, face$diff.share) #Uma correlação média

fit <- lm(diff.share ~ d.comp, data = face) #rodando o modelo de regressão

summary(fit)

abline(fit, col = "darkgreen", lwd = 2) #adicionando linha de regressão ao gráfico

#O valor de Y quando X = 1, é o valor do Intercept + Estimate
(-0.31223)+0.66038 #0.34815 +- 0.12
