
#Aula 2
#Introdu??o ao R

#Arquivo de dados

#Como escolher o diret?rio de trabalho?

setwd("~/Curso_R_2024/")
getwd()

#Como baixar arquivo de dados para o ambiente de trabalho
#Arquivo tipo CSV

UNpop <- read_csv("UNpop.csv")
View(UNpop)

#Analisando a estrutura do meu arquivo de dados

names(UNpop) #Nome das minhas vari?veis
nrow(UNpop) #Quantas casos (linhas) tem o meu dataframe?
ncol(UNpop) #Quantas vari?veis (colunas) tem o meu dataframe?
dim(UNpop) #Quantos casos e vari?veis?
summary(UNpop) #Resumo do meu dataframe

#Como acessar as vari?veis? Os vetores?

UNpop$world.pop

#Outras formas de acessar valores

#Padr?o: "Nome do arquivo"[acessar valores pela linha,acessar valores pela coluna]

UNpop[,"world.pop"] #Acessar s? a minha vari?vel world.pop

UNpop[c(1, 2, 3),] #Acessar s? valores das linhas 1, 2 e 3

UNpop[1:3, "year"] #Acessar linhas especificas de uma vari?vel espec?fica

UNpop$world.pop[seq(from = 1, to = nrow(UNpop), by = 2)] #s?o 7 linhas

#Lidando com NAs (Valores omissos)

world.pop <- c(UNpop$world.pop, NA)
world.pop

mean(UNpop$world.pop)

mean(world.pop)

mean(world.pop, na.rm = TRUE)

#Salvar o ambiente de trabalho

save.image("~/Curso_R_2024/Ambiente_R_Aula_2.RData")

#Salvar como RData

save(UNpop, file = "UNpop.RData")

#Salvando arquivos do ambiente para CSV

write.csv(UNpop, file = "UNpop2.csv")

#Acessando arquivos tipo R

load("~/Curso_R_2024/UNpop.RData")

#Como instalar pacotes?

install.packages("foreign")
library(foreign)

WVS <- read.dta("WVS_Wave_7_Brazil_Stata_v5.0.dta")

#Parte 2 - Mensuração

#Dados Afeganistão

afghan <- data

#Estatísticas descritivas / resumos das variáveis

summary(afghan$age.tens) #Resumo da variável "age.tens"
#Formato divido por 10, então eu tenho que arrumar

class(afghan$age.tens) #Vamos ver qual é classe... numeric!

#Portanto, eu tenho que multiplicar esses dados por 10!

afghan$age <- afghan$age.tens * 10

summary(afghan$age)

#Como verificar a frequência de uma variável

afghan$violent.exp.ISAF #Se o afegão sofreu danos da International Security 
#and Assistance Force (ISAF) que é a organização da Otan no Afeganistão

table(afghan$violent.exp.ISAF) #É uma variável dummy

table(afghan$violent.exp.taliban) #Danos sofridos pelo talibã

#Como rodar uma tabela de referência cruzada

table(ISAF = afghan$violent.exp.ISAF, Talibã = afghan$violent.exp.taliban)

prop.table(table(ISAF = afghan$violent.exp.ISAF,
                 Talibã = afghan$violent.exp.taliban))

#Trablhando com dados ausentes / dados omissos (NA)

head(afghan$income, n = 10)

head(is.na(afghan$income), n = 10)

#Formas de lidar com NA

#1. Eliminar os indivíduos com valores omissos

nrow(afghan) #Quantos casos eu tenho?

afghan.sub <- na.omit(afghan) #Vou tirar todos os casos que tenham algum NA

nrow(afghan.sub)

length(na.omit(afghan$income)) #quero olhar só na variável "income"

afghan.sub <- na.omit(afghan$income) #Vou tirar só os omissos da variável income

#Visualização de dados - distribuição univariada

#Barplot - Gráfico de barras
#Usamos barplot para plotar variáveis categóricas / variáveis discretas

table(ISAF = afghan$violent.exp.ISAF)

prop.table(table(ISAF = afghan$violent.exp.ISAF))

prop.table(table(ISAF = afghan$violent.exp.ISAF, exclude = NULL))

ISAF.ptable <- prop.table(table(ISAF = afghan$violent.exp.ISAF,exclude = NULL))

barplot(ISAF.ptable,
        names.arg = c("Sem dano","Sofreu dano","Não respondeu"),
        main = "Civis vítimizados pelo ISAF",
        xlab = "Categoria de resposta",
        ylab = "Proporção de respondentes",
        ylim = c(0, 0.7))

#Histograma

table(afghan$educ.years)

hist(afghan$educ.years, freq = FALSE,
     breaks = seq(from = -0.5, to = 18.5, by = 1),
     xlab = "Anos de educação",
     ylab = "Densidade",
     main = "Distribuição de educação dos respondentes")
abline(v = median(afghan$educ.years))
text(x = 3, y = 0.5, "median")

#Boxplot

boxplot(afghan$age,
        main = "Distribuição da idade",
          ylab = "Idade",
        ylim = c(10, 80))

summary(afghan$age)

boxplot(educ.years ~ province.id, data = afghan,
        main = "Educação por província",
        ylab = "Anos de educação",
        xlab = "Província")

#Transformação Log Natural

boxplot(villages.balance$population,
        ylab = "população") #Muito difícil de visualizar por conter
#casos extremos, muito diferentes da maioria da amostra

boxplot(log(villages.balance$population),
        ylab = "log população") #Melhora a visualização

boxplot(log(population) ~ village.surveyed, data = villages.balance,
        ylab = "log população", names = c("Nonsampled","Sampled"))

#Relações bivariadas

#Ideologia dos congressistas norte-americanos

congress <- read_csv("congress.csv")

#Vamos criar 2 subsets, um só com os congressistas republicanos
#e outro só com os congressistas democratas

table(congress$party_code) #Vamos olhar a distribuição dos partidos

rep <- subset(congress, subset = (party_code == 200)) #Subset só com republicanos

dem <- congress[congress$party_code == 100, ] #subset só com democratas

rep80 <- subset(rep, subset = (congress == 80)) #1947-1949 80 Legislatura

dem80 <- subset(dem, subset = (congress == 80)) #1947-1949 80 Legislatura

rep112 <- subset(rep, subset = (congress == 112)) #2011-2012 112 Legislatura

dem112 <- subset(dem, subset = (congress == 112)) #2011-2012 112 Legislatura

xlab <- "Economic liberalism/conservatism"

ylab <- "Racial liberalism/conservatism"

lim <- c(-1.5, 1.5)

#Fazendo o gráfico de relação bivariada

#Vamos olhar primeiro para a 80ª Legislatura

plot(dem80$nominate_dim1,
     dem80$nominate_dim2, pch = 16, col = "blue",
     xlim = lim, ylim = lim, xlab = xlab, ylab = ylab,
     main = "80th Congress (1947-1949)")

points(rep80$nominate_dim1,
       rep80$nominate_dim2, pch = 17, col = "red")

text(-0.75, 1, "Democrats")

text(1, -1, "Republicans")

#Vamos olhar primeiro para a 80ª Legislatura

plot(dem80$nominate_dim1,
     dem80$nominate_dim2, pch = 16, col = "blue",
     xlim = lim, ylim = lim, xlab = xlab, ylab = ylab,
     main = "80th Congress (1947-1949)")

points(rep80$nominate_dim1,
       rep80$nominate_dim2, pch = 17, col = "red")

text(-0.75, 1, "Democrats")

text(1, -1, "Republicans")

#Vamos olhar primeiro para a 112ª Legislatura

plot(dem112$nominate_dim1,
     dem112$nominate_dim2, pch = 16, col = "blue",
     xlim = lim, ylim = lim, xlab = xlab, ylab = ylab,
     main = "112th Congress (2011-2012)")

points(rep112$nominate_dim1,
       rep112$nominate_dim2, pch = 17, col = "red")

text(-0.75, 1, "Democrats")

text(1, -1, "Republicans")
