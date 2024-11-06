
#Aula 1 Explorando o Mundo dos Dados

#Introdução ao R

#1. Operações Aritméticas

5+3 #Ctrl + Enter para rodar o código do Script no Console

5 + 3

2*2+3

#Se eu quiser que a soma seja rodada primeiro, tenho que colocar
#entre parênteses

2*(2+3)

2*(2*(2+3))

#Função raiz quadrada - Nome da função é "sqrt"

sqrt(4)

sqrt(25)

#2. Criando objetos

result <- 5+3

result

result2 <- 15*(3+2)

result2

print(result2)

#Objetos armazenam também caracteres

Lucas <- "professor curso R"

Result <- "5+3"

#Posso calcular em cima de objetos numéricos, mas não em cima de objetos
#com caracteres

#Exemplo

result*2

Result*2

#Avaliando a classe do objeto

class(result) #numeric

class(Result) #character

class(sqrt) #function

#Criando vetores

world.pop <- c(2525779, 3026003,3691173, 4449049,
               5320817, 6127700, 6916183)

#Vetores podem ser combinados

pop.first <- c(2525779, 3026003, 3691173) #População mundial de 1950, 1960 e 1970

pop.second <- c(4449049, 5320817,6127700, 6916183) #Restante dos anos

pop.all <- c(pop.first, pop.second)

pop.all

pop.all2 <- c(pop.second,pop.first)

pop.all2

#Operações com vetores

pop.million <- world.pop / 1000

pop.million

pop.rate <- world.pop / world.pop[1]

pop.rate

pop.rate[c(2,3)] <- c(19.8 , 46.1)

pop.rate

pop.rate[4] <- 76.1

pop.rate

pop.rate[c(5,6,7)] <- 2

pop.rate

#Funções

#Funções básicas

length(world.pop) #Tamanho do meu vetor, quantas informações tem dentro do vetor

min(world.pop) #Valor mínimo

max(world.pop) #Valor máximo

range(world.pop) #Vai me apresentar o menor e o maior valor

mean(world.pop) #Cálculo da média

sum(world.pop) / length(world.pop) #Outro forma de calcular a média

#Criando vetor ano

year <- seq(from = 1950, to = 2010, by = 10)
year

year2 <- seq(from = 2010, to = 1950, by = -10)
year2

names(world.pop)
names(world.pop) <- year
names(world.pop)

world.pop

#Criando funções

#Criando função de "resumo"

my.summary <- function(x){
  
  s.out <- sum(x)
  l.out <- length(x)
  m.out <- s.out / l.out #ou usar mean(x)
  out <- c(s.out, l.out, m.out)
  names(out) <- c("sum","length","mean")
  
  return(out)
}

#Nossa função tem uma forma: my.summary(x)

z <- 1:10

my.summary(z)

my.summary(world.pop)
