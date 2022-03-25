install.packages("readxl")
install.packages("DescTools")
install.packages("ggplot2")
install.packages("agricolae")
library("readxl")
library("DescTools")
library("ggplot2")
library("agricolae")
#insurance <- read_excel("C:/lisp/Insurance.xlsx",sheet = "Folha2",col_names = TRUE)
insurance <- read_excel("C:\\Users\\gonca\\OneDrive\\Ambiente de Trabalho\\Insurance.xlsx",sheet = "Folha2",col_names = TRUE)

insurance$bmi <- as.numeric(insurance$bmi)
insurance$charges <- as.numeric(insurance$charges)

#tabela de frequencia AGE
age_freq <- Freq(insurance$age)
#quantitativo - hist - polygno de freq, polygon.freq
barplot(age_freq$freq, main="Age Distribution", names.arg=age_freq$level)

#tabela de frequencia SEX
sex_freq <- Freq(insurance$sex)
pie(sex_freq$freq, labels = sex_freq$level, main="Pie Chart of Genders")
barplot(sex_freq$freq, names.arg=sex_freq$level, main="Sex Distribuition")
#qualitativa nominal -> grafico circular


#tabela de frequencia CHILDREN
children_freq <- Freq(as.character(insurance$children))
hist(insurance$children)

#tabela de frequencia BMI
aux <- sturges.freq(insurance$bmi)
bmi_freq <- Freq(insurance$bmi, breaks = aux$breaks, include.lowest = TRUE)
hist(insurance$bmi)

#tabela de frequencia Smoker
smoker_freq <- Freq(insurance$smoker)
pie(smoker_freq$freq, labels = smoker_freq$level, main="Pie Chart of Smokers")
barplot(smoker_freq$freq, names.arg=smoker_freq$level, main="Smoker Distribuition")

#tabela de frequencia Region
region_freq <- Freq(insurance$region)
pie(region_freq$freq, labels = region_freq$level, main="Pie Chart of Regions")
barplot(region_freq$freq, names.arg=region_freq$level, main="Region Distribuition")

#tabela de frequencia Charges
aux <- sturges.freq(insurance$charges)
charges_freq <- Freq(insurance$charges, breaks = aux$breaks, include.lowest = TRUE)
hist(insurance$charges)
  
factor(cut(chargesframe,breaks=nclass.Sturges(chargesframe)))
# Frequencia de filhos por grupos de idade
grupo_de_idade_menor_25 <- which(insurance$age < 21)
Freq(as.character(insurance$children[grupo_de_idade_menor_25]))
grupo_de_idade_25_a_42 <- which(insurance$age >= 21 & insurance$age < 42)
Freq(as.character(insurance$children[grupo_de_idade_25_a_42]))
grupo_de_idade_42_a_64 <- which(insurance$age >= 42 & insurance$age <= 64)
Freq(as.character(insurance$children[grupo_de_idade_42_a_64]))

#fummadores seguro
#Resultado fumadores pagam mais em seguros
grupo_fumador <- which(insurance$smoker == "yes")
mean(insurance$charges[grupo_fumador])

grupo_nao_fumador <- which(insurance$smoker == "no")
mean(insurance$charges[grupo_nao_fumador])

#obesos x seguro
#Resultado individuos obesos pagam mais em seguros
obesos <- which(insurance$bmi>30)
nao_obesos <-which(insurance$bmi<30)
mean(insurance$charges[obesos])
mean(insurance$charges[nao_obesos])

#filhos x seguro
#Resultado pessoas com sem filhos ou 1 pagam 
#praticamente o mesmo mas as pessoas com mais 
#de 1 filho pagam mais
com_1filho <- which(insurance$children==1)
sem_filhos <- which(insurance$children==0)
com_maisde1filho <-which(insurance$children>1)
mean(insurance$charges[com_1filho])
mean(insurance$charges[sem_filhos])
mean(insurance$charges[com_maisde1filho])

#desvio padrão 
sd(insurance$age)
sd(insurance$charges)
sd(insurance$bmi)
sd(insurance$children)

#média
mean(insurance$age)
mean(insurance$charges)
mean(insurance$bmi)
mean(insurance$children)


#cheking outliers
boxplot(insurance$age)
boxplot(insurance$bmi) #existe outliers
boxplot(insurance$children) 
boxplot(insurance$charges)# existe outliers

#mediana

median(insurance$age)
median(insurance$bmi)
median(insurance$children)
median(insurance$charges)

#moda

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(insurance$sex)
getmode(insurance$smoker)
getmode(insurance$region)
getmode(insurance$age)
getmode(insurance$children)

#Quartis

quantile(insurance$age)
quantile(insurance$bmi)
quantile(insurance$children)
quantile(insurance$charges)


#Graficos


