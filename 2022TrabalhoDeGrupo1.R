
#Rodar para instalar as packages
install.packages("readxl")
install.packages("DescTools")
install.packages("ggplot2")
install.packages("agricolae")
#Rodar para inicializar as packages
library("readxl")
library("DescTools")
library("ggplot2")
library("agricolae")

#Import da base de dados 
bikesRawBD <- read_excel("C:/R/Bike_Price_Prediction.xlsx",sheet = "Sheet1",col_names = TRUE)
#bikesRawBD <- read_excel("PC RUI Path",sheet = "Sheet1",col_names = TRUE)
#bikesRawBD <- read_excel("PC Matheus Path",sheet = "Sheet1",col_names = TRUE)

#remover campos não utilizados
bikes = subset(bikesRawBD, select = -c(Bike_model,S.no)) 

#função para remover os ultimos x characteres de uma variavel, ex: "220CC" -> "220"
remove_n_trailing_characters <- function(data_to_change, number_of_trailing_characters) {
  reg_expresion <- paste('.{',number_of_trailing_characters,"}$", sep="") # criar regular expression
  gsub(reg_expresion, '', data_to_change)
}

#Remover "CC" do fim da cubic capacity (Rodar apenas uma vez)
bikes$`CC(Cubic capacity)` = remove_n_trailing_characters(bikes$`CC(Cubic capacity)`,2)


#Passar variaveis para numerico:
bikes$`CC(Cubic capacity)` <- as.numeric(bikes$`CC(Cubic capacity)`)
#insurance$bmi <- as.numeric(insurance$bmi)
#insurance$charges <- as.numeric(insurance$charges)

### Variaveis Escolhidas: (preencher tipo)
#bike_company         - Qualitativa Nominal
#Manufactured_year    - Quantitativa Discreta (not 100% sure)
#Engine_warranty      - Quantitativa Discreta (not 100% sure)
#Engine_type          - Qualitativa Nominal
#Fuel_type            - Qualitativa Nominal
#CC(Cubic capacity)   - Quantitativa Continua (not 100% sure)
#Fuel_Capacity        - No Clue
#Price                - Quantitativa Continua

#quantitativo -> histograma ou polygno de freq, polygon.freq
#qualitativa nominal -> grafico circular
#exemplos:
#barplot(bike_company_freq$freq, main="Company Distribution", names.arg=bike_company_freq$level)
#pie(bike_company_freq$freq, labels = bike_company_freq$level, main="Pie Chart of Genders")
#hist(insurance$children)

### Tabelas de Frequencia + graficos
#Bike Company
bike_company_freq <- Freq(bikes$Bike_company)

bike_company_freq <- Freq(bikes$bike_company_freq)
pie(bike_company_freq$perc, labels = paste(round(bike_company_freq$perc*100),"%"),
    main="Pie Chart of Bike Company", col = terrain.colors(length(bike_company_freq$level)))
legend("right", bike_company_freq$level,
       cex = 0.45, fill = terrain.colors(length(bike_company_freq$level)))

#Manufactured_year
#TRATAR DISTO
Manufactured_year_freq <- Freq(bikes$Manufactured_year)
hist(bikes$Manufactured_year)

Manufactured_year_Outliers_Filtrados <- which(bikes$Manufactured_year>1000)
hist(bikes$Manufactured_year[Manufactured_year_Outliers_Filtrados])
barplot(Manufactured_year_freq$freq, main="Manufactured Year", names.arg=Manufactured_year_freq$level
        bikes$Manufactured_year[Manufactured_year_Outliers_Filtrados])


#Engine_warranty
Engine_warranty_freq <- Freq(bikes$Engine_warranty)

#Engine_type
Engine_type_freq <- Freq(bikes$Engine_type)
pie(Engine_type_freq$perc, labels = paste(round(Engine_type_freq$perc*100),"%"),
    main="Pie Chart of Engine Types", col = terrain.colors(length(Engine_type_freq$level)))
legend("right", Engine_type_freq$level,
       cex = 1.2, fill = terrain.colors(length(Engine_type_freq$level)))

#Fuel_type
Fuel_type_freq <- Freq(bikes$Fuel_type)
pie(Fuel_type_freq$perc, labels = paste(round(Fuel_type_freq$perc*100),"%"),
    main="Pie Chart of Fuel Types", col = terrain.colors(length(Fuel_type_freq$level)))
legend("right", Fuel_type_freq$level,
       cex = 1.5, fill = terrain.colors(length(Fuel_type_freq$level)))

#CC(Cubic capacity)
CC_freq <- Freq(bikes$`CC(Cubic capacity)`)
hist(bikes$`CC(Cubic capacity)`)

#Fuel_Capacity
Fuel_Capacity <- Freq(bikes$Fuel_Capacity)

#Price
Price_freq <- Freq(bikes$Price)


### Comparações / Relações entre variaveis:
#posiveis comparações para estudo:

# Cubic capacity Eletrico vs Combustivel

#anos de garantia VS ano de criação

#anos de garantia vs Eletrico/Combustivl

#Fuel Capacity vs ano de criação

#Price Eletrico VS Combustivel

#Cubic Capacity Petrol VS Diesel

#Cubic capacity VS Engine type

#Engine type VS price

#Price Petrol VS Diesel

#Price VS Manufactured Year

#Price vs Bike_company

# Adicionar mais


### Desvio Padrão
#Apenas para variaveis em que faça sentido (quantitativas)
sd(bikes$`CC(Cubic capacity)`)

### Média
#Apenas para variaveis em que faça sentido (quantitativas)
mean(bikes$`CC(Cubic capacity)`)


###Verificação de outliers ( indicar se existe ou não para depois de mostrar no relatorio)
#Apenas para variaveis em que faça sentido (quantitativas)
boxplot(bikes$`CC(Cubic capacity)`) # Existe outliers

### Mediana
#Apenas para variaveis em que faça sentido (quantitativas)
median(bikes$`CC(Cubic capacity)`)

### Moda
#Função para obter a moda:
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(bikes$Engine_type)
getmode(bikes$Fuel_type)
getmode(bikes$`CC(Cubic capacity)`)

### Quartis

quantile(bikes$`CC(Cubic capacity)`)