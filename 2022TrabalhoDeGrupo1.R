
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

#remover campos nï¿½o utilizados
bikes = subset(bikesRawBD, select = -c(Bike_model,S.no)) 

#funï¿½ï¿½o para remover os ultimos x characteres de uma variavel, ex: "220CC" -> "220"
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
       cex = 0.7, fill = terrain.colors(length(bike_company_freq$level)))

#Manufactured_year
#avisar no relatorio que eliminei o outlier
Manufactured_year_freq <- Freq(bikes$Manufactured_year)
Manufactured_year_outlier_filter <- which(bikes$Manufactured_year>2000 & bikes$Manufactured_year<2030)
hist(bikes$Manufactured_year[Manufactured_year_outlier_filter])


#Engine_warranty
#avisar no relatorio que eliminei o outlier
Engine_warranty_freq <- Freq(bikes$Engine_warranty)
hist(bikes$Engine_warranty)
Engine_warranty_Outliers_Filtrados <- which(bikes$Engine_warranty<20)
hist(bikes$Engine_warranty[Engine_warranty_Outliers_Filtrados])

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
#avisar no relatorio que removemos os que tinham "battery"
Fuel_capacity_Filtred <- which(bikes$Fuel_Capacity != "Battery")
Fuel_capacity_No_Chars <- as.numeric(remove_n_trailing_characters(bikes$Fuel_Capacity[Fuel_capacity_Filtred],7))
Fuel_Capacity <- Freq(Fuel_capacity_No_Chars)
hist(Fuel_capacity_No_Chars)

#Price
Price_freq <- Freq(bikes$Price)
hist(bikes$Price)

### Comparacoes / Relacoes entre variaveis:
#posiveis comparacoes para estudo:

Manufactured_year_Outliers_Filtrados <- which(bikes$Manufactured_year>1000)
hist(bikes$Manufactured_year[Manufactured_year_Outliers_Filtrados])
bikes$`CC(Cubic capacity)`[eletricos]

# Cubic capacity Eletrico vs Combustivel
bikes$Fuel_type
eletricos <- which(bikes$Fuel_type== "Electricity")
Petrol <-which(bikes$Fuel_type== "Petrol")
bikes$`CC(Cubic capacity)`[eletricos]
bikes$`CC(Cubic capacity)`[Petrol]
mean(insurance$charges[obesos])
mean(insurance$charges[nao_obesos])

#anos de garantia VS ano de criaï¿½ï¿½o

#anos de garantia vs Eletrico/Combustivl

#Fuel Capacity vs ano de criaï¿½ï¿½o

#Price Eletrico VS Combustivel

#Cubic Capacity Petrol VS Diesel

#Cubic capacity VS Engine type

#Engine type VS price

#Price Petrol VS Diesel

#Price VS Manufactured Year

#Price vs Bike_company

# Adicionar mais


### Desvio Padrï¿½o
#Apenas para variaveis em que faï¿½a sentido (quantitativas)
sd(bikes$Manufactured_year)
Engine_warranty_Outliers_Filtrados <- which(bikes$Engine_warranty!="NA")
sd(bikes$Engine_warranty[Engine_warranty_Outliers_Filtrados])
sd(bikes$`CC(Cubic capacity)`)
sd(Fuel_capacity_No_Chars)
sd(bikes$Price)


### Media
#Apenas para variaveis em que faï¿½a sentido (quantitativas)
mean(bikes$Manufactured_year)
mean(bikes$Engine_warranty[Engine_warranty_Outliers_Filtrados])
mean(bikes$`CC(Cubic capacity)`)
mean(Fuel_capacity_No_Chars)
mean(bikes$Price)

###Verificaï¿½ï¿½o de outliers ( indicar se existe ou nï¿½o para depois de mostrar no relatorio)
#Apenas para variaveis em que faï¿½a sentido (quantitativas)
boxplot(bikes$Manufactured_year) # Existe outliers
boxplot(bikes$Engine_warranty[Engine_warranty_Outliers_Filtrados]) # Não Existe outliers
boxplot(bikes$`CC(Cubic capacity)`) # Existe outliers
boxplot(Fuel_capacity_No_Chars) # existe poucos outliers
boxplot(bikes$Price) # existe outliers

### Mediana
#Apenas para variaveis em que faï¿½a sentido (quantitativas)
median(bikes$Manufactured_year)
median(bikes$Engine_warranty[Engine_warranty_Outliers_Filtrados])
median(bikes$`CC(Cubic capacity)`)
median(Fuel_capacity_No_Chars)
median(bikes$Price)

### Moda
#Funï¿½ï¿½o para obter a moda:
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(bikes$Bike_company)
getmode(bikes$Manufactured_year)
getmode(bikes$Engine_warranty)
getmode(bikes$Engine_type)
getmode(bikes$Fuel_type)
getmode(bikes$`CC(Cubic capacity)`)
getmode(bikes$Fuel_Capacity)
getmode(Fuel_capacity_No_Chars)
getmode(bikes$Price)

### Quartis
quantile(bikes$Manufactured_year)
quantile(bikes$Engine_warranty[Engine_warranty_Outliers_Filtrados])
quantile(bikes$`CC(Cubic capacity)`)
quantile(Fuel_capacity_No_Chars)
quantile(bikes$Price)
