
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

#remover campos n�o utilizados
bikes = subset(bikesRawBD, select = -c(Bike_model,S.no)) 

#fun��o para remover os ultimos x characteres de uma variavel, ex: "220CC" -> "220"
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
#Manufactured_year    - Quantitativa Discreta 
#Engine_warranty      - Quantitativa Discreta 
#Engine_type          - Qualitativa Nominal
#Fuel_type            - Qualitativa Nominal
#CC(Cubic capacity)   - Quantitativa Continua 
#Fuel_Capacity        - Quantitativa discreta
#Price                - Quantitativa Continua


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
Manufactured_year_freq_no_outlier <- Freq(bikes$Manufactured_year[Manufactured_year_outlier_filter])
hist(bikes$Manufactured_year[Manufactured_year_outlier_filter])


#Engine_warranty
#avisar no relatorio que eliminei o outlier
Engine_warranty_freq <- Freq(bikes$Engine_warranty)
Engine_warranty_Outliers_Filtrados <- which(bikes$Engine_warranty<20)
Engine_warranty_freq_no_outlier <- Freq(bikes$Engine_warranty[Engine_warranty_Outliers_Filtrados])
hist(bikes$Engine_warranty)
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

# Cubic capacity Eletrico vs Combustivel
bikes$Fuel_type
eletricos <- which(bikes$Fuel_type== "Electricity")
mean(bikes$`CC(Cubic capacity)`[eletricos])
mean(bikes$`CC(Cubic capacity)`[Petrol])
hist(bikes$`CC(Cubic capacity)`[eletricos])
hist(bikes$`CC(Cubic capacity)`[Petrol])
#conclus�o, carros a combustivel s�o mais fortes

#anos de garantia VS ano de cria��o
bikes_copy <- bikes[which(bikes$Engine_warranty != "NA"),]
mean_by_year_ew <- aggregate(bikes_copy$Engine_warranty, list(bikes_copy$Manufactured_year), FUN=mean)
mean_by_year_ew_filter <- which(mean_by_year_ew$Group.1 > 1900 & mean_by_year_ew$Group.1 < 2030)
barplot(mean_by_year_ew$x[mean_by_year_ew_filter], names.arg = mean_by_year_ew$Group.1[mean_by_year_ew_filter])
#conclus�o: o tempo de garantia tem se mantido estavel ao longo dos ultimos anos

#anos de garantia vs Eletrico/Combustivl
warranty_eletricos <- bikes$Engine_warranty[eletricos]
warranty_eletricos_outlier_filter = which((warranty_eletricos < 30))
mean(warranty_eletricos[warranty_eletricos_outlier_filter])
petro_engine_warranty <- bikes$Engine_warranty[Petrol]
petro_engine_warranty <- petro_engine_warranty[which(petro_engine_warranty !="NA" )]
mean(petro_engine_warranty)
hist(warranty_eletricos[warranty_eletricos_outlier_filter])
hist(petro_engine_warranty)
#conclus�o, carros a combustivel tem mais anos de garantia

#Fuel Capacity vs ano de cria��o
bikes_copy <- bikes[which(bikes$Fuel_Capacity != "Battery"),]
bikes_copy$Fuel_Capacity = as.numeric(remove_n_trailing_characters(bikes_copy$Fuel_Capacity,7))
mean_by_Fuel_Capacity_Ano <- aggregate(bikes_copy$Fuel_Capacity, list(bikes_copy$Manufactured_year), FUN=mean)
mean_by_Fuel_Capacity_Ano_filter <- which(mean_by_Fuel_Capacity_Ano$Group.1 > 1900 & mean_by_Fuel_Capacity_Ano$Group.1 < 2030)
barplot(mean_by_Fuel_Capacity_Ano$x[mean_by_Fuel_Capacity_Ano_filter], names.arg = mean_by_Fuel_Capacity_Ano$Group.1[mean_by_Fuel_Capacity_Ano_filter])
#conclus�o: a media de combustivel tem se mantido estavel ao longo dosanos

#Price Eletrico VS Combustivel
mean(bikes$Price[eletricos])
mean(bikes$Price[Petrol])
max(bikes$Price[eletricos])
max(bikes$Price[Petrol])
hist(bikes$Price[Petrol])
hist(bikes$Price[eletricos])
#conclus�o, motas combustiveis s�o muito mais caras em media, porem motas topo de gama a combustivel tem os maiores pre�os observados

#Cubic capacity VS Engine type
mean_by_Engine_type_CC <- aggregate(bikes$`CC(Cubic capacity)`, list(bikes$Engine_type), FUN=mean)
barplot(mean_by_Engine_type_CC$x, names.arg = mean_by_Engine_type_CC$Group.1)
#conclus�o: motoress Dual Stroke s�o os mais potentes, sendo o resto dos modelos equiparaveis entre si

#Engine type VS price
mean_by_Engine_type_Price <- aggregate(bikes$Price, list(bikes$Engine_type), FUN=mean)
barplot(mean_by_Engine_type_Price$x, names.arg = mean_by_Engine_type_Price$Group.1)
#conclus�o, Motores Dual Stroke s�o muito mais caros

#Price VS Manufactured Year
mean_by_Manufactured_Year_Price <- aggregate(bikes$Price, list(bikes$Manufactured_year), FUN=mean)
mean_by_Manufactured_Year_Price_filter <- which(mean_by_Manufactured_Year_Price$Group.1 > 1900 & mean_by_Manufactured_Year_Price$Group.1 < 2030)
barplot(mean_by_Manufactured_Year_Price$x[mean_by_Manufactured_Year_Price_filter], names.arg = mean_by_Manufactured_Year_Price$Group.1[mean_by_Manufactured_Year_Price_filter])
#concus�o: notsure vejam o grafico

#Price vs Bike_company
mean_by_Bike_Company_Price <- aggregate(bikes$Price, list(bikes$Bike_company), FUN=mean)
mean_by_Manufactured_Year_Price_filter <- which(mean_by_Manufactured_Year_Price$Group.1 > 1900 & mean_by_Manufactured_Year_Price$Group.1 < 2030)
barplot(mean_by_Bike_Company_Price$x, names.arg = mean_by_Bike_Company_Price$Group.1)
#conclusao, ignorar este grafico impercetivel
# Adicionar mais


### Desvio Padr�o
#Apenas para variaveis em que fa�a sentido (quantitativas)
sd(bikes$Manufactured_year)
Engine_warranty_Outliers_Filtrados <- which(bikes$Engine_warranty!="NA")
sd(bikes$Engine_warranty[Engine_warranty_Outliers_Filtrados])
sd(bikes$`CC(Cubic capacity)`)
sd(Fuel_capacity_No_Chars)
sd(bikes$Price)


### Media
#Apenas para variaveis em que fa�a sentido (quantitativas)
mean(bikes$Manufactured_year)
mean(bikes$Engine_warranty[Engine_warranty_Outliers_Filtrados])
mean(bikes$`CC(Cubic capacity)`)
mean(Fuel_capacity_No_Chars)
mean(bikes$Price)

###Verifica��o de outliers ( indicar se existe ou n�o para depois de mostrar no relatorio)
#Apenas para variaveis em que fa�a sentido (quantitativas)
boxplot(bikes$Manufactured_year) # Existe outliers
boxplot(bikes$Engine_warranty[Engine_warranty_Outliers_Filtrados]) # N�o Existe outliers
boxplot(bikes$`CC(Cubic capacity)`) # Existe outliers
boxplot(Fuel_capacity_No_Chars) # existe poucos outliers
boxplot(bikes$Price) # existe outliers

### Mediana
#Apenas para variaveis em que fa�a sentido (quantitativas)
median(bikes$Bike_company)
median(bikes$Manufactured_year)
median(bikes$Engine_warranty[Engine_warranty_Outliers_Filtrados])
median(bikes$`CC(Cubic capacity)`)
median(Fuel_capacity_No_Chars)
median(bikes$Price)

### Moda
#Fun��o para obter a moda:
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
quantile(bikes$Manufactured_year[Manufactured_year_outlier_filter])
quantile(bikes$Engine_warranty[Engine_warranty_Outliers_Filtrados])
quantile(bikes$`CC(Cubic capacity)`)
quantile(Fuel_capacity_No_Chars)
quantile(bikes$Price)
