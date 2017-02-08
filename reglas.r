## ---- message=FALSE, warning=FALSE---------------------------------------
#install.packages("arules")
library(arules)

## ------------------------------------------------------------------------
# Fichero de películas
movieData <- read.csv2("./ml-1m/movies.dat", sep = ";", header = F)
names(movieData) <- c("MoviesId", "Title", "Year","Genres")
head(movieData, n=2)

# Fichero de rating
ratingsData <- read.csv2("./ml-1m/ratings.dat", sep = ";", header = F)
names(ratingsData) <- c("UserId", "MoviesId", "Ratings", "Timestamp")
head(ratingsData, n=2)

# Fichero de usuarios
usersData <- read.csv2("./ml-1m/users.dat", sep = ";", header = F)
names(usersData) <- c("UserId", "Gender", "Age" , "Occupation", "Zip-code")
head(usersData, n=2)

# Juntando los datasets
totalData <- base::merge(ratingsData, movieData, by="MoviesId")
totalData <- base::merge(totalData[,2:7], usersData, by="UserId")

## ------------------------------------------------------------------------
# Características del dataset
head(totalData, n=3)
dim(totalData)
summary(totalData)
str(totalData)

## ------------------------------------------------------------------------
# Instrucciones para Eliminar columnas del dataset
totalData[["Timestamp"]] = NULL
totalData[["Zip-code"]] = NULL

## ------------------------------------------------------------------------
#Instrucciones para hacer los cortes:
totalData$Age <- as.factor(totalData$Age)
totalData$Age <- factor(totalData$Age, labels = c("Under_18", "18-24", "25-34","35-44",
                                                  "45-49", "50-55", "56+"))
summary(totalData$Age)

totalData$Occupation <- as.factor(totalData$Occupation)
totalData$Occupation <- factor(totalData$Occupation, labels = c("other",
                                                                   "academic/educator",
                                                                   "artist",
                                                                   "clerical/admin",
                                                                   "college/grad student",
                                                                   "customer service",
                                                                   "doctor/health care",
                                                                   "executive/managerial",
                                                                   "farmer",
                                                                   "homemaker",
                                                                   "K-12 student",
                                                                   "lawyer",
                                                                   "programmer",
                                                                   "retired",
                                                                   "sales/marketing",
                                                                   "scientist",
                                                                   "self-employed",
                                                                   "technician/engineer",
                                                                   "tradesman/craftsman",
                                                                   "unemployed",
                                                                   "writer"))
totalData$UserId <- as.factor(totalData$UserId)
totalData$Ratings <- as.factor(totalData$Ratings)

## ------------------------------------------------------------------------
totalData[[ "Year"]] = ordered( cut ( totalData[[ "Year"]],
                                      c(1918,1929,1939,1949,1959,1969,1979,1989,1999,2000) ) ,
labels = c ("less30", "30s", "40s", "50s", "60s", "70s", "80s", "90s", "2000s"))
summary(totalData$Year)

## ------------------------------------------------------------------------
maleVector <- which(totalData$Gender == "M")
femaleVector <- which(totalData$Gender == "F")

male <- vector(length = length(totalData$Gender))
female <- vector(length = length(totalData$Gender))
male[maleVector] <- T
female[femaleVector] <- T
# Se añaden las variables creadas al dataset
totalData <- cbind(totalData, male)
totalData <- cbind(totalData, female)

# Una vez añadido las dos variables creadas al dataset, se elimina la variable Gender.
totalData[["Gender"]] = NULL

## ------------------------------------------------------------------------
# Se comprueba que todos los datos son categóricos
str(totalData)
# Convertimos el data.frame en un conjunto de transacciones
movieTran <- as(totalData, "transactions")

## ------------------------------------------------------------------------
summary(movieTran)

## ---- fig.cap="Plot de los itemsets más frecuentes\\label{fig:plotItems}"----
itemFrequencyPlot(movieTran, support = 0.1, cex.names = 0.8)

## ---- message=FALSE, warning=FALSE, results= "hide"----------------------
# Extraer Itemsets frecuentes
aprioriMovie <- apriori(movieTran, parameter = list(support = 0.1, target="frequent"))

## ------------------------------------------------------------------------
aprioriMovie <- sort(aprioriMovie, by="support") # Los ordenamos por el valor del soporte

## ------------------------------------------------------------------------
inspect(head(aprioriMovie, n=10))
size(head(aprioriMovie, n=5))

## ---- fig.cap="Plot donde se agrupan los itemsets por su tamaño \\label{fig:itemTam}"----
barplot(table(size(aprioriMovie)), xlab="itemset size", ylab="count")
inspect(head(aprioriMovie[size(aprioriMovie) == 1], n=5))

## ------------------------------------------------------------------------
# Extraer Itemsets maximales y cerrados
iMaxMovie <- aprioriMovie[is.maximal(aprioriMovie)]
inspect(head(sort(iMaxMovie, by="support")))

## ------------------------------------------------------------------------
icloMovie <- aprioriMovie[is.closed(aprioriMovie)]
inspect(head(sort(icloMovie, by="support")))

## ---- fig.cap="Plot de los itemsets frecuentes, cerrados y maximales\\label{fig:plotFCM}"----
barplot( c(frequent=length(aprioriMovie), closed=length(icloMovie), maximal=length(iMaxMovie)),
         ylab="count", xlab="itemsets")

## ---- message=FALSE, warning=FALSE---------------------------------------
# Extraer reglas: Apriori
rules <- apriori(movieTran, parameter = list(support = 0.1, confidence = 0.3, minlen = 2))

## ------------------------------------------------------------------------
summary(rules)

## ------------------------------------------------------------------------
inspect(head(rules, n=5))
quality(head(rules, n=5))

## ------------------------------------------------------------------------
# Ordenar las reglas por el campo que más nos interese.
rulesSorted <- sort(rules, by = "lift")
inspect(head(rulesSorted, n=5))

## ------------------------------------------------------------------------
rulesYears90s <- subset(rules, subset = rhs %in% "Year=90s" & lift > 0.95)
inspect(head(rulesYears90s, n=5))

## ------------------------------------------------------------------------
# Eliminar reglas redundantes
subsetMatrix <- is.subset(rulesSorted, rulesSorted)
subsetMatrix[lower.tri(subsetMatrix, diag=T)] <- NA
redundant <- colSums(subsetMatrix, na.rm=T) >= 1
rulesPruned <- rulesSorted[!redundant] # remove redundant rules
inspect(head(rulesPruned, n=5))

## ------------------------------------------------------------------------
# Medidas de interés adicionales
mInteres <- interestMeasure(rulesPruned, measure=c("hyperConfidence", "leverage" ,"phi", "gini"),
                            transactions= movieTran)
quality(rulesPruned) <- cbind(quality(rulesPruned), mInteres)
inspect(head(sort(rulesPruned, by="phi"), n=3))

## ---- message=FALSE, warning=FALSE---------------------------------------
# Visualización (arulesViz)
# install.packages("arulesViz")
library (arulesViz)

## ---- fig.cap="Plot de todas las reglas usando el paquete arules\\label{fig:scatter}"----
plot(rules)

## ---- fig.cap="Plot de las reglas representandolas como un grafo\\label{fig:grafo}"----
# Otros métodos de visualización
plot(rules[1:10], method="graph", control=list(type="items"))

## ---- fig.cap="Plot de las reglas en forma de matriz agrupada\\label{fig:plotMatriz}"----
# reglas como una matriz agrupada
plot(rules, method="grouped", interactive= F)

## ------------------------------------------------------------------------
# Escribir fichero
write(rulesPruned, file="reglas.csv", sep = ",", col.names=NA)

## ---- message=FALSE, warning=FALSE---------------------------------------
library(pmml)
write.PMML(rulesPruned, file="reglas.pmml")
# leer reglas PMML
reglasPMML = read.PMML("reglas.pmml")

