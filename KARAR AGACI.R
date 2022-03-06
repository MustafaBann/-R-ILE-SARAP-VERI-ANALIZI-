#C4.5 Karar aðacý Algoritmasý
veriler= read.table(file.choose(), header = T, sep = ";")
library(rJava)
library(RWeka)
head(veriler)
data(veriler)
View(veriler)
str(veriler)
summary(veriler)
install.packages("Plyr")
library(plyr)
data(veriler)

veriler$Kalite <- as.factor(veriler$Kalite)

veriler$Kalite <- revalue(veriler$Kalite, c("4"="Kötü","5"="Kötü"))
veriler$Kalite <- revalue(veriler$Kalite, c("6"="iyi"))
veriler$Kalite <- revalue(veriler$Kalite, c("7"="Kaliteli","8"="Kaliteli"))


veriler$Kalite <- as.factor(veriler$Kalite)

model <- J48(Kalite~.,data = veriler)
View(model)
print(model)
summary(model)
plot(model)


summary(veriler)
