# knn karar ağacına uyarlanmıştır.Sadece nümerik değerlerden oluşmaktadır.

veriler= read.table(file.choose(), header = T, sep = ";")
install.packages("caret")
library(caret)
install.packages("cluster")
library("cluster")
library("plyr")
library("RWeka")
library("rJava")

View(veriler)
summary(veriler)
str(veriler)
attributes(veriler)

#VERİLER nümerik ve faktör olarak tanımlanır
veriler$Uçucu.asitlik <- as.numeric(veriler$Uçucu.asitlik)
veriler$Sitrik.asit <- as.numeric(veriler$Sitrik.asit)
veriler$Klorürler <- as.numeric(veriler$Klorürler)
veriler$Serbest.kükürt.dioksit <- as.numeric(veriler$Serbest.kükürt.dioksit)
veriler$Toplam.kükürt.dioksit <- as.numeric(veriler$Toplam.kükürt.dioksit)
veriler$Yoğunluk <- as.numeric(veriler$Yoğunluk)
veriler$Sülfatlar <- as.numeric(veriler$Sülfatlar)
veriler$Kalite <- as.character(veriler$Kalite)


veriler$Kalite <- revalue(veriler$Kalite, c("4"="Kötü","5"="Kötü"))
veriler$Kalite <- revalue(veriler$Kalite, c("6"="İyi"))
veriler$Kalite <- revalue(veriler$Kalite, c("7"="Kaliteli","8"="Kaliteli"))
#sadece nümerik  değerlerden oluşan alt küme oluşturuldu ve nümerik değerlere karşılık gelen
#verilerin değerleri sayısal olarak girildi.
n_veriler <- veriler [c(1,2,3,4,5,6,7,8)]

#rastgele veri seçimi için set.seed kullanılır.
set.seed(1234)

ind <- sample(1:239,239)
veriler <- n_veriler[ind,]

veriler$Kalite <- as.factor(veriler$Kalite)

deneme <- J48(Kalite~.,data = veriler)

#kurallari gorelim
print(deneme)

summary(deneme)

#grafigini cizelim
plot(deneme)
