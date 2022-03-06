#Naive Bayes Algoritması
veriler =read.table (file.choose(),header=T,sep=";")
# veri seti incelenir, nÃ¼merik ve kategorik veriler tanÄ±mlanÄ±r
library("plyr")
veriler$Kalite <- as.factor(veriler$Kalite)
veriler$Kalite <-revalue(veriler$Kalite, c("4"="Kötü","5"="Kötü"))
veriler$Kalite <-revalue(veriler$Kalite, c("6"="İyi"))
veriler$Kalite <-revalue(veriler$Kalite, c("7"="Kaliteli","8"="Kaliteli"))
#veri seti eÄŸitim ve test veri seti olarak ayrÄ±lÄ±r.
#veri seti egitim ve test seti olarak ikiye ayrilacak

 library(caret)
 set.seed
 
 verisetibolme <- createDataPartition(y=veriler$Kalite, p=0.6,list=FALSE)
  egitim <- veriler[verisetibolme,]
  test <- veriler[-verisetibolme,]
  
#Eğitim ve test veri setine tahmininde kullanılacak nitelik ve hedef nitelik(diyabetik
  #polinöropati) atanır. Diyabetik polinöropati 8. Sütunda olduğu için 8 kullanıldı.
 testNitelikleri <- test[,-8]
 testHedefNitelik <- test[[8]]
 egitimNitelikleri <- egitim [,-8]
 egitimHedefNitelik <- egitim [[8]]

 library(e1071)
 naiveBayes_modeli_kuruldu <- naiveBayes(egitimNitelikleri, egitimHedefNitelik)
 naiveBayes_modeli_kuruldu

#modelin tahminleri bulunur
 (tahminiSiniflar <- predict(naiveBayes_modeli_kuruldu, testNitelikleri))
 #gercek siniflar ile tahmini siniflarin kiyasi
   (karisiklikmatrisi <- table(tahminiSiniflar, testHedefNitelik, dnn =c ("TahminiSiniflar", "Gercek Siniflar")))

 (TP <- karisiklikmatrisi [1])
 
 (FP <- karisiklikmatrisi [4])
 
 (FN <- karisiklikmatrisi [7])

 (TN <- karisiklikmatrisi [2])
 (TP <- karisiklikmatrisi [5])

 (FP <- karisiklikmatrisi [8])

 (FN <- karisiklikmatrisi [3])
 
 (TN <- karisiklikmatrisi [6])
 
 (TP <- karisiklikmatrisi [9])
 
 paste0("Dogruluk = ",(Dogruluk <- (TP+TN)/sum(karisiklikmatrisi)))
  paste0("Hata = ",(Hata <- 1-Dogruluk))
 