#Naive Bayes Algoritmas�
veriler =read.table (file.choose(),header=T,sep=";")
# veri seti incelenir, nümerik ve kategorik veriler tanımlanır
library("plyr")
veriler$Kalite <- as.factor(veriler$Kalite)
veriler$Kalite <-revalue(veriler$Kalite, c("4"="K�t�","5"="K�t�"))
veriler$Kalite <-revalue(veriler$Kalite, c("6"="�yi"))
veriler$Kalite <-revalue(veriler$Kalite, c("7"="Kaliteli","8"="Kaliteli"))
#veri seti eğitim ve test veri seti olarak ayrılır.
#veri seti egitim ve test seti olarak ikiye ayrilacak

 library(caret)
 set.seed
 
 verisetibolme <- createDataPartition(y=veriler$Kalite, p=0.6,list=FALSE)
  egitim <- veriler[verisetibolme,]
  test <- veriler[-verisetibolme,]
  
#E�itim ve test veri setine tahmininde kullan�lacak nitelik ve hedef nitelik(diyabetik
  #polin�ropati) atan�r. Diyabetik polin�ropati 8. S�tunda oldu�u i�in 8 kullan�ld�.
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
 