
moment_centralny <- function(a, n) {
  ilosc <- length(a)
  srednia <- sum(a) / ilosc
  wynik <- 0
  for(val in a) {
    wynik <- wynik + (val - srednia) ^ n
  }
  wynik <- wynik / ilosc
  return(wynik)
}

modalna <- function(a) {
  wynik <- unique(a)
  wynik[which.max(tabulate(match(a, wynik)))]
}

#kod zajebany
test_zgodnosci <- function(a){
  # Sortowanie danych
  dane<-sort(a)
  
  # Zmienne i wektory
  standaryzacja <- dane
  dystrybuanta_empiryczna <- dane
  dystrybuanta_hipotetyczna <-dane
  roznica <- dane
  tablica_rozkladu = 0.264 # Statystyka z tablicy rozk³adu Ko³mogorowa
  
  # Standaryzacja
  for(i in 1:length(a)) {
    standaryzacja[i] <- (dane[i]-sredniaa)/odchStanda
  }
  
  # Dystrybuanta rozk³adu hipotetycznego
  for(i in 1:length(a)) {
    dystrybuanta_hipotetyczna[i] <- pnorm(standaryzacja[i]) #pnorm zwraca funkcjê dystrybuanty
  }
  
  # Dystrybuanta empiryczna
  for(i in 1:length(a)) {
    dystrybuanta_empiryczna[i] <- i/length(a)
  }
  
  # Ró¿nica dystrybuant
  for(i in 1:length(a)) {
    roznica[i] <- abs(dystrybuanta_hipotetyczna[i] - dystrybuanta_empiryczna[i])
  }
  
  # Wartoœæ statystyki testowej
  wartosc_statystyki_testowej <- max(roznica)
  
  
  writeLines("H0 - rozk³ad normalny\n")
  writeLines("H1 - brak rozk³adu normalnego\n")
  
  writeLines("dla aycyjnego systemu:\n")
  if(wartosc_statystyki_testowej < tablica_rozkladu || wartosc_statystyki_testowej> 1){
    writeLines("Brak podstaw do odrzucenia hipotezy H0\n")
    writeLines("Dane maj¹ rozk³ad normalny\n")
  }else{
    writeLines("Istniej¹ podstawy do odrzucenia hipotezy H0\n")
    writeLines("Dane nie maj¹ rozk³adu normalnego\n")
  }
}

wspolczynnik_TStudenta <- function(ufn, n) {
  return(qt((1 - ufn) / 2, n - 1, lower.tail = FALSE, log.p = FALSE))
}

# Przedzia³ ufnoœci dla œredniej
dolna_granica_sred <- function(srednia, wspT, odch, licz) {
  dolna_granica = srednia - wspT * (odch / sqrt(licz - 1))
  return(dolna_granica)
}

gorna_granica_sred <- function(srednia, wspT, odch, licz) {
  gorna_granica = srednia + wspT * (odch / sqrt(licz - 1))
  return(gorna_granica)
}

wspChiKwadrat<-function(ufn,n){
  return (qchisq(ufn,n-1))
}
#Granice przedzia³u ufnoœci dla wariancji wartoœci pracy nowej hali
granica_wariancja<-function(n,war,wspChi){
  return(n*war/wspChi)
}
#Wzglêdna precyzja oszacowania
wzgledna_precyzja_wariancja <- function(dolnaGranica,gornaGranica,war){
  return(0.5*((gornaGranica-dolnaGranica)/war))
}

statystyka <- function(x, y) {
  # Funkcja zwraca wartoœæ statystyki testowej dla porównania
  # dwóch œrednich z prób podanych jako argumenty
  x.srednia <- mean(x)
  x.wariancja <- var(x)
  y.srednia <- mean(y)
  y.wariancja <- var(y)
  
  U = (x.srednia - y.srednia) /
    sqrt( (x.wariancja / length(x)) + (y.wariancja / length(y)) )
  
  return(U)
}
#koniec kodu zajebanego