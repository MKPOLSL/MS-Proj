
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
  tablica_rozkladu = 0.264 # Statystyka z tablicy rozk�adu Ko�mogorowa
  
  # Standaryzacja
  for(i in 1:length(a)) {
    standaryzacja[i] <- (dane[i]-sredniaa)/odchStanda
  }
  
  # Dystrybuanta rozk�adu hipotetycznego
  for(i in 1:length(a)) {
    dystrybuanta_hipotetyczna[i] <- pnorm(standaryzacja[i]) #pnorm zwraca funkcj� dystrybuanty
  }
  
  # Dystrybuanta empiryczna
  for(i in 1:length(a)) {
    dystrybuanta_empiryczna[i] <- i/length(a)
  }
  
  # R�nica dystrybuant
  for(i in 1:length(a)) {
    roznica[i] <- abs(dystrybuanta_hipotetyczna[i] - dystrybuanta_empiryczna[i])
  }
  
  # Warto�� statystyki testowej
  wartosc_statystyki_testowej <- max(roznica)
  
  
  writeLines("H0 - rozk�ad normalny\n")
  writeLines("H1 - brak rozk�adu normalnego\n")
  
  writeLines("dla aycyjnego systemu:\n")
  if(wartosc_statystyki_testowej < tablica_rozkladu || wartosc_statystyki_testowej> 1){
    writeLines("Brak podstaw do odrzucenia hipotezy H0\n")
    writeLines("Dane maj� rozk�ad normalny\n")
  }else{
    writeLines("Istniej� podstawy do odrzucenia hipotezy H0\n")
    writeLines("Dane nie maj� rozk�adu normalnego\n")
  }
}

wspolczynnik_TStudenta <- function(ufn, n) {
  return(qt((1 - ufn) / 2, n - 1, lower.tail = FALSE, log.p = FALSE))
}

# Przedzia� ufno�ci dla �redniej
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
#Granice przedzia�u ufno�ci dla wariancji warto�ci pracy nowej hali
granica_wariancja<-function(n,war,wspChi){
  return(n*war/wspChi)
}
#Wzgl�dna precyzja oszacowania
wzgledna_precyzja_wariancja <- function(dolnaGranica,gornaGranica,war){
  return(0.5*((gornaGranica-dolnaGranica)/war))
}

statystyka <- function(x, y) {
  # Funkcja zwraca warto�� statystyki testowej dla por�wnania
  # dw�ch �rednich z pr�b podanych jako argumenty
  x.srednia <- mean(x)
  x.wariancja <- var(x)
  y.srednia <- mean(y)
  y.wariancja <- var(y)
  
  U = (x.srednia - y.srednia) /
    sqrt( (x.wariancja / length(x)) + (y.wariancja / length(y)) )
  
  return(U)
}
#koniec kodu zajebanego