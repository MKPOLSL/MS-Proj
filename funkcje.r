funkcja_testowa <- function(a){
  return (a * 2)
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
  tablica_rozkladu = 0.264 # Statystyka z tablicy rozkładu Kołmogorowa
  
  # Standaryzacja
  for(i in 1:length(a)) {
    standaryzacja[i] <- (dane[i]-sredniaa)/odchStanda
  }
  
  # Dystrybuanta rozkładu hipotetycznego
  for(i in 1:length(a)) {
    dystrybuanta_hipotetyczna[i] <- pnorm(standaryzacja[i]) #pnorm zwraca funkcję dystrybuanty
  }
  
  # Dystrybuanta empiryczna
  for(i in 1:length(a)) {
    dystrybuanta_empiryczna[i] <- i/length(a)
  }
  
  # Różnica dystrybuant
  for(i in 1:length(a)) {
    roznica[i] <- abs(dystrybuanta_hipotetyczna[i] - dystrybuanta_empiryczna[i])
  }
  
  # Wartość statystyki testowej
  wartosc_statystyki_testowej <- max(roznica)
  
  
  writeLines("H0 - rozkład normalny\n")
  writeLines("H1 - brak rozkładu normalnego\n")
  
  writeLines("dla aycyjnego systemu:\n")
  if(wartosc_statystyki_testowej < tablica_rozkladu || wartosc_statystyki_testowej> 1){
    writeLines("Brak podstaw do odrzucenia hipotezy H0\n")
    writeLines("Dane mają rozkład normalny\n")
  }else{
    writeLines("Istnieją podstawy do odrzucenia hipotezy H0\n")
    writeLines("Dane nie mają rozkładu normalnego\n")
  }
}
#koniec kodu zajebanego