

kwantyl <- function(lista, q)
{
  pozycjaKwartyla = sum(lista$counts) * q
  nIsk <- lista$counts   #liczebnosc skumulowana
  
  for(i in 1:length(lista$counts))
  {
    nIsk[i] = sum(lista$counts[1:i])
  }
  
  znaleziona = 0
  pozycja = 1
  while (znaleziona == 0)
  {
    if (pozycjaKwartyla < nIsk[pozycja] )
      znaleziona = 1
    else
      pozycja = pozycja + 1
  }
  
  xI0 = lista$breaks[pozycja]       #dolna wartosc przedzialu z kwantylem
  nIskminus1 = nIsk[pozycja-1]    #liczebnosc skumulowana przedzialu poprzedzajacego kwantyl
  nI0 = lista$counts[pozycja]       #liczebnosc przedzialu z kwantylem
  rozpietoscPrzedzialu = lista$breaks[pozycja+1] - lista$breaks[pozycja]
  wynik = xI0 + ((pozycjaKwartyla - nIskminus1) * (rozpietoscPrzedzialu / nI0))
  return (wynik)
}

modalna_przedzial <- function(a) {
  
  najwieksza_liczebnosc <- max(a$counts)
  liczebnosc_poprz <- stara_histogram$counts[which.max(a$counts) - 1]
  liczebnosc_nastepn <- stara_histogram$counts[which.max(a$counts) + 1]
  return(mod_stara + (najwieksza_liczebnosc-liczebnosc_poprz)/(2*najwieksza_liczebnosc - liczebnosc_poprz - liczebnosc_nastepn) * (a$breaks[which.max(a$counts) + 1]-a$breaks[which.max(a$counts)]))
}

moment_centralny_przedzial <- function(lista, n) {
  suma <- sum(lista$counts * lista$mids)
  srednia <- suma / sum(lista$counts)
  print(suma)
  print(srednia)
  wynik <- 0
  for(i in 1:length(lista$counts)){
    wynik <- wynik + ((lista$mids[i] - srednia) ^ n * lista$counts[i])
  }
  wynik <- wynik / sum(lista$counts)
  return(wynik)
}

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

wspolczynnik_TStudenta <- function(ufn, n) {
  return(qt((1 - ufn) / 2, n - 1, lower.tail = FALSE, log.p = FALSE))
}

# Przedział ufności dla średniej
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
#Granice przedziału ufności dla wariancji wartości pracy nowej hali
granica_wariancja<-function(n,war,wspChi){
  return(n*war/wspChi)
}
#Względna precyzja oszacowania
wzgledna_precyzja_wariancja <- function(dolnaGranica,gornaGranica,war){
  return(0.5*((gornaGranica-dolnaGranica)/war))
}

statystyka <- function(x, y) {
  # Funkcja zwraca wartość statystyki testowej dla porównania
  # dwóch średnich z prób podanych jako argumenty
  x.srednia <- mean(x)
  x.wariancja <- var(x)
  y.srednia <- mean(y)
  y.wariancja <- var(y)
  
  U = (x.srednia - y.srednia) /
    sqrt( (x.wariancja / length(x)) + (y.wariancja / length(y)) )
  
  return(U)
}
#koniec kodu zajebanego