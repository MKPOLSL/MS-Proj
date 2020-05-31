kwantyl <- function(lista, q)
{
  pozycja_kwantyla = sum(lista$counts) * q
  liczebnosc_skumulowana <- lista$counts   #liczebnosc skumulowana
  
  for(i in 1:length(lista$counts))
  {
    liczebnosc_skumulowana[i] = sum(lista$counts[1:i])
  }
  
  znaleziona = FALSE
  pozycja = 1
  while (!znaleziona)
  {
    if (pozycja_kwantyla < liczebnosc_skumulowana[pozycja] )
      znaleziona = TRUE
    else
      pozycja = pozycja + 1
  }
  
  dolna_wart_przedzialu = lista$breaks[pozycja]       #dolna wartosc przedzialu z kwantylem
  liczebnosc_skumulowana_poprzedzajacy = liczebnosc_skumulowana[pozycja-1]  
  liczebnosc_przedzialu = lista$counts[pozycja]       #liczebnosc przedzialu z kwantylem
  szerokosc_przedzialu = lista$breaks[pozycja+1] - lista$breaks[pozycja]
  wynik = dolna_wart_przedzialu + ((pozycja_kwantyla - liczebnosc_skumulowana_poprzedzajacy) * (szerokosc_przedzialu / liczebnosc_przedzialu))
  return (wynik)
}

modalna_przedzial <- function(a) {
  dolna_granica <- a$breaks[which.max(a$counts)]
  najwieksza_liczebnosc <- max(a$counts)
  liczebnosc_poprz <- a$counts[which.max(a$counts) - 1]
  liczebnosc_nastepn <- a$counts[which.max(a$counts) + 1]
  return(dolna_granica + (najwieksza_liczebnosc-liczebnosc_poprz)/(2*najwieksza_liczebnosc - liczebnosc_poprz - liczebnosc_nastepn) * (a$breaks[which.max(a$counts) + 1]-a$breaks[which.max(a$counts)]))
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

#test sgodnosci kolmogorowa-lileforsa
# zadanie 2
test_zgodnosci<-function(hala)
{
  #wartosci potrzebne w dalszej czesci testu
  
  
  tablica_rozkladu<-sqrt(length(hala))
  srednia=sum(hala)/length(hala)
  #odchyenie standardowe
  wariancja <- 0
  for(val in hala)
  {
    wariancja <- wariancja + (val-srednia)^2/length(hala)
  }
  odchylenie_standardowe <- sqrt(wariancja)
  
  #sortowansko starej i nowej 
  dane <-sort(hala)
  #standaryzacja starej
  standaryzacja<-dane
  for(i in hala)
  {
    standaryzacja[i]<-(dane[i]-srednia)/odchylenie_standardowe
  }
  
  #dystrybuanta rozkladu hipotetycznego starej
  dystrybuanta_hipotetyczna<-dane
  for(i in hala) {
    dystrybuanta_hipotetyczna[i] <- pnorm(standaryzacja[i]) #pnorm zwraca funkcj� dystrybuanty
  }
  
  #dystrybuanta empiryczna starej
  dystrybuanta_empiryczna<-dane
  
  for(i in hala) {
    dystrybuanta_empiryczna[i] <- i/length(hala)
  }
  
  # roznica dystrybuant
  roznica<-0
  for(i in hala) {
    roznica[i] <- abs(dystrybuanta_hipotetyczna[i] - dystrybuanta_empiryczna[i])
  }
  
  # Wartosc statystyki testowej
  wartosc_statystyki_testowej <- max(roznica,na.rm=TRUE)
  
  writeLines("H0 - rozklad normalny\n")
  writeLines("H1 - brak rozkladu normalnego\n")
  
  writeLines("dla starej hali rozklad normalny:\n")
  if(wartosc_statystyki_testowej < tablica_rozkladu || wartosc_statystyki_testowej> 1){
    writeLines("Brak podstaw do odrzucenia hipotezy H0\n")
    writeLines("Dane maja rozklad normalny\n")
  }else{
    writeLines("Istnieja podstawy do odrzucenia hipotezy H0\n")
    writeLines("Dane nie maja rozkladu normalnego\n")
  }
  
}

#koniec zadania drugiego

precyzjaSred <- function(gg, dg, sr){
  precyzjaOszacowaniaTrad = 0.5 * (gg - dg) / sr
  return(precyzjaOszacowaniaTrad)
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


