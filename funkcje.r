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

precyzja_oszacowania <- function(gorna_granica, dolna_granica, srednia){
  return((0.5 * (gorna_granica - dolna_granica) / srednia) * 100)
}

wspolczynnik_TStudenta <- function(ufn, n) {
  return(qt((1 - ufn) / 2, n - 1, lower.tail = FALSE, log.p = FALSE))
}

# Przedzialy ufnosci dla sredniej
dolna_granica_sred <- function(srednia, wspT, odch, licz) {
  dolna_granica = srednia - wspT * (odch / sqrt(licz - 1))
  return(dolna_granica)
}

gorna_granica_sred <- function(srednia, wspT, odch, liczba) {
  gorna_granica = srednia + wspT * (odch / sqrt(liczba - 1))
  return(gorna_granica)
}

wspChiKwadrat<-function(ufn,n){
  return (qchisq(ufn,n-1))
}
#Granice przedzialu ufnosci dla wariancji wartsci pracy nowej hali
granica_wariancja<-function(n,war,wspChi){
  return(n*war/wspChi)
}
#Wzgledna precyzja oszacowania
wzgledna_precyzja_wariancja <- function(dolna_granica,gorna_granica,wariancja){
  return((0.5 * (gorna_granica - dolna_granica) / wariancja) * 100)
}

statystyka <- function(x, y) {
  # Funkcja zwraca wartosc statystyki testowej dla porownania
  # dwoch srednich z prob podanych jako argumenty
  x.srednia <- mean(x)
  x.wariancja <- var(x)
  y.srednia <- mean(y)
  y.wariancja <- var(y)
  
  U = (x.srednia - y.srednia) / sqrt((x.wariancja / length(x)) + (y.wariancja / length(y)))
  
  return(U)
}


