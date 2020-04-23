#wczytanie danych

dane <- read.csv2("dane.csv", sep=";")
stara_hala <- as.vector(dane[[1]], mode = "double")
nowa_hala <- as.vector(dane[[2]], mode = "double")
nowa_hala <- na.omit(nowa_hala)  #nie uwzględniaj NA

source('funkcje.r')

#miary zróżnicowania pozycyjne
rozstep_stara = max(stara_hala) - min(stara_hala)
rozstep_nowa = max(nowa_hala) - min(nowa_hala)

#miary przeciętne - średnia arytmetyczna (szereg szczegółowy)
suma_stara <- sum(stara_hala)
liczba_stara <- length(stara_hala)

suma_nowa <- sum(nowa_hala)
liczba_nowa <- length(nowa_hala)

srednia_stara <- suma_stara / liczba_stara
srednia_nowa <- suma_nowa / liczba_nowa

#miary zroznicowania klasyczne
wariancja_nowa <- 0
wariancja_stara <- 0
odch_przecietne_nowa <- 0
odch_przecietne_stara <- 0
for(val in nowa_hala)
{
  wariancja_nowa <- wariancja_nowa + (val-srednia_nowa)^2/liczba_nowa
}
for(val in nowa_hala)
{
  odch_przecietne_nowa <- odch_przecietne_nowa + abs(val-srednia_nowa)/liczba_nowa
}
for(val in stara_hala)
{
  wariancja_stara <- wariancja_stara + (val-srednia_stara)^2/liczba_stara
}
for(val in stara_hala)
{
  odch_przecietne_stara <- odch_przecietne_stara + abs(val-srednia_stara)/liczba_stara
}

odchylenie_standardowe_nowa <- sqrt(wariancja_nowa)
odchylenie_standardowe_stara <- sqrt(wariancja_stara)

mediana_nowa <- median(nowa_hala)
mediana_stara <- median(stara_hala)

kwantyl_25_nowa <- quantile(nowa_hala, probs=(0.25))
kwantyl_75_nowa <- quantile(nowa_hala, probs=(0.75))

kwantyl_25_stara <- quantile(stara_hala, probs=(0.25))
kwantyl_75_stara <- quantile(stara_hala, probs=(0.75))

odchylenie_cwiartkowe_nowa <- (kwantyl_75_nowa - kwantyl_25_nowa)/2
odchylenie_cwiartkowe_stara <- (kwantyl_75_stara - kwantyl_25_stara)/2

wspolczynnik_pozycyjny_nowa <- (odchylenie_cwiartkowe_nowa/mediana_nowa)*100
wspolczynnik_pozycyjny_stara <- (odchylenie_cwiartkowe_stara/mediana_stara)*100

#miary zróżnicowania względne
wspolczynnik_zmiennosci_nowa <- (odchylenie_standardowe_nowa/srednia_nowa)*100
wspolczynnik_zmiennosci_stara <- (odchylenie_standardowe_stara/srednia_stara)*100

histogram_nowa <- hist(nowa_hala)
histogram_stara <- hist(stara_hala)

#miary asymetrii  
wspolczynnik_asymetrii_stara = (sum((sort(stara_hala) - srednia_stara) ^ 3) / length(stara_hala))/ (odchylenie_standardowe_stara ^ 3 )
wspolczynnik_asymetrii_nowa = (sum((sort(nowa_hala) - srednia_nowa) ^ 3) / length(nowa_hala))/ (odchylenie_standardowe_nowa ^ 3 )

#miary koncentracji
kurtoza_stara = (sum((sort(stara_hala) - srednia_stara) ^ 4) / length(sort(stara_hala))) / (odchylenie_standardowe_stara ^ 4)
kurtoza_nowa = (sum((sort(nowa_hala) - srednia_nowa) ^ 4) / length(sort(nowa_hala))) / (odchylenie_standardowe_nowa ^ 4)

eksces_stara = kurtoza_stara - 3
eksces_nowa = kurtoza_nowa - 3

#szereg przedziałowy
nowa_przedzial <- cut(nowa_hala, sqrt(liczba_nowa))
stara_przedzial <- cut(stara_hala, sqrt(liczba_stara))

nowa_przedzial <- hist(nowa_hala)
stara_przedzial <- hist(stara_hala)

suma_stara <- sum(stara_przedzial$counts * stara_przedzial$mids)
suma_nowa <- sum(nowa_przedzial$counts * nowa_przedzial$mids)

srednia_przedzial_stara <- suma_stara / sum(stara_przedzial$counts)
srednia_przedzial_nowa <- suma_nowa / sum(nowa_przedzial$counts)

wariancja_przedzial_stara = sum(((stara_przedzial$mids-srednia_przedzial_stara) ^ 2) * stara_przedzial$counts) / sum(stara_przedzial$counts)
wariancja_przedzial_nowa = sum(((nowa_przedzial$mids-srednia_przedzial_nowa) ^ 2) * nowa_przedzial$counts) / sum(nowa_przedzial$counts)

nowa_przedzial_srednia <- sum(nowa_przedzial$counts * nowa_przedzial$mids)/sum(nowa_przedzial$counts)

wspolczynnik_asymetrii_nowa_przedzialowy = (sum((histogram_nowa$mids - srednia_nowa) ^ 3 * histogram_nowa$counts) / sum(histogram_nowa$counts)) / (odchylenie_standardowe_nowa ^ 3)
wspolczynnik_asymetrii_stara_przedzialowy = (sum((histogram_stara$mids - srednia_stara) ^ 3 * histogram_stara$counts) / sum(histogram_stara$counts)) / (odchylenie_standardowe_stara ^ 3)

kurtoza_stara_przedzialowa = (sum((histogram_stara$mids - srednia_stara) ^ 4 * histogram_stara$counts) / sum(histogram_stara$counts)) / (odchylenie_standardowe_stara ^ 4)
kurtoza_nowa_przedzialowa =  (sum((histogram_nowa$mids - srednia_nowa) ^ 4 * histogram_nowa$counts) / sum(histogram_nowa$counts)) / (odchylenie_standardowe_nowa ^ 4)

eksces_stara_przedzialowy = kurtoza_stara_przedzialowa - 3
eksces_nowa_przedzialowy = kurtoza_nowa_przedzialowa - 3

a = 10
b = funkcja_testowa(a)

#2 prosze uprzejmie sprawdzic czy zmiennatablica rozkladu jest dobrze policzona 
tablica_rozkladu_stara=sqrt(length(stara_hala))
tablica_rozkladu_nowa=sqrt(length(nowa_hala))
srednia_stara=sum(stara_hala)/length(stara_hala)
srednia_nowa=sum(nowa_hala)/length(nowa_hala)

#sortowansko starej i nowej 
dane_stara <-sort(stara_hala)
dane_nowa<-sort(nowa_hala)
#standaryzacja starej
standaryzacja_stara<-dane_stara
for(i in stara_hala)
{
  standaryzacja_stara[i]<-(dane_stara[i]-srednia_stara)/odchylenie_standardowe_stara
}
#standaryzacja nowej
standaryzacja_nowa<-dane_nowa
for(i in nowa_hala)
{
  standaryzacja_nowa[i]<-(dane_nowa[i]-srednia_nowa)/odchylenie_standardowe_nowa
}
#dystrybuanta rozkladu hipotetycznego starej
dystrybuanta_hipotetyczna_stara<-dane_stara
for(i in stara_hala) {
  dystrybuanta_hipotetyczna_stara[i] <- pnorm(standaryzacja_stara[i]) #pnorm zwraca funkcję dystrybuanty
}

dystrybuanta_hipotetyczna_nowa<-dane_nowa
for(i in nowa_hala) {
  dystrybuanta_hipotetyczna_nowa[i] <- pnorm(standaryzacja_nowa[i]) #pnorm zwraca funkcję dystrybuanty
}

#dystrybuanta empiryczna starej
dystrybuanta_empiryczna_stara<-dane_stara

for(i in stara_hala) {
  dystrybuanta_empiryczna_stara[i] <- i/length(stara_hala)
}

#dystrybuanta empiryczna nowa
dystrybuanta_empiryczna_nowa<-dane_nowa

for(i in nowa_hala) {
  dystrybuanta_empiryczna_nowa[i] <- i/length(nowa_hala)
}
# roznica dystrybuant
roznica_stara<-0
for(i in stara_hala) {
  roznica_stara[i] <- abs(dystrybuanta_hipotetyczna_stara[i] - dystrybuanta_empiryczna_stara[i])
}
# roznica dystrybuant
roznica_nowa<-0
for(i in nowa_hala) {
  roznica_nowa[i] <- abs(dystrybuanta_hipotetyczna_nowa[i] - dystrybuanta_empiryczna_nowa[i])
}
# Wartosc statystyki testowej
wartosc_statystyki_testowej_stara <- max(roznica_stara,na.rm=TRUE)

wartosc_statystyki_testowej_nowa <- max(roznica_nowa,na.rm=TRUE)

writeLines("H0 - rozkład normalny\n")
writeLines("H1 - brak rozkładu normalnego\n")

writeLines("dla starej hali rozklad normalny:\n")
if(wartosc_statystyki_testowej_stara < tablica_rozkladu_stara || wartosc_statystyki_testowej_stara> 1){
  writeLines("Brak podstaw do odrzucenia hipotezy H0\n")
  writeLines("Dane mają rozkład normalny\n")
}else{
  writeLines("Istnieją podstawy do odrzucenia hipotezy H0\n")
  writeLines("Dane nie maja rozkladu normalnego\n")
}

writeLines("dla nowej hali rozklad normalny:\n")
if(wartosc_statystyki_testowej_nowa < tablica_rozkladu_nowa || wartosc_statystyki_testowej_nowa> 1){
  writeLines("Brak podstaw do odrzucenia hipotezy H0\n")
  writeLines("Dane mają rozkład normalny\n")
}else{
  writeLines("Istnieją podstawy do odrzucenia hipotezy H0\n")
  writeLines("Dane nie maja rozkladu normalnego\n")
}
#koniec zadania 2
#tu zajebalem z innego projektu, trzeba tylko dane pozmieniac xd
#x <- test_zgodnosci(stara_hala)
#y <- test_zgodnosci(nowa_hala)

