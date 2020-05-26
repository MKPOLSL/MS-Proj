#wczytanie danych

dane <- read.csv2("dane.csv", sep=";")
stara_hala <- as.vector(dane[[1]], mode = "double")
nowa_hala <- as.vector(dane[[2]], mode = "double")
nowa_hala <- na.omit(nowa_hala)  #nie uwzglêdniaj NA

source('funkcje.r')

#miary po³o¿enia (szereg szczegó³ówy)
suma_stara <- sum(stara_hala)
liczba_stara <- length(stara_hala)

suma_nowa <- sum(nowa_hala)
liczba_nowa <- length(nowa_hala)

srednia_stara <- suma_stara / liczba_stara
srednia_nowa <- suma_nowa / liczba_nowa

mediana_nowa <- median(nowa_hala)
mediana_stara <- median(stara_hala)

moda_stara <- modalna(stara_hala)
moda_nowa <- modalna(nowa_hala)

kwantyl_25_nowa <- quantile(nowa_hala, probs=(0.25))
kwantyl_75_nowa <- quantile(nowa_hala, probs=(0.75))

kwantyl_25_stara <- quantile(stara_hala, probs=(0.25))
kwantyl_75_stara <- quantile(stara_hala, probs=(0.75))

#miary rozproszenia
wariancja_nowa <- moment_centralny(nowa_hala, 2)
wariancja_stara <- moment_centralny(stara_hala, 2)

odchylenie_standardowe_nowa <- sqrt(wariancja_nowa)
odchylenie_standardowe_stara <- sqrt(wariancja_stara)

odch_przecietne_nowa <- 0
odch_przecietne_stara <- 0

for(val in nowa_hala){
  odch_przecietne_nowa <- odch_przecietne_nowa + abs(val-srednia_nowa)/liczba_nowa
}
for(val in stara_hala){
  odch_przecietne_stara <- odch_przecietne_stara + abs(val-srednia_stara)/liczba_stara
}

odch_przecietne_od_mediany_nowa <- 0
odch_przecietne_od_mediany_stara <- 0

for(val in nowa_hala){
  odch_przecietne_od_mediany_nowa <- odch_przecietne_od_mediany_nowa + abs(val-mediana_nowa)/liczba_nowa
}
for(val in stara_hala){
  odch_przecietne_od_mediany_stara <- odch_przecietne_od_mediany_stara + abs(val-mediana_stara)/liczba_stara
}

odchylenie_cwiartkowe_nowa <- (kwantyl_75_nowa - kwantyl_25_nowa)/2
odchylenie_cwiartkowe_stara <- (kwantyl_75_stara - kwantyl_25_stara)/2

wspolczynnik_pozycyjny_nowa <- (odchylenie_cwiartkowe_nowa/mediana_nowa)*100
wspolczynnik_pozycyjny_stara <- (odchylenie_cwiartkowe_stara/mediana_stara)*100

wspolczynnik_zmiennosci_nowa <- (odchylenie_standardowe_nowa/srednia_nowa)*100
wspolczynnik_zmiennosci_stara <- (odchylenie_standardowe_stara/srednia_stara)*100

#miary asymetrii i koncentracji 

wspolczynnik_asymetrii_stara <- moment_centralny(stara_hala, 3) / odchylenie_standardowe_stara ^ 3 
wspolczynnik_asymetrii_nowa <- moment_centralny(nowa_hala, 3) / odchylenie_standardowe_nowa ^ 3 

kurtoza_stara <- moment_centralny(stara_hala, 4) / odchylenie_standardowe_stara ^ 4
kurtoza_nowa <- moment_centralny(nowa_hala, 4) / odchylenie_standardowe_nowa ^ 4

eksces_stara = kurtoza_stara - 3
eksces_nowa = kurtoza_nowa - 3


#szereg przedzia³owy

# Szerokoœæ przedzia³u klasowego
szerokosc_stara_hala <- (max(stara_hala) - min(stara_hala)) / ceiling((sqrt(liczba_stara)))
szerokosc_nowa_hala <- (max(nowa_hala) - min(nowa_hala)) /  ceiling((sqrt(liczba_nowa)))

# Punkty przeciêcia
punkty_przeciecia_stara = seq(min(stara_hala), max(stara_hala), szerokosc_stara_hala)
punkty_przeciecia_nowa = seq(min(nowa_hala), max(nowa_hala), szerokosc_nowa_hala)

nowa_histogram <- hist(nowa_hala, breaks = punkty_przeciecia_nowa)
stara_histogram <- hist(stara_hala, breaks = punkty_przeciecia_stara)

suma_stara <- sum(stara_histogram$counts * stara_histogram$mids)
suma_nowa <- sum(nowa_histogram$counts * nowa_histogram$mids)

srednia_przedzial_stara <- suma_stara / sum(stara_histogram$counts)
srednia_przedzial_nowa <- suma_nowa / sum(nowa_histogram$counts)

wariancja_przedzial_stara = sum(((stara_histogram$mids-srednia_przedzial_stara) ^ 2) * stara_histogram$counts) / sum(stara_histogram$counts)
wariancja_przedzial_nowa = sum(((nowa_histogram$mids-srednia_przedzial_nowa) ^ 2) * nowa_histogram$counts) / sum(nowa_histogram$counts)

wspolczynnik_asymetrii_nowa_przedzialowy = (sum((nowa_histogram$mids - srednia_nowa) ^ 3 * nowa_histogram$counts) / sum(nowa_histogram$counts)) / (odchylenie_standardowe_nowa ^ 3)
wspolczynnik_asymetrii_stara_przedzialowy = (sum((stara_histogram$mids - srednia_stara) ^ 3 * stara_histogram$counts) / sum(stara_histogram$counts)) / (odchylenie_standardowe_stara ^ 3)

kurtoza_stara_przedzialowa = (sum((stara_histogram$mids - srednia_przedzial_stara) ^ 4 * stara_histogram$counts) / sum(stara_histogram$counts)) / (odchylenie_standardowe_stara ^ 4)
kurtoza_nowa_przedzialowa =  (sum((nowa_histogram$mids - srednia_przedzial_nowa) ^ 4 * nowa_histogram$counts) / sum(nowa_histogram$counts)) / (odchylenie_standardowe_nowa ^ 4)

eksces_stara_przedzialowy = kurtoza_stara_przedzialowa - 3
eksces_nowa_przedzialowy = kurtoza_nowa_przedzialowa - 3


#ZADANIE 2

#prosze uprzejmie sprawdzic czy zmiennatablica rozkladu jest dobrze policzona 

tablica_rozkladu_stara <- ((0.83+length(stara_hala))/(sqrt(length(stara_hala))))-0.01
tablica_rozkladu_nowa <- ((0.83+length(nowa_hala))/(sqrt(length(nowa_hala))))-0.01
srednia_stara <- sum(stara_hala)/length(stara_hala)
srednia_nowa <- sum(nowa_hala)/length(nowa_hala)

#sortowansko starej i nowej 

dane_stara <- sort(stara_hala)
dane_nowa <- sort(nowa_hala)

standaryzacja_stara <- dane_stara
for(i in dane_stara)
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
  dystrybuanta_hipotetyczna_stara[i] <- pnorm(standaryzacja_stara[i]) #pnorm zwraca funkcjê dystrybuanty
}

dystrybuanta_hipotetyczna_nowa<-dane_nowa
for(i in nowa_hala) {
  dystrybuanta_hipotetyczna_nowa[i] <- pnorm(standaryzacja_nowa[i]) #pnorm zwraca funkcjê dystrybuanty
}
print(dystrybuanta_hipotetyczna_nowa)

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

writeLines("H0 - rozklad normalny\n")
writeLines("H1 - brak rozkladu normalnego\n")

writeLines("dla starej hali rozklad normalny:\n")
if(wartosc_statystyki_testowej_stara < tablica_rozkladu_stara || wartosc_statystyki_testowej_stara> 1){
  writeLines("Brak podstaw do odrzucenia hipotezy H0\n")
  writeLines("Dane maja rozklad normalny\n")
}else{
  writeLines("Istnieja podstawy do odrzucenia hipotezy H0\n")
  writeLines("Dane nie maja rozkladu normalnego\n")
}

writeLines("dla nowej hali rozklad normalny:\n")
if(wartosc_statystyki_testowej_nowa < tablica_rozkladu_nowa || wartosc_statystyki_testowej_nowa> 1){
  writeLines("Brak podstaw do odrzucenia hipotezy H0\n")
  writeLines("Dane maja rozklad normalny\n")
}else{
  writeLines("Istnieja podstawy do odrzucenia hipotezy H0\n")
  writeLines("Dane nie maja rozkladu normalnego\n")
}
#koniec zadania 2
#x <- test_zgodnosci(stara_hala)
#y <- test_zgodnosci(nowa_hala)

# ZADANIE 3
# Wspó³czynnik ufnoœci 1 - alfa = 0.95
# st¹d alfa = 0.05

# Wzglêdna precyzja oszacowania
precyzjaSred <- function(gg, dg, sr){
  precyzjaOszacowaniaTrad = 0.5 * (gg - dg) / sr
  return(precyzjaOszacowaniaTrad)
}

# Przedzia³ ufnoœci dla starej hali
granica_dolna_stara <- dolna_granica_sred(srednia_stara, wspolczynnik_TStudenta(0.95, liczba_stara), odchylenie_standardowe_stara, liczba_stara)
granica_gorna_stara <- gorna_granica_sred(srednia_stara, wspolczynnik_TStudenta(0.95, liczba_stara), odchylenie_standardowe_stara, liczba_stara)
# przedzial <46, 52>
# srednia = 49 wiêc zawiera siê w przedziale ufnoœci

# Precyzja oszacowania dla starej hali
precyzja_stara <- precyzjaSred(granica_gorna_stara, granica_dolna_stara, srednia_stara)

# zadanie 4
# Wspó³czynnik ufnoœci 1 - alfa = 0.95

#Wspó³czynniki chiKwadrat dla: chi(0.975,42), chi(0.025,42)
wspAlfa<-0.05
ChiKwadrat1<-wspChiKwadrat(wspAlfa/2,liczba_nowa) 
ChiKwadrat2<-wspChiKwadrat(1-(wspAlfa/2),liczba_nowa)

#Przedzia³ ufnoœci dla nowej hali
gornaGranicaNowa <- granica_wariancja(liczba_nowa,wariancja_nowa,ChiKwadrat1)
dolnaGranicaNowa <- granica_wariancja(liczba_nowa,wariancja_nowa,ChiKwadrat2)

gornaGranicaNowa <- sqrt(gornaGranicaNowa)
dolnaGranicaNowa <- sqrt(dolnaGranicaNowa)
# przedzial <8, 13>
# odchylenie standardowe = 10, zawiera sie w przedziale

#Precyzja oszacowania dla nowej hali
precyzja_nowa <- wzgledna_precyzja_wariancja(dolnaGranicaNowa,gornaGranicaNowa,wariancja_nowa)

#zadanie 5

# Hipoteza zerowa: wartoœæ wydajnoœci pracy przy produkcji w starej hali s¹ wiêksze
# Hipoteza alternatywna: wartoœæ wydajnoœci pracy przy produkcji w starej hali nie s¹ wiêksze

wartosc.statystyki <- statystyka(nowa_hala, stara_hala)
kwantyl95 <- qnorm(0.95)

print(paste("Obszar przyjec statystyki: (",format(kwantyl95, digits=3), ", nieskonczonosc)"))
print(paste("Obszar krytyczny statystyki: (-nieskonczonosc, ",format(kwantyl95, digits=3), ")"))
print(paste("Wartosc statystyki testowej: ", format(wartosc.statystyki, digits=3)))



