#wczytanie danych

dane <- read.csv2("dane.csv", sep=";")
stara_hala <- as.vector(dane[[1]], mode = "double")
nowa_hala <- as.vector(dane[[2]], mode = "double")
nowa_hala <- na.omit(nowa_hala)  #nie uwzględniaj NA

source('funkcje.r')

#miary położenia (szereg szczegółówy)
suma_stara <- sum(stara_hala)
suma_nowa <- sum(nowa_hala)

liczba_stara <- length(stara_hala)
liczba_nowa <- length(nowa_hala)

srednia_stara <- suma_stara / liczba_stara
srednia_nowa <- suma_nowa / liczba_nowa

mediana_stara <- median(stara_hala)
mediana_nowa <- median(nowa_hala)

moda_stara <- modalna(stara_hala)
moda_nowa <- modalna(nowa_hala)

kwantyl_25_stara <- quantile(stara_hala, probs=(0.25), names = FALSE)
kwantyl_75_stara <- quantile(stara_hala, probs=(0.75), names = FALSE)

kwantyl_25_nowa <- quantile(nowa_hala, probs=(0.25), names = FALSE)
kwantyl_75_nowa <- quantile(nowa_hala, probs=(0.75), names = FALSE)


#miary rozproszenia
wariancja_stara <- moment_centralny(stara_hala, 2)
wariancja_nowa <- moment_centralny(nowa_hala, 2)

odchylenie_standardowe_stara <- sqrt(wariancja_stara)
odchylenie_standardowe_nowa <- sqrt(wariancja_nowa)

odch_przecietne_stara <- 0
odch_przecietne_nowa <- 0

for(val in stara_hala){
  odch_przecietne_stara <- odch_przecietne_stara + abs(val-srednia_stara)/liczba_stara
}
for(val in nowa_hala){
  odch_przecietne_nowa <- odch_przecietne_nowa + abs(val-srednia_nowa)/liczba_nowa
}

odch_przecietne_od_mediany_stara <- 0
odch_przecietne_od_mediany_nowa <- 0

for(val in stara_hala){
  odch_przecietne_od_mediany_stara <- odch_przecietne_od_mediany_stara + abs(val-mediana_stara)/liczba_stara
}
for(val in nowa_hala){
  odch_przecietne_od_mediany_nowa <- odch_przecietne_od_mediany_nowa + abs(val-mediana_nowa)/liczba_nowa
}

odchylenie_cwiartkowe_stara <- (kwantyl_75_stara - kwantyl_25_stara)/2
odchylenie_cwiartkowe_nowa <- (kwantyl_75_nowa - kwantyl_25_nowa)/2

wspolczynnik_pozycyjny_stara <- (odchylenie_cwiartkowe_stara/mediana_stara)*100
wspolczynnik_pozycyjny_nowa <- (odchylenie_cwiartkowe_nowa/mediana_nowa)*100

wspolczynnik_zmiennosci_stara <- (odchylenie_standardowe_stara/srednia_stara)*100
wspolczynnik_zmiennosci_nowa <- (odchylenie_standardowe_nowa/srednia_nowa)*100

#miary asymetrii i koncentracji 

skosnosc_stara <- moment_centralny(stara_hala, 3) / odchylenie_standardowe_stara ^ 3 
skosnosc_nowa <- moment_centralny(nowa_hala, 3) / odchylenie_standardowe_nowa ^ 3 

kurtoza_stara <- moment_centralny(stara_hala, 4) / odchylenie_standardowe_stara ^ 4
kurtoza_nowa <- moment_centralny(nowa_hala, 4) / odchylenie_standardowe_nowa ^ 4

eksces_stara = kurtoza_stara - 3
eksces_nowa = kurtoza_nowa - 3


#szereg przedziałowy

# Szerokość przedziału klasowego
szerokosc_stara_hala <- (max(stara_hala) - min(stara_hala)) / ceiling(sqrt(liczba_stara))
szerokosc_nowa_hala <- (max(nowa_hala) - min(nowa_hala)) /  ceiling(sqrt(liczba_nowa) + 1)

# Punkty przecięcia
punkty_przeciecia_stara = seq(min(stara_hala), max(stara_hala), szerokosc_stara_hala)
punkty_przeciecia_nowa = seq(min(nowa_hala), max(nowa_hala), szerokosc_nowa_hala)

stara_histogram <- hist(stara_hala, breaks = punkty_przeciecia_stara)
nowa_histogram <- hist(nowa_hala, breaks = punkty_przeciecia_nowa)

#miary polozenia
suma_stara <- sum(stara_histogram$counts * stara_histogram$mids)
suma_nowa <- sum(nowa_histogram$counts * nowa_histogram$mids)

srednia_przedzial_stara <- suma_stara / sum(stara_histogram$counts)
srednia_przedzial_nowa <- suma_nowa / sum(nowa_histogram$counts)

mediana_przedzial_stara <- kwantyl(stara_histogram, 0.5)
mediana_przedzial_nowa <- kwantyl(nowa_histogram, 0.5)

kwantyl_25_przedzial_stara <- kwantyl(stara_histogram, 0.25)
kwantyl_75_przedzial_stara <- kwantyl(stara_histogram, 0.75)

kwantyl_25_przedzial_nowa <- kwantyl(nowa_histogram, 0.25)
kwantyl_75_przedzial_nowa <- kwantyl(nowa_histogram, 0.75)

moda_stara_przedzial <- modalna_przedzial(stara_histogram) 
moda_nowa_przedzial <- modalna_przedzial(nowa_histogram)

#miary rozproszenia 
wariancja_przedzial_stara <- moment_centralny_przedzial(stara_histogram, 2)
wariancja_przedzial_nowa <- moment_centralny_przedzial(nowa_histogram, 2)

odchylenie_standard_przedzial_stara <- sqrt(wariancja_przedzial_stara)
odchylenie_standard_przedzial_nowa <- sqrt(wariancja_przedzial_nowa)

odch_przecietne_przedzial_stara <- sum(abs(stara_histogram$mids - srednia_przedzial_stara) * stara_histogram$counts) / sum(stara_histogram$counts)
odch_przecietne_przedzial_nowa <- sum(abs(nowa_histogram$mids - srednia_przedzial_nowa) * nowa_histogram$counts) / sum(nowa_histogram$counts)

odch_med_przedzial_stara <- sum(abs(stara_histogram$mids - mediana_przedzial_stara) * stara_histogram$counts) / sum(stara_histogram$counts)
odch_med_przedzial_nowa <- sum(abs(nowa_histogram$mids - mediana_przedzial_nowa) * nowa_histogram$counts) / sum(nowa_histogram$counts)

odchylenie_cwiartkowe_przedzial_stara <- (kwantyl_75_przedzial_stara - kwantyl_25_przedzial_stara)/2
odchylenie_cwiartkowe_przedzial_nowa <- (kwantyl_75_przedzial_nowa - kwantyl_25_przedzial_nowa)/2

wspolczynnik_pozycyjny_przedzial_stara <- (odchylenie_cwiartkowe_przedzial_stara/mediana_przedzial_stara)*100
wspolczynnik_pozycyjny_przedzial_nowa <- (odchylenie_cwiartkowe_przedzial_nowa/mediana_przedzial_nowa)*100

wspolczynnik_zmiennosci_przedzial_stara <- (odchylenie_standard_przedzial_stara/srednia_przedzial_stara)*100
wspolczynnik_zmiennosci_przedzial_nowa <- (odchylenie_standard_przedzial_nowa/srednia_przedzial_nowa)*100

#miary asymetrii i koncentracji
skosnosc_stara_przedzialowy <- moment_centralny_przedzial(stara_histogram, 3) / odchylenie_standard_przedzial_stara ^ 3
skosnosc_nowa_przedzialowy <- moment_centralny_przedzial(nowa_histogram, 3) / odchylenie_standard_przedzial_nowa ^ 3

kurtoza_stara_przedzialowa <- moment_centralny_przedzial(stara_histogram, 4) / odchylenie_standard_przedzial_stara ^ 4
kurtoza_nowa_przedzialowa <-  moment_centralny_przedzial(nowa_histogram, 4) / odchylenie_standard_przedzial_nowa ^ 4

eksces_stara_przedzialowy <- kurtoza_stara_przedzialowa - 3
eksces_nowa_przedzialowy <- kurtoza_nowa_przedzialowa - 3


#ZADANIE 2


#koniec zadania 2
x <- test_zgodnosci(stara_hala)
y <- test_zgodnosci(nowa_hala)

# ZADANIE 3
# Współczynnik ufności 1 - alfa = 0.95
# stąd alfa = 0.05

# Względna precyzja oszacowania

# Przedział ufności dla starej hali
granica_dolna_stara <- dolna_granica_sred(srednia_stara, wspolczynnik_TStudenta(0.95, liczba_stara), odchylenie_standardowe_stara, liczba_stara)
granica_gorna_stara <- gorna_granica_sred(srednia_stara, wspolczynnik_TStudenta(0.95, liczba_stara), odchylenie_standardowe_stara, liczba_stara)
# przedzial <46, 52>
# srednia = 49 więc zawiera się w przedziale ufności

# Precyzja oszacowania dla starej hali
precyzja_stara <- precyzjaSred(granica_gorna_stara, granica_dolna_stara, srednia_stara)

# zadanie 4
# Współczynnik ufności 1 - alfa = 0.95

#Współczynniki chiKwadrat dla: chi(0.975,42), chi(0.025,42)
wspAlfa<-0.05
ChiKwadrat1<-wspChiKwadrat(wspAlfa/2,liczba_nowa) 
ChiKwadrat2<-wspChiKwadrat(1-(wspAlfa/2),liczba_nowa)

#Przedział ufności dla nowej hali
gornaGranicaNowa <- granica_wariancja(liczba_nowa,wariancja_nowa,ChiKwadrat1)
dolnaGranicaNowa <- granica_wariancja(liczba_nowa,wariancja_nowa,ChiKwadrat2)

gornaGranicaNowa <- sqrt(gornaGranicaNowa)
dolnaGranicaNowa <- sqrt(dolnaGranicaNowa)
# przedzial <8, 13>
# odchylenie standardowe = 10, zawiera sie w przedziale

#Precyzja oszacowania dla nowej hali
precyzja_nowa <- wzgledna_precyzja_wariancja(dolnaGranicaNowa,gornaGranicaNowa,wariancja_nowa)

#zadanie 5

# Hipoteza zerowa: wartość wydajności pracy przy produkcji w starej hali są większe
# Hipoteza alternatywna: wartość wydajności pracy przy produkcji w starej hali nie są większe

wartosc.statystyki <- statystyka(nowa_hala, stara_hala)
kwantyl95 <- qnorm(0.95)

print(paste("Obszar przyjec statystyki: (",format(kwantyl95, digits=3), ", nieskonczonosc)"))
print(paste("Obszar krytyczny statystyki: (-nieskonczonosc, ",format(kwantyl95, digits=3), ")"))
print(paste("Wartosc statystyki testowej: ", format(wartosc.statystyki, digits=3)))



