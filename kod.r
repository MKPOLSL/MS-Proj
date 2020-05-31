#wczytanie danych

dane <- read.csv2("dane.csv", sep=";")
stara_hala <- as.vector(dane[[1]], mode = "double")
nowa_hala <- as.vector(dane[[2]], mode = "double")
nowa_hala <- na.omit(nowa_hala)  #nie uwzględniaj NA

source('funkcje.r')
source('Zadania.r')

nowa_hala_szczegolowy <- Zadanie1_szczegolowy(nowa_hala)
stara_hala_szczegolowy <- Zadanie1_szczegolowy(stara_hala)
nowa_hala_przedzialowy <- Zadanie1_przedzialowy(nowa_hala, ceiling(sqrt(length(nowa_hala))+1))
stara_hala_przedzialowy <- Zadanie1_przedzialowy(stara_hala, ceiling(sqrt(length(stara_hala))))
print(nowa_hala_szczegolowy)
print(stara_hala_szczegolowy)

#ZADANIE 2

test_zgodnosci(stara_hala)
test_zgodnosci(nowa_hala)

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



