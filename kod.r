#wczytanie danych

dane <- read.csv2("dane.csv", sep=";")
stara_hala <- as.vector(dane[[1]], mode = "double")
nowa_hala <- as.vector(dane[[2]], mode = "double")
nowa_hala <- na.omit(nowa_hala)  #nie uwzględniaj NA

source('funkcje.r')
source('Zadania.r')

#ZADANIE 1

nowa_hala_szczegolowy <- Zadanie1_szczegolowy(nowa_hala)
stara_hala_szczegolowy <- Zadanie1_szczegolowy(stara_hala)
nowa_hala_przedzialowy <- Zadanie1_przedzialowy(nowa_hala)
stara_hala_przedzialowy <- Zadanie1_przedzialowy(stara_hala)
print(nowa_hala_szczegolowy)
print(nowa_hala_przedzialowy)
print(stara_hala_szczegolowy)
print(stara_hala_przedzialowy)

#ZADANIE 2

print("H0 - rozklad normalny")
print("H1 - brak rozkladu normalnego")
Zadanie2(nowa_hala)
Zadanie2(stara_hala)

#ZADANIE 3

Zadanie3(stara_hala)

#ZADANIE 4

Zadanie4(nowa_hala)

#ZADANIE 5

Zadanie5(stara_hala, nowa_hala)



