source("funkcje.r")

Zadanie1_szczegolowy <- function(hala){
  #miary położenia (szereg szczegółówy)
  suma <- sum(hala)
  liczba <- length(hala)
  srednia <- suma / liczba
  
  mediana <- median(hala)
  
  kwantyl_25 <- quantile(hala, probs=(0.25), names = FALSE)
  kwantyl_75 <- quantile(hala, probs=(0.75), names = FALSE)
  
  #miary rozproszenia
  wariancja <- moment_centralny(hala, 2)
  odchylenie_standardowe <- sqrt(wariancja)

  odch_przecietne <- 0
  for(val in hala){
    odch_przecietne <- odch_przecietne + abs(val-srednia)/liczba
  }

  odch_przecietne_od_mediany <- 0
  for(val in hala){
    odch_przecietne_od_mediany <- odch_przecietne_od_mediany + abs(val-mediana)/liczba
  }
  odchylenie_cwiartkowe <- (kwantyl_75 - kwantyl_25)/2
  wspolczynnik_pozycyjny <- (odchylenie_cwiartkowe/mediana)*100
  wspolczynnik_zmiennosci <- (odchylenie_standardowe/srednia)*100
  
  #miary asymetrii i koncentracji 
  
  skosnosc <- moment_centralny(hala, 3) / odchylenie_standardowe ^ 3 
  kurtoza <- moment_centralny(hala, 4) / odchylenie_standardowe ^ 4
  eksces = kurtoza - 3
  
  wynik <- c(srednia, mediana, kwantyl_25, kwantyl_75, wariancja, odchylenie_standardowe, odch_przecietne, odch_przecietne_od_mediany, 
             odchylenie_cwiartkowe, wspolczynnik_pozycyjny, wspolczynnik_zmiennosci, skosnosc, kurtoza, eksces)
  return(wynik)
}

Zadanie1_przedzialowy <- function(hala, n){
  # Szerokość przedziału klasowego
  szerokosc_przedzialu <- (max(hala) - min(hala)) / n
  
  # Punkty przecięcia
  punkty_przeciecia = seq(min(hala), max(hala), szerokosc_przedzialu)
  
  histogram <- hist(hala, breaks = punkty_przeciecia)
 
  #miary polozenia
  suma <- sum(histogram$counts * histogram$mids)
  srednia <- suma / sum(histogram$counts)
  
  mediana <- kwantyl(histogram, 0.5)
  kwantyl_25 <- kwantyl(histogram, 0.25)
  kwantyl_75 <- kwantyl(histogram, 0.75)
  
  moda <- modalna_przedzial(histogram) 
  
  #miary rozproszenia 
  wariancja <- moment_centralny_przedzial(histogram, 2)
  odchylenie_standardowe <- sqrt(wariancja)
  
  odch_przecietne <- sum(abs(histogram$mids - srednia) * histogram$counts) / sum(histogram$counts)
  
  odch_przecietne_od_mediany <- sum(abs(histogram$mids - mediana) * histogram$counts) / sum(histogram$counts)
  
  odchylenie_cwiartkowe <- (kwantyl_75 - kwantyl_25)/2
  
  wspolczynnik_pozycyjny <- (odchylenie_cwiartkowe/mediana)*100
  wspolczynnik_zmiennosci <- (odchylenie_standardowe/srednia)*100
  
  #miary asymetrii i koncentracji
  skosnosc <- moment_centralny_przedzial(histogram, 3) / odchylenie_standardowe ^ 3
  kurtoza <- moment_centralny_przedzial(histogram, 4) / odchylenie_standardowe ^ 4
  eksces <- kurtoza - 3
  wynik <- c(srednia, mediana, kwantyl_25, kwantyl_75, moda, wariancja, odchylenie_standardowe, odch_przecietne, odch_przecietne_od_mediany, 
             odchylenie_cwiartkowe, wspolczynnik_pozycyjny, wspolczynnik_zmiennosci, skosnosc, kurtoza, eksces)
  return(wynik)
  
}