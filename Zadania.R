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

Zadanie2 <- function(hala){
  #wartosci potrzebne w dalszej czesci testu
  
  
  tablica_rozkladu<-sqrt(length(hala))
  srednia=sum(hala)/length(hala)
  #odchyenie standardowe
  wariancja <- 0
  for(val in hala){
    wariancja <- wariancja + (val-srednia)^2/length(hala)
  }
  odchylenie_standardowe <- sqrt(wariancja)
  
  #sortowanie starej i nowej 
  dane <-sort(hala)
  #standaryzacja starej
  standaryzacja<-dane
  for(i in hala){
    standaryzacja[i]<-(dane[i]-srednia)/odchylenie_standardowe
  }
  
  #dystrybuanta rozkladu hipotetycznego starej
  dystrybuanta_hipotetyczna<-dane
  for(i in hala) {
    dystrybuanta_hipotetyczna[i] <- pnorm(standaryzacja[i]) #pnorm zwraca funkcję dystrybuanty
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
  
  if(wartosc_statystyki_testowej < tablica_rozkladu || wartosc_statystyki_testowej> 1){
    print("Brak podstaw do odrzucenia hipotezy H0")
    print("Dane maja rozklad normalny")
  }else{
    print("Istnieja podstawy do odrzucenia hipotezy H0")
    print("Dane nie maja rozkladu normalnego")
  }
  
}

Zadanie3 <- function(hala){
  # Współczynnik ufności 1 - alfa = 0.95
  # stąd alfa = 0.05
  liczba <- length(hala)
  srednia <- sum(hala) / liczba
  odch <- sqrt(var(hala))
  
  # Przedział ufności dla starej hali
  granica_dolna <- dolna_granica_sred(srednia, wspolczynnik_TStudenta(0.95, liczba), odch, liczba)
  granica_gorna <- gorna_granica_sred(srednia, wspolczynnik_TStudenta(0.95, liczba), odch, liczba)

  print(paste("<",granica_dolna,",",granica_gorna,">"))
  print(paste("Srednia: ",format(srednia, digits=3)))
  if(srednia < granica_gorna & srednia > granica_dolna){
    print("Srednia zawiera sie w przedziale ufnosci, mamy podstawy do uogolniania przedzialu ufnosci na cala populacje")
  } else print("Srednia nie zawiera sie w przedziale ufnosci, nie mamy pods````````````````````````````taw do uogolniania przedzialu ufnosci na cala populacje")
  # Precyzja oszacowania dla starej hali
  precyzja <- precyzja_oszacowania(granica_gorna, granica_dolna, srednia)
  print(paste("Wzgledna precyzja oszacowania: ", format(precyzja, digits=3)))
}

Zadanie4 <- function(hala){
  # Współczynnik ufności 1 - alfa = 0.95
  liczba <- length(hala)
  srednia <- sum(hala) / liczba
  wariancja <- var(hala)
  odch <- sqrt(wariancja)
  
  #Współczynniki chiKwadrat dla: chi(0.975,42), chi(0.025,42)
  wspAlfa<-0.05
  ChiKwadrat1<-wspChiKwadrat(wspAlfa/2,liczba) 
  ChiKwadrat2<-wspChiKwadrat(1-(wspAlfa/2),liczba)
  
  #Przedział ufności 
  granica_gorna <- granica_wariancja(liczba,wariancja,ChiKwadrat1)
  granica_dolna <- granica_wariancja(liczba,wariancja,ChiKwadrat2)
  
  granica_gorna <- sqrt(granica_gorna)
  granica_dolna <- sqrt(granica_dolna)
  
  print(paste("<",granica_dolna,",",granica_gorna,">"))
  print(paste("Odchylenie standardowe: ",format(odch, digits=3)))
  if(odch < granica_gorna & odch > granica_dolna){
    print("Odchylenie standardowe zawiera sie w przedziale ufnosci, mamy podstawy do uogolniania przedzialu ufnosci na cala populacje")
  } else print("Odchylenie standardowe nie zawiera sie w przedziale ufnosci, nie mamy podstaw do uogolniania przedzialu ufnosci na cala populacje")
  
  #Precyzja oszacowania 
  precyzja <- wzgledna_precyzja_wariancja(granica_dolna,granica_gorna,wariancja)
  print(paste("Wzgledna precyzja oszacowania: ", format(precyzja, digits=3)))
}

Zadanie5 <- function(stara_hala, nowa_hala)
{
  # Hipoteza zerowa: wartość wydajności pracy przy produkcji w starej hali są większe
  # Hipoteza alternatywna: wartość wydajności pracy przy produkcji w starej hali nie są większe
  
  wartosc_statystyki <- statystyka(stara_hala, nowa_hala)
  kwantyl975 <- qnorm(0.975)
  
  print(paste("Obszar przyjec statystyki: (",format(kwantyl975, digits=3), ", nieskonczonosc)"))
  print(paste("Obszar krytyczny statystyki: (-nieskonczonosc, ",format(kwantyl975, digits=3), ")"))
  print(paste("Wartosc statystyki testowej: ", format(wartosc_statystyki, digits=3)))
  if(wartosc_statystyki < kwantyl975){
    print("Istnieja podstawy do odrzucenia hipotezy zerowej")
  } else print("Brak podstaw do odrzucenia hipotezy zerowej")
}
