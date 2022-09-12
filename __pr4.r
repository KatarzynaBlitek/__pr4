#Zalozenia symulacji:
#Generujemy grupy o rozkdzie normalnym, o tej samej wariancji oraz wartosciach oczekiwanych
#(Rozklad normalny i rownosc wariancji to zalozenia Anovy)
#Wartosci oczekiwane w grupach musza byc rowne - bedziemy sprawdzali jak czesto testy odrzucaja prawdziwa hipoteze zerowa
#H0: Wartosci oczekiwane w podgrupach sa ronwe (dla obu testow)
#Bedziemy przeprowadzali symulacje dla 3 podgrup (Polecenie)
#Wyniki bedziemy opisywac w zaleznosci od:
#-liczebnosci generowanych grup
#-roznicy pomiedzy liczebnosciami generowanych grup
#Podgrupy musza miec zawsze rozna liczebnosc (Polecenie)


#Symulacje przeprowadzimy dla rozkladu normalnego (-3,8)

#1. ANOVA

#1a) Symulacja w zaleznosci od liczebnosci generowanych prob

#Zaczniemy od liczebnsosci 10,20,30 w podgrupach (60 danych w zbiorze) i bedziemy zwiekszac o 3 (Czyli o 1 w ka¿dej podgrupie) a¿ do 150,160,170 (480 danych w zbiorze)
#Liczebnoæ zawsze bedzie roznic sie o 10

#Potem przeprowadzimy symulacje dla roznic: 20, 30, 40

#Roznica w liczebnosci miedzy grupami - 10
roznica <- 10
start <- 10
pv <- c(1:1000)
wektor_srednich1 <- c(1:141)
pom <- 0

for(j in seq(60,480,3))
{
  for(i in 1:1000)
  {
    r_normalny <- rnorm(j,-3,8)
    wektor_grup <- c(rep(1,start+pom),rep(2,start+pom+roznica),rep(3,start+pom+roznica+roznica))
    dane <- data.frame(grupa=wektor_grup,obserwacja=r_normalny)
    aov(obserwacja ~ grupa, data = dane) -> model
    pv[i] <-unlist(summary(model))['Pr(>F)1']
  }
  pom <- pom + 1
  wektor_srednich1[pom] <- mean(pv<0.05)
}
l_danych1 <- seq(60, 480,3)
plot(l_danych1, wektor_srednich1, type="l",main="Anova - roznica liczebnosci miedzy grupami - 10", ylab = "Osetek odrzucen H0", xlab="Liczba danych w calym zbiorze")

#Wnioski - Zwiekszanie liczby danych w generowanych podgrupach (Przy zachowaniu tej samej roznicy miedzy grupami)
#nie ma wplywu na odsetek odrzucen prawdziwej hipotezy H0. Utrzymuje sie on miedzy 0,03 a 0,07 (Dla przyjetego poziomu istotnosci 0,05)
#niezaleznie od ilosci obserwacji w podgrupach

#Roznica w liczebnosci miedzy grupami - 20
roznica <- 20
start <- 10
pom <- 0
wektor_srednich2 <- c(1:141)

for(j in seq(90,510,3))
{
  for(i in 1:1000)
  {
    r_normalny <- rnorm(j,-3,8)
    wektor_grup <- c(rep(1,start+pom),rep(2,start+pom+roznica),rep(3,start+pom+roznica+roznica))
    dane <- data.frame(grupa=wektor_grup,obserwacja=r_normalny)
    aov(obserwacja ~ grupa, data = dane) -> model
    pv[i] <-unlist(summary(model))['Pr(>F)1']
  }
  pom <- pom + 1
  wektor_srednich2[pom] <- mean(pv<0.05)
}
l_danych2 <- seq(90, 510,3)
plot(l_danych2, wektor_srednich2, type="l",main="Anova - roznica liczebnosci miedzy grupami - 20", ylab = "Osetek odrzucen H0", xlab="Liczba danych w calym zbiorze")
#Wnioski - tak jak w powyzszym przypadku odsetek odrzucen H0 utrzymuje sie miedzy 0,03 a 0,07

#Roznica w liczebnosci miedzy grupami - 30
roznica <- 30
start <- 10
pom <- 0
wektor_srednich3 <- c(1:141)

for(j in seq(120,540,3))
{
  for(i in 1:1000)
  {
    r_normalny <- rnorm(j,-3,8)
    wektor_grup <- c(rep(1,start+pom),rep(2,start+pom+roznica),rep(3,start+pom+roznica+roznica))
    dane <- data.frame(grupa=wektor_grup,obserwacja=r_normalny)
    aov(obserwacja ~ grupa, data = dane) -> model
    pv[i] <-unlist(summary(model))['Pr(>F)1']
  }
  pom <- pom + 1
  wektor_srednich3[pom] <- mean(pv<0.05)
}
l_danych3 <- seq(120, 540,3)
plot(l_danych3, wektor_srednich3, type="l",main="Anova - roznica liczebnosci miedzy grupami - 30", ylab = "Osetek odrzucen H0", xlab="Liczba danych w calym zbiorze")
#Wnioski - tak jak w poprzednim przypadku, odsetek odrzucen prawdziwej hipotezy H0 utrzymuje sie ciagle w okolicy 0,05


#Roznica w liczebnosci miedzy grupami - 40
roznica <- 40
start <- 10
pom <- 0
wektor_srednich4 <- c(1:141)

for(j in seq(150,570,3))
{
  for(i in 1:1000)
  {
    r_normalny <- rnorm(j,-3,8)
    wektor_grup <- c(rep(1,start+pom),rep(2,start+pom+roznica),rep(3,start+pom+roznica+roznica))
    dane <- data.frame(grupa=wektor_grup,obserwacja=r_normalny)
    aov(obserwacja ~ grupa, data = dane) -> model
    pv[i] <-unlist(summary(model))['Pr(>F)1']
  }
  pom <- pom + 1
  wektor_srednich4[pom] <- mean(pv<0.05)
}
l_danych4 <- seq(150, 570,3)
plot(l_danych4, wektor_srednich4, type="l",main="Anova - roznica liczebnosci miedzy grupami - 40", ylab = "Osetek odrzucen H0", xlab="Liczba danych w calym zbiorze")
#Wnioski - takie same jak w powyzszych przypadkach


plot(l_danych4, wektor_srednich1, type="l", col="blue",main="Anova - te same roznice miedzy podgrupami", ylab = "Odsetek odrzucen H0", xlab="Liczba danych w calym zbiorze")
lines(l_danych4, wektor_srednich2, type="l", col="red")
lines(l_danych4, wektor_srednich3, type="l", col="green")
lines(l_danych4, wektor_srednich4, type="l", col="yellow")
legend("topleft",inset=0.02,legend=c("Roznica 10","Roznica 20", "Roznica 30", "Roznica 40"), col=c("blue", "red", "green", "yellow"), pch=c(15,15), cex=0.60)

#Wnioski ogolne - Jak wynika z tego nieczytelnego wykresu dla wszystkich przeprowadzonych symulacji odsetek odrzucen utrzymuje sie w okolicach 0,05 (+/- 0,02)
#Nie ma na to wplywu ani wzrost liczby obserwacji w generowanych podgrupach ani wzrost roznicy w liczebnosci generowanych podgrup.


#1b) Symulacja w zaleznosci od roznic pomiedzy generowanymi grupami
#Przy przeprowadzaniu tej symulacji bedziemy zwiekszali nie tylko liczebnosc podgrup ale rowniez roznice pomiedzy liczebnosciami poszczegolnych podgrup (Wczesniej te roznice byly stale)

roznica <- 10
start <- 10
pv <- c(1:1000)
wektor_srednich5 <- c(1:101)
pom <- 0
pom2 <- 0

for(j in seq(60,1260,12))
{
  for(i in 1:1000)
  {
    r_normalny <- rnorm(j,-3,8)
    wektor_grup <- c(rep(1,start+pom),rep(2,start+pom+roznica+pom),rep(3,start+pom+roznica+roznica+pom+pom))
    dane <- data.frame(grupa=wektor_grup,obserwacja=r_normalny)
    aov(obserwacja ~ grupa, data = dane) -> model
    pv[i] <-unlist(summary(model))['Pr(>F)1']
  }
  pom <- pom + 2
  pom2 <- pom2 + 1
  wektor_srednich5[pom2] <- mean(pv<0.05)
}
l_danych5 <- seq(60,1260,12)
plot(l_danych5, wektor_srednich5, type="l",main="Anova - wzrost roznicy w liczebnosci miedzy grupami", ylab = "Osetek odrzucen H0", xlab="Liczba danych w calym zbiorze")

#Wnioski - Tak jak w przypadku poprzedniej symulacji, zwiekszanie roznic pomiedzy liczebnosciami poszczegolnych podgrup nie ma wplywu na odsetek odrzucen prawdziwej hipotezy H0.
#Odsetek utrzymuje sie w okolicach 0.05 (+/- 0.02), przy przyjetym poziomie istotnosci 0.05.

#1c) Zmiana poziomu istotnosci na 0.1
roznica <- 10
start <- 10
pv <- c(1:1000)
wektor_srednich52 <- c(1:101)
pom <- 0
pom2 <- 0

for(j in seq(60,1260,12))
{
  for(i in 1:1000)
  {
    r_normalny <- rnorm(j,-3,8)
    wektor_grup <- c(rep(1,start+pom),rep(2,start+pom+roznica+pom),rep(3,start+pom+roznica+roznica+pom+pom))
    dane <- data.frame(grupa=wektor_grup,obserwacja=r_normalny)
    aov(obserwacja ~ grupa, data = dane) -> model
    pv[i] <-unlist(summary(model))['Pr(>F)1']
  }
  pom <- pom + 2
  pom2 <- pom2 + 1
  wektor_srednich52[pom2] <- mean(pv<0.1)
}
l_danych5 <- seq(60,1260,12)
plot(l_danych5, wektor_srednich52, type="l",main="Anova - poziom istotnosci 0.1", ylab = "Osetek odrzucen H0", xlab="Liczba danych w calym zbiorze")

#Wnioski - zgodnie z przypuszczeniami odsetek odrzucen prawdziwej hipotezy H0 przy zmianie poziomu istotnosci na 0.1 utrzymuje sie wlasnie na tym poziomie (+/- 0.05)


#2. TEST KRUSKALA-WALLISA

#Dla testu Kruskalla-Wallisa przeprowadzimy te same symulacje co dla Anovy

#Roznica w liczebnosci miedzy grupami - 10
roznica <- 10
start <- 10
pv <- c(1:1000)
wektor_srednich6 <- c(1:141)
pom <- 0

for(j in seq(60,480,3))
{
  for(i in 1:1000)
  {
    r_normalny <- rnorm(j,-3,8)
    wektor_grup <- c(rep(1,start+pom),rep(2,start+pom+roznica),rep(3,start+pom+roznica+roznica))
    dane <- data.frame(grupa=wektor_grup,obserwacja=r_normalny)
    pv[i] <-  kruskal.test(obserwacja ~ grupa, data = dane)$p.value
  }
  pom <- pom + 1
  wektor_srednich6[pom] <- mean(pv<0.05)
}
l_danych6 <- seq(60, 480,3)
plot(l_danych6, wektor_srednich6, type="l",main="Kruskal-Wallis - roznica liczebnosci miedzy grupami - 10", ylab = "Osetek odrzucen H0", xlab="Liczba danych w calym zbiorze")

#Wnioski - Tak jak w przypadku Anovy zwiekszanie liczby danych w generowanych podgrupach (Przy zachowaniu tej samej roznicy miedzy grupami)
#nie ma wplywu na odsetek odrzucen prawdziwej hipotezy H0. Utrzymuje sie on miedzy 0,03 a 0,07 (Dla przyjetego poziomu istotnosci 0,05)
#niezaleznie od ilosci obserwacji w podgrupach

#Roznica w liczebnosci miedzy grupami - 20
roznica <- 20
start <- 10
pom <- 0
wektor_srednich7 <- c(1:141)

for(j in seq(90,510,3))
{
  for(i in 1:1000)
  {
    r_normalny <- rnorm(j,-3,8)
    wektor_grup <- c(rep(1,start+pom),rep(2,start+pom+roznica),rep(3,start+pom+roznica+roznica))
    dane <- data.frame(grupa=wektor_grup,obserwacja=r_normalny)
    pv[i] <-  kruskal.test(obserwacja ~ grupa, data = dane)$p.value
  }
  pom <- pom + 1
  wektor_srednich7[pom] <- mean(pv<0.05)
}
l_danych7 <- seq(90, 510,3)
plot(l_danych7, wektor_srednich7, type="l",main="Kruskal-Wallis - roznica liczebnosci miedzy grupami - 20", ylab = "Osetek odrzucen H0", xlab="Liczba danych w calym zbiorze")
#Wnioski - tak jak w powyzszym przypadku odsetek odrzucen H0 utrzymuje sie miedzy 0,03 a 0,07

#Roznica w liczebnosci miedzy grupami - 30
roznica <- 30
start <- 10
pom <- 0
wektor_srednich8 <- c(1:141)

for(j in seq(120,540,3))
{
  for(i in 1:1000)
  {
    r_normalny <- rnorm(j,-3,8)
    wektor_grup <- c(rep(1,start+pom),rep(2,start+pom+roznica),rep(3,start+pom+roznica+roznica))
    dane <- data.frame(grupa=wektor_grup,obserwacja=r_normalny)
    pv[i] <-  kruskal.test(obserwacja ~ grupa, data = dane)$p.value
  }
  pom <- pom + 1
  wektor_srednich8[pom] <- mean(pv<0.05)
}
l_danych8 <- seq(120, 540,3)
plot(l_danych8, wektor_srednich8, type="l",main="Kruskal-Wallis - roznica liczebnosci miedzy grupami - 30", ylab = "Osetek odrzucen H0", xlab="Liczba danych w calym zbiorze")
#Wnioski - jak wy¿ej


#Roznica w liczebnosci miedzy grupami - 40
roznica <- 40
start <- 10
pom <- 0
wektor_srednich9 <- c(1:141)

for(j in seq(150,570,3))
{
  for(i in 1:1000)
  {
    r_normalny <- rnorm(j,-3,8)
    wektor_grup <- c(rep(1,start+pom),rep(2,start+pom+roznica),rep(3,start+pom+roznica+roznica))
    dane <- data.frame(grupa=wektor_grup,obserwacja=r_normalny)
    pv[i] <-  kruskal.test(obserwacja ~ grupa, data = dane)$p.value
  }
  pom <- pom + 1
  wektor_srednich9[pom] <- mean(pv<0.05)
}
l_danych9 <- seq(150, 570,3)
plot(l_danych9, wektor_srednich9, type="l",main="Kruskal-Wallis - roznica liczebnosci miedzy grupami - 40", ylab = "Osetek odrzucen H0", xlab="Liczba danych w calym zbiorze")
#Wnioski - takie same jak w powyzszych przypadkach


plot(l_danych9, wektor_srednich6, type="l", col="blue",main="Kruskal-Wallis - te same roznice miedzy podgrupami", ylab = "Odsetek odrzucen H0", xlab="Liczba danych w calym zbiorze")
lines(l_danych9, wektor_srednich7, type="l", col="red")
lines(l_danych9, wektor_srednich8, type="l", col="green")
lines(l_danych9, wektor_srednich9, type="l", col="yellow")
legend("topleft",inset=0.02,legend=c("Roznica 10","Roznica 20", "Roznica 30", "Roznica 40"), col=c("blue", "red", "green", "yellow"), pch=c(15,15), cex=0.60)
#Wnioski ogólne - dokadnie takie same jak w przypadku Anovy.


#2b) Symulacja w zaleznosci od roznic pomiedzy generowanymi grupami
#Przy przeprowadzaniu tej symulacji bedziemy zwiekszali nie tylko liczebnosc podgrup ale rowniez roznice pomiedzy liczebnosciami poszczegolnych podgrup (Wczesniej te roznice byly stale)

roznica <- 10
start <- 10
pv <- c(1:1000)
wektor_srednich10 <- c(1:101)
pom <- 0
pom2 <- 0

for(j in seq(60,1260,12))
{
  for(i in 1:1000)
  {
    r_normalny <- rnorm(j,-3,8)
    wektor_grup <- c(rep(1,start+pom),rep(2,start+pom+roznica+pom),rep(3,start+pom+roznica+roznica+pom+pom))
    dane <- data.frame(grupa=wektor_grup,obserwacja=r_normalny)
    pv[i] <-  kruskal.test(obserwacja ~ grupa, data = dane)$p.value
  }
  pom <- pom + 2
  pom2 <- pom2 + 1
  wektor_srednich10[pom2] <- mean(pv<0.05)
}
l_danych10 <- seq(60,1260,12)
plot(l_danych10, wektor_srednich10, type="l",main="Kruskal-Wallis - wzrost roznicy w liczebnosci miedzy grupami", ylab = "Osetek odrzucen H0", xlab="Liczba danych w calym zbiorze")

#Wnioski - Tak jak w przypadku poprzedniej symulacji, zwiekszanie roznic pomiedzy liczebnosciami poszczegolnych podgrup nie ma wplywu na odsetek odrzucen prawdziwej hipotezy H0.
#Odsetek utrzymuje sie w okolicach 0.05 (+/- 0.02), przy przyjetym poziomie istotnosci 0.05. (Te same wnioski co przy Anovie)

#2c) Zmiana poziomu istotnosci na 0.1
roznica <- 10
start <- 10
pv <- c(1:1000)
wektor_srednich102 <- c(1:101)
pom <- 0
pom2 <- 0

for(j in seq(60,1260,12))
{
  for(i in 1:1000)
  {
    r_normalny <- rnorm(j,-3,8)
    wektor_grup <- c(rep(1,start+pom),rep(2,start+pom+roznica+pom),rep(3,start+pom+roznica+roznica+pom+pom))
    dane <- data.frame(grupa=wektor_grup,obserwacja=r_normalny)
    pv[i] <-  kruskal.test(obserwacja ~ grupa, data = dane)$p.value
  }
  pom <- pom + 2
  pom2 <- pom2 + 1
  wektor_srednich102[pom2] <- mean(pv<0.1)
}
l_danych10 <- seq(60,1260,12)
plot(l_danych10, wektor_srednich102, type="l",main="Kruskal-Wallis - poziom istotnosci 0.1", ylab = "Osetek odrzucen H0", xlab="Liczba danych w calym zbiorze")

#Wnioski - zgodnie z przypuszczeniami odsetek odrzucen prawdziwej hipotezy H0 przy zmianie poziomu istotnosci na 0.1 utrzymuje sie wlasnie na tym poziomie (+/- 0.05)
#(Te same wnioski co przy Anovie)


#3. Porównanie Anovy i testu Kruskala-Wallisa
#Do porownania wykorzystamy te symulacje w ktorej  zwiekszalismy nie tylko liczebnosc podgrup ale rowniez roznice pomiedzy liczebnosciami poszczegolnych podgrup
#Przyjety poziom istotnosci - 0.05
plot(l_danych10, wektor_srednich5, type="l", col="blue",main="Porownanie Anovy i testu Kruskala-Wallisa", ylab = "Odsetek odrzucen H0", xlab="Liczba danych w calym zbiorze")
lines(l_danych10, wektor_srednich10, type="l", col="red")
legend("topleft",inset=0.02,legend=c("Anova","Kruskal-Wallis"), col=c("blue", "red"), pch=c(15,15), cex=0.60)

#Wnioski - Nie ma istotnej roznicy miedzy Anova, a testem Kruskala-Wallisa jesli chodzi o odsetek odrzucen prawdziwej Hipotezy H0.
