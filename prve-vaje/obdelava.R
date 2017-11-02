# PRVA NALOGA

#uvozimo podatke

podatki_2008 <- read.csv2(file = "prve-vaje/podatki/euribor_2008.csv")
podatki_2009 <- read.csv2(file = "prve-vaje/podatki/euribor_2009.csv")
podatki_2010 <- read.csv(file = "prve-vaje/podatki/euribor_2010.csv")

#obrežemo podatke in jih transponiramo, pri tem nažalost nimamo več data frama vendar matriko

podatki_2008 <- podatki_2008[1:15,] %>% select(X, X2.01.2008, X1.02.2008, X3.03.2008, X1.04.2008, X2.05.2008, X2.06.2008, X1.07.2008, X1.08.2008, X1.09.2008, X1.10.2008, X3.11.2008, X1.12.2008) %>% t()
podatki_2009 <- podatki_2009[1:15,] %>% select(X, X2.01.2009, X2.02.2009, X2.03.2009, X1.04.2009, X4.05.2009, X1.06.2009, X1.07.2009, X3.08.2009, X1.09.2009, X1.10.2009, X2.11.2009, X1.12.2009) 
podatki_2010 <- podatki_2010[1:15,] %>% select(X, X04.01.2010, X01.02.2010, X01.03.2010, X01.04.2010, X03.05.2010, X01.06.2010, X01.07.2010, X02.08.2010, X01.09.2010, X01.10.2010, X01.11.2010, X01.12.2010) %>% t()


#Podatke nato pretvorimo nazaj v data frame za lažjo obdelavo

podatki_2008 <- data.frame(podatki_2008)[-1,]
podatki_2010 <- data.frame(podatki_2010)[-1,]

#Popravimo napako pri vnosu podatkov za maj 2009
podatki_2009$X4.05.2009 <- parse_number(podatki_2009$X4.05.2009)
podatki_2009$X4.05.2009 <- podatki_2009$X4.05.2009 / 1000
podatki_2009 <- t(podatki_2009)
podatki_2009 <- data.frame(podatki_2009)[-1,]                  

colnames(podatki_2008) <- c('1w', '2w', '3w', '1m', '2m', '3m', '4m', '5m', '6m', '7m', '8m', '9m', '10m', '11m', '12m')
colnames(podatki_2009) <- c('1w', '2w', '3w', '1m', '2m', '3m', '4m', '5m', '6m', '7m', '8m', '9m', '10m', '11m', '12m')
colnames(podatki_2010) <- c('1w', '2w', '3w', '1m', '2m', '3m', '4m', '5m', '6m', '7m', '8m', '9m', '10m', '11m', '12m')

#Združimo podatke z rbind in narišemo graf(T = 6, U = 12)

podatki_euribor <- rbind(podatki_2008, podatki_2009, podatki_2010)


podatki_euribor$`1w` <- parse_number(podatki_euribor$`1w`)
podatki_euribor$`2w` <- parse_number(podatki_euribor$`2w`)
podatki_euribor$`3w` <- parse_number(podatki_euribor$`3w`)
podatki_euribor$`1m` <- parse_number(podatki_euribor$`1m`)
podatki_euribor$`2m` <- parse_number(podatki_euribor$`2m`)
podatki_euribor$`3m` <- parse_number(podatki_euribor$`3m`)
podatki_euribor$`4m` <- parse_number(podatki_euribor$`4m`)
podatki_euribor$`5m` <- parse_number(podatki_euribor$`5m`)
podatki_euribor$`6m` <- parse_number(podatki_euribor$`6m`)
podatki_euribor$`7m` <- parse_number(podatki_euribor$`7m`)
podatki_euribor$`8m` <- parse_number(podatki_euribor$`8m`)
podatki_euribor$`9m` <- parse_number(podatki_euribor$`9m`)
podatki_euribor$`10m` <- parse_number(podatki_euribor$`10m`)
podatki_euribor$`11m` <- parse_number(podatki_euribor$`11m`)
podatki_euribor$`12m` <- parse_number(podatki_euribor$`12m`)


imena = c("6 - mesecna", "12 - mesecna")
casovna_vrsta1 <- ts(podatki_euribor$`6m`, start= c(2008, 1), frequency = 12)
casovna_vrsta2 <- ts(podatki_euribor$`12m`, start= c(2008, 1), frequency = 12)
graf_obrestnih_mer <- ts.plot(casovna_vrsta1, 
                              casovna_vrsta2, 
                              xlab='Leto', ylab ="Obrestna mera v odstotkih", 
                              main = "6-mesecna in 12-mesecna obrestna mera",
                              col = c("blue","red"), 
                              lwd = 2)
legend('topright', imena, lty = 1, col = c("blue","red"), lwd = 3)


# DRUGA NALOGA
podatki_shranjeni <- podatki_euribor #Shranim si podatke za tretjo nalogo
podatki_euribor_preurejeni <- podatki_euribor
podatki_euribor_preurejeni <- data.frame(t(podatki_euribor_preurejeni))

# Zanimivi datumi so po mojem mnenju 1.2.2008, 1.10.2008 in 2.1.2009
podatki_euribor_preurejeni <- podatki_euribor_preurejeni[c(2, 10, 13)]
tedni_v_mesecih <- c(0.23, 0.47, 0.73, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
novi_podatki <- cbind(tedni_v_mesecih, podatki_euribor_preurejeni)
imena2 = c("1.2.2008", "1.10.2008", "2.1.2009")
graf_zanimiv <- plot(y = novi_podatki[,c(2)], 
                     x = tedni_v_mesecih, 
                     type = "o", 
                     main = "Struktura euribora",
                     ylim = c(min(0), max(6)),
                     xlab = "Čas dospetja", ylab = "Obrestna mera glede na dospetje")
lines(novi_podatki$tedni_v_mesecih, col = "green", novi_podatki$X1.02.2008, type = "o", pch = 19, text(10, 4.8,"1.2.2008", col = "green"))
lines(novi_podatki$tedni_v_mesecih, col = "red", novi_podatki$X1.10.2008, type = "o", pch = 19, text(10, 5.8, "1.10.2008", col = "red"))
lines(novi_podatki$tedni_v_mesecih, col = "brown", novi_podatki$X2.01.2009, type = "o", pch = 19, text(10, 3.4, "2.1.2009", col = "brown"))

#tukaj pri pch izberem najlepšo obliko oznak, kar v tem primeru predstavljajo pike ( dobra je tudi pch = 16)


# Utemelitve
## Po rahlem padcu v začetku leta 2008 lahko opazimo naraščanje obrestne mere,
## katera v začetku narašča tudi glede na dospetje. Vendar to velja le za dospetja 
## do treh mesecev, kar prikazuje slabo mnenje o prihodnji finančni situaciji.
## Euribor tako narašča, kar vodi do rekordne vrednosti oktobra 2008,
## kjer narašča sorazmerno z dospetjem. Po velikem padcu doseže euribor
## z letom 2009 skoraj polovično vrednost svoje največje vrednosti (oktober 2008).
## Tudi v tem času euribor narašča glede na dospetje, največji poskok pa je
## opaziti v časih dospetja do dveh let.



#TRETJA NALOGA
#a) T = 6, U = 12

napoved <- c(0)
podatki_tretja_naloga <- cbind(podatki_shranjeni, napoved) #Ustvarimo novo tabelo, kjer bomo v stolpec "napoved" shranjevali izračunane napovedi

i <- 1
while (i < 37) {
  if (i < 7){
    podatki_tretja_naloga[i,16] <- NA
    } else {
    podatki_tretja_naloga[i,16] <- 100 * 
      (1/(1 - 0.5))*
      ((1 + 0.01 * podatki_tretja_naloga[i - 6, 15])/
         (1 + 0.01 * 0.5 * podatki_tretja_naloga[i - 6, 9]) - 1)
    }
  i <- i + 1
}

# Pri računanju sem najprej pomnožil z 0.01, da sem ustvaril realne odstotke,
# ter nato še množil z 100, da sem dobil podatke primerljive z ostalimi podatki
# v tabeli (torej podatke izražene v odstotkih).


#b)

podatki_tretja_naloga <- podatki_tretja_naloga[c(9, 15, 16)]
colnames(podatki_tretja_naloga) <- c("Euribor6m", "Euribor12m", "Napoved6m")

#c)

imena3 <- c("2008", "2009", "2010")

leto_2008 <- podatki_tretja_naloga[1:12,]
leto_2009 <- podatki_tretja_naloga[13:24,]
leto_2010 <- podatki_tretja_naloga[25:36,]

razsevni_graf <- plot(y = podatki_tretja_naloga$Euribor6m,
                      x = podatki_tretja_naloga$Napoved6m,
                      type = "p",
                      main = "6 mesecni Euribor 2008 - 2010",
                      ylim = c(min(0), max(6)),
                      xlim = c(min(0), max(6)),
                      xlab = "Napoved",
                      ylab = "Opazovano")
points(y = leto_2008[,1], x = leto_2008[, 3], type = "p", pch = 16, col = "green")
points(y = leto_2009[,1], x = leto_2009[, 3], type = "p", pch = 16, col = "red")
points(y = leto_2010[,1], x = leto_2010[, 3], type = "p", pch = 16, col = "brown")
abline(a = 0, b = 1, type = "n")   #Simetrala lihih kvadrantov
regresijska_premica <- lm(Euribor6m ~ Napoved6m, data = podatki_tretja_naloga)
abline(regresijska_premica)
legend("topleft", imena3, col = c("green", "red", "brown"), pch = c(19,19,19))


#d)

graf_2008 <- plot(y = leto_2008$Euribor6m,
                  x  = leto_2008$Napoved6m,
                  type = "p",
                  main = "6 mesecni Euribor 2008",
                  ylim = c(min(0), max(6)),
                  xlim = c(min(0), max(6)),
                  xlab = "Napoved",
                  ylab = "Opazovano")
points(y = leto_2008[,1], x = leto_2008[, 3], type = "p", pch = 16, col = "green")
abline(a = 0, b = 1, type = "n")   #Simetrala lihih kvadrantov
regresijska_premica <- lm(Euribor6m ~ Napoved6m, data = leto_2008)
abline(regresijska_premica)


graf_2008 <- plot(y = leto_2009$Euribor6m,
                  x  = leto_2009$Napoved6m,
                  type = "p",
                  main = "6 mesecni Euribor 2009",
                  ylim = c(min(0), max(6)),
                  xlim = c(min(0), max(6)),
                  xlab = "Napoved",
                  ylab = "Opazovano")
points(y = leto_2009[,1], x = leto_2009[, 3], type = "p", pch = 16, col = "red")
abline(a = 0, b = 1, type = "n")   #Simetrala lihih kvadrantov
regresijska_premica <- lm(Euribor6m ~ Napoved6m, data = leto_2009)
abline(regresijska_premica)


graf_2010 <- plot(y = leto_2010$Euribor6m,
                  x  = leto_2010$Napoved6m,
                  type = "p",
                  main = "6 mesecni Euribor 2010",
                  ylim = c(min(0), max(6)),
                  xlim = c(min(0), max(6)),
                  xlab = "Napoved",
                  ylab = "Opazovano")
points(y = leto_2010[,1], x = leto_2010[, 3], type = "p", pch = 16, col = "brown")
abline(a = 0, b = 1, type = "n")   #Simetrala lihih kvadrantov
regresijska_premica <- lm(Euribor6m ~ Napoved6m, data = leto_2010)
abline(regresijska_premica)

#e)

## Grafikoni v točkah c) in d) bi, v primeru da hipoteza velja, morali izgledati tako, 
## da bi bile meritve raztresene okoli simetrale lihih kvadrantov. Torej da bi se tudi 
## regresijska premica praktično skoraj prilegala simetrali lihih kvadrantov. Če bi pa 
## zahtevali da hipoteza resnično drži, bi morale biti vse meritve točno na simetrali. 
## Pri mojih podatkih so bili podatki še najbližje temud a hipoteza velja v letu 2009, 
## sicer so bila odstopanja resnično velika.
## Prav tako sem opazil tudi skoraj na simetralo pravokotno regresijsko premico. Velja,
## da je leto 2008 leto hude gospodarske krize. Tako lahko opazimo, da so bile napovedi
## za čas krize bolj optimistične, toraj da krize ni bilo ravno pričakovati.
## vsaj s tega vidika ne.