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
podatki_euribor_preurejeni <- podatki_euribor
podatki_euribor_preurejeni <- data.frame(t(podatki_euribor_preurejeni))
podatki_shranjeni <- podatki_euribor_preurejeni #Shranim si podatke za tretju nalogo
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
## Po rahlem padcu v začetku leta 2008 opazimo naraščanje obrestne mere, katera v začetku narašča tudi glede na dospetje. Vendar to le za dospetja do treh mesecev,
## kar prikazuje slabo mnenje o prihodnji finančni situaciji. Euribor tako narašča, kar vodi do rekordne vrednosti oktobra 2008, kjer narašča sorazmerno z dospetjem.
## Po velikem padcu doseže euribor z letom 2009 skoraj polovično vrednost svoje največje vrednosti (oktober 2008). Tudi v tem času euribor narašča glede na dospetje,
## največji poskok pa je opaziti v časih dospetja do dveh let.



#TRETJA NALOGA
#a) T = 6, U = 12
napoved <- c(0)
podatki_tretja_naloga <- cbind(podatki_shranjeni, napoved) #Ustvarimo novo tabelo, kjer bomo v stolpec "napoved" shranjevali izračunane napovedi

i <- 1
while i < 16 {
  
  
}
