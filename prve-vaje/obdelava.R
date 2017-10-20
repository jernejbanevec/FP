#uvozimo podatke

podatki_2008 <- read.csv2(file = "prve-vaje/podatki/euribor_2008.csv")
podatki_2009 <- read.csv2(file = "prve-vaje/podatki/euribor_2009.csv")
podatki_2010 <- read.csv(file = "prve-vaje/podatki/euribor_2010.csv")

#obrežemo podatke

podatki_2008 <- podatki_2008[1:15,]
podatki_2009 <- podatki_2009[1:15,]
podatki_2010 <- podatki_2010[1:15,]

#obrnemo podatke

rownames(podatki_2008) <- podatki_2008$X
podatki_2008$X <- NULL
podatki_2008 <- podatki_2008 %>% subset(select=c(X2.01.2008, X1.02.2008, X))
podatki_2008 <- t(podatki_2008)

rownames(podatki_2009) <- podatki_2009$X
podatki_2009$X <- NULL
podatki_2009 <- t(podatki_2009)

rownames(podatki_2010) <- podatki_2010$X
podatki_2010$X <- NULL
podatki_2010 <- t(podatki_2010)