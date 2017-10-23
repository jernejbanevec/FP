#uvozimo podatke

podatki_2008 <- read.csv2(file = "prve-vaje/podatki/euribor_2008.csv")
podatki_2009 <- read.csv2(file = "prve-vaje/podatki/euribor_2009.csv")
podatki_2010 <- read.csv(file = "prve-vaje/podatki/euribor_2010.csv")

#obrežemo podatke in jih transponiramo, pri tem nažalost nimamo več data frama vendar matriko

podatki_2008 <- podatki_2008[1:15,] %>% select(X, X2.01.2008, X1.02.2008, X3.03.2008, X1.04.2008, X2.05.2008, X2.06.2008, X1.07.2008, X1.08.2008, X1.09.2008, X1.10.2008, X3.11.2008, X1.12.2008) %>% t()
podatki_2009 <- podatki_2009[1:15,] %>% select(X, X2.01.2009, X2.02.2009, X2.03.2009, X1.04.2009, X4.05.2009, X1.06.2009, X1.07.2009, X3.08.2009, X1.09.2009, X1.10.2009, X2.11.2009, X1.12.2009) %>% t()
podatki_2010 <- podatki_2010[1:15,] %>% select(X, X04.01.2010, X01.02.2010, X01.03.2010, X01.04.2010, X03.05.2010, X01.06.2010, X01.07.2010, X02.08.2010, X01.09.2010, X01.10.2010, X01.11.2010, X01.12.2010) %>% t()

#Podatke nato pretvorimo nazaj v data frame za lažjo obdelavo

podatki_2008 <- data.frame(podatki_2008)[-1,]
podatki_2009 <- data.frame(podatki_2009)[-1,]
podatki_2010 <- data.frame(podatki_2010)[-1,]

colnames(podatki_2008) <- c('1w', '2w', '3w', '1m', '2m', '3m', '4m', '5m', '6m', '7m', '8m', '9m', '10m', '11m', '12m')
colnames(podatki_2009) <- c('1w', '2w', '3w', '1m', '2m', '3m', '4m', '5m', '6m', '7m', '8m', '9m', '10m', '11m', '12m')
colnames(podatki_2010) <- c('1w', '2w', '3w', '1m', '2m', '3m', '4m', '5m', '6m', '7m', '8m', '9m', '10m', '11m', '12m')