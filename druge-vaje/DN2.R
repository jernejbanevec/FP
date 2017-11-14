### DRUGA DOMAČA NALOGA

#Uvozimo podatke

podatki <- read.table(file = "druge-vaje/podatki/vzorec3.txt")
colnames(podatki) <- "meritev"

##PRVA NALOGA
#a)

histogram <- hist(podatki$meritev, 
                  breaks = 50,
                  main = "Histogram odskodnin",
                  xlab = "Visina odskodnine",
                  ylab = "Pogostost",
                  col = "brown")


#b)

parametri <- mde(podatki$meritev, ppareto1, start = list(shape = 1, min = 1), measure = "CvM");
shape <- parametri$estimate[1];
min <- parametri$estimate[2];

#c)

histogram <- hist(podatki$meritev, 
                  breaks = 50,
                  main = "Histogram odskodnin",
                  xlab = "Visina odskodnine",
                  ylab = "Verjetnost",
                  col = "brown",
                  probability = TRUE)
curve(dpareto1(x, shape = shape, min = min), from = 0, to = 40, add = TRUE, col = "blue", lwd = 2)
legend('topright', "Paretova porazdelitev", lty = 1, col = "blue", lwd = 3)

#Primerjava vzorčne in teoretične porazdelitvene funkcije

plot(ecdf(podatki$meritev), 
     ylim = c(min(0), max(1)), 
     main = "Porazdelitvena funkcija odskodnin", 
     ylab = "Porazdelitvena funkcija",
     xlab = "Visina odskodnine")
curve(ppareto1(x, shape = shape, min = min), 
      from = 0, 
      to = 40, 
      add = TRUE, 
      col = "blue", 
      lwd = 2)


#d)

#Waldove identitete
#
#E(S) = E(E(S|N)) = E(N*E(Y)) = E(Y)*E(N), N stevilo odskodninskih zneskov
#E(S|N=k) = E(sum|N=k) = E(sum) = sum(E(Yi)) = kE(Y)
#Var(S) = Var(Y)*E(N) + E(Y)^2*Var(N)

alfa <- shape
x_m<- min #Predstavlja parameter merila

E_Y <- (alfa * x_m) / (alfa - 1)
Var_Y <- Inf #Vrednost je neskončno, saj je alfa v našem primeru < 2

E_N <- 15 #Upanje poisonove z lambda = 15
E_S <- E_Y * E_N
Var_S <- Inf #Saj je v vsoti tudi Var_Y pomnožena z nečim pozitivnim

##DRUGA NALOGA
#a)

h <- 0.5
n <- 80  #število korakov

dis_pareto <- diffinv(discretize(ppareto1(x, alfa, x_m),
                         from = 0,
                         to = 40,
                         step = h))

#diffinf nam da porazdelitveno funkcijo

#b)
stopnice <- stepfun(seq(0, 39.5, by = h), 
                    dis_pareto)

plot(stopnice,
     col = "orange",
     do.points = FALSE,
     main = "Paretova porazdelitev",
     xlab = "x",
     ylab = "Porazdelitvena funkcija",
     lwd = 2)
curve(ppareto1(x, alfa, x_m),
      from = 0,
      to = 45,
      add = TRUE)

#c)

porazdelitvena <- aggregateDist(method = "recursive", 
                                model.freq = "poisson", 
                                model.sev = diff(dis_pareto), #diff nam da višino skoka (boljše predstavlja verjetnost)
                                lambda = 15,
                                p0 = NULL,
                                x.scale = h)
plot(porazdelitvena) #Nariše graf - NAROBE!!!

#d)

#tu je S diskretna slučajna spremenljivka
vrednosti1 <- knots(stopnice)
vrednosti <- 0.5 * (2 * vrednosti + 1)
verjetnosti <- diff(dis_pareto)

upanje <- (vrednosti %*% verjetnosti) * 15 #to je skalarni produkt
razdalja <- vrednosti - E_Y #to je uredu zaradi krožnega dopolnjevanja
varianca <- (razdalja * razdalja) %*% verjetnosti

#e)

odst_95 <- VaR(porazdelitvena)[2]
