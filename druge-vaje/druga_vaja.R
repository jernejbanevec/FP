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
Var_N <- 15
  
##DRUGA NALOGA
#a)

h <- 0.5
n <- 80  #število korakov

dis_pareto <- discretize(ppareto1(x, alfa, x_m),
                         from = 0,
                         to = 40,
                         step = h)

kumulativna_dis_pareto <- discretize(ppareto1(x, alfa, x_m),
                         from = 0,
                         to = 100000,
                         step = h) 

#tu definiram kumulativno_dis_pareto, 
#ki jo uporabim da dobim boljšo kumulativno porazdelitveno funkcijo s panjarjevim algoritmom

#b)

#diffinf nam da porazdelitveno funkcijo
stopnice <- stepfun(seq(0, 39.5, by = h), 
                    diffinv(dis_pareto))

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
                                model.freq = "pois", 
                                model.sev = kumulativna_dis_pareto, #diff nam da višino skoka (boljše predstavlja verjetnost)
                                lambda = 15,
                                convolve = 0,
                                p0 = exp(-15),
                                x.scale = h,
                                maxit = 1000000,
                                tol = 0.002)
plot(porazdelitvena) #Nariše graf 

#d)

#tu je S diskretna slučajna spremenljivka
vrednosti <- knots(porazdelitvena)
#vrednosti <- 0.5 * (2 * vrednosti1 + 1)
verjetnosti <- diff(porazdelitvena)

upanje_S <- vrednosti %*% verjetnosti #to je skalarni produkt
varianca_S <- (vrednosti * vrednosti) %*% verjetnosti - upanje^2 # Var(S) = E[S^2] - E[S]^2


#e)

odst_995 <- VaR(porazdelitvena, 0.995)
izpad_005 <- CTE(porazdelitvena, 0.005)


## TRETJA NALOGA

#a)
sim_N <- rpois(10000, 15)

sim_S <- c()
for (i in sim_N){
  sim_S <- c(sim_S, sum(rpareto1(i, alfa, x_m)))
}

#b)

ocena_upanje <- mean(sim_S)
ocena_varianca <- var(sim_S)
  
# V primerjavi z nalogo 2.d, dobim tokrat boljše pribljižke tako za upanje s.s S, kot za 
# varianco s.s. S. Tako je zato, ker je vzorec nekoliko večji (10000) kot prej (130).

#c)

#elemente sortiram in poberem 9950-ti največji element -> izpustim torej 99,5% ostalih, ki so manjši
ocena_odst_995 <- sort(sim_S)[9950]

# Najverjetneje malo zgrešen algoritem, saj vrača precej različno vrednost kot tista iz Pajnarjevega algoritma,
# pozneje sem sicer ugovotil, da tega ni v navodilih vendar kljub temu obdržim, če bom mogoče potreboval v kakšni
# poznejši domači nalogi.

ocena_izpad_005 <- 0
for (i in 9951:10000){
  i_ti_element <- sort(sim_S)[i]
  predhodki_i <- sort(sim_S)[i-1]
  ocena_izpad_005 <- ocena_izpad_005 + i_ti_element * ((1 - ecdf(sim_S)(predhodki_i)) - (1 - ecdf(sim_S)(i_ti_element)))
  print(ocena_izpad_005)
}


#d)
plot(porazdelitvena)
plot(ecdf(sim_S),
     col = 'green',
     add = TRUE,
     lwd = 2)
legend('bottomright', 
       legend = c('Panjerjev algoritem', 'Monte Carlo simulacija'),
       col = c('black', 'green'),
       lty = 1:1, 
       bty = "n", #Nam vrne legendo brez obrob kot v zgledu
       lwd = 2)
