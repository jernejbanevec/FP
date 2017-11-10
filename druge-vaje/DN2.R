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
alfa <- shape
x_m<- min #Predstavlja parameter merila

E_S <- (alfa * x_m) / (alfa - 1)
Var_S <- Inf #Vrednost je neskončno, saj je alfa v našem primeru < 2

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

plot(stepfun(seq(0, 39.5, by = h), 
             dis_pareto),
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
