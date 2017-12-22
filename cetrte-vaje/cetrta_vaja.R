## ČETRTA DOMAČA NALOGA

# PRVA NALOGA

#podatke uvozim iz knjižnice Quandl (izberem ceno po mesecih)
#a)
podatki <- Quandl("LBMA/GOLD", collapse="monthly", start_date="2012-12-07") %>% select(c(1, 6))
podatki <- podatki[c(FALSE, rep(TRUE, 60)),]
podatki <- podatki[c(60:1),] #podatke obrnem, da nimam problemov potem s časovno vrsto

#b)
casovna_vrsta <- ts(data = podatki[,2], start = c(2012, 12), frequency = 12)  #uporabim le drugi stolpec
graf_gold <- ts.plot(casovna_vrsta, 
                              xlab='Čas',
                              ylab ="Cena zlata v eurih", 
                              main = "Zlato",
                              col = c("gold"), 
                              lwd = 2)


#DRUGA NALOGA

#a) 

G <- function(vrsta, k){
  
  glajeni <- c()
  for (i in c(1: (length(vrsta) - k))){ 
    glajeni[i] <- sum(vrsta[i : (k+i-1)]) / k
  }
  
  zacetno_leto <- ceiling(2012 + k/12)
  zacetni_mesec <- k %% 12
  
  glajena_vrsta <- ts(glajeni, start = c(zacetno_leto, zacetni_mesec), frequency = 12)
  
  return(glajena_vrsta)
}


#b) 

glajena_7 <- G(casovna_vrsta, 7)

# Napoved za naslednji mesec

napoved <- function(vrsta, k){
  
  dolzina <- length(vrsta)
  napoved <- (sum(casovna_vrsta[(dolzina - k + 1): dolzina])) / k
  
  return(napoved)
}
#c) 

graf_glajen_gold <- ts.plot(casovna_vrsta,
                            glajena_7,
                            xlab='Čas',
                            ylab ="Cena zlata v eurih", 
                            main = "Drseče povprečje",
                            col = c("gold", "red"), 
                            lwd = 2)


#d)

MSE <- function(vrsta, glajena, k){
  dolzina <- length(vrsta)
  napaka <- 0
  for (i in c((k+1) : dolzina)){
    napaka <- napaka + (vrsta[i] - glajena[i-k]) ** 2
  }
  return (napaka / (dolzina - k))
}

#e

#Glajenja
glajena_14 <- G(casovna_vrsta, 14)
glajena_30 <- G(casovna_vrsta, 30)

#Napovedi
napoved_7 <- napoved(casovna_vrsta, 7)
napoved_14 <- napoved(casovna_vrsta, 14)
napoved_30 <- napoved(casovna_vrsta, 30)

#Srednje kvadratične napake
skn_7 <- MSE(casovna_vrsta, glajena_7, 7)
skn_14 <- MSE(casovna_vrsta, glajena_14, 14)
skn_30 <- MSE(casovna_vrsta, glajena_30, 30)

par(mfrow = c(2,2))

graf_glajen_gold <- ts.plot(casovna_vrsta,
                            glajena_7,
                            xlab='Čas',
                            ylab ="Cena zlata v eurih", 
                            main = "Drseče povprečje 7",
                            col = c("gold", "red"), 
                            lwd = 2)

graf_glajen_gold <- ts.plot(casovna_vrsta,
                            glajena_14,
                            xlab='Čas',
                            ylab ="Cena zlata v eurih", 
                            main = "Drseče povprečje 14",
                            col = c("gold", "red"), 
                            lwd = 2)

graf_glajen_gold <- ts.plot(casovna_vrsta,
                            glajena_30,
                            xlab='Čas',
                            ylab ="Cena zlata v eurih", 
                            main = "Drseče povprečje 30",
                            col = c("gold", "red"), 
                            lwd = 2)

## TRETJA NALOGA

#a)

EG <- function(vrsta, alpha){
  
  dolzina <- length(vrsta) 
  eksp_glajena <- c()
  eksp_glajena[1] <- vrsta[1]
  for (i in c(2:dolzina)){
    eksp_glajena[i] <- alpha * vrsta[i] + (1 - alpha) * eksp_glajena[i-1]
  }
  eksp_glajena_vrsta <- ts(eksp_glajena, start = c(2013,1), frequency = 12)
  return(eksp_glajena_vrsta)
}

#b)

par(mfrow = c(1,1)) # Samo da mi ne bo več stavilo k prejšnjim trem grafom
alpha_1 <- 0.60
eksp_glajena <- EG(casovna_vrsta, alpha_1)
eksp_napoved <- eksp_glajena[60]
graf_gold <- ts.plot(casovna_vrsta,
                     eksp_glajena,
                     xlab='Čas',
                     ylab ="Cena zlata v eurih", 
                     main = "Eksponentno glajenje",
                     col = c("gold", "red"), 
                     lwd = 2)


#c)

izracun_alpha <- function(vrsta, alpha){
  dolzina <- length(vrsta)
  glajena <- EG(vrsta, alpha)
  napaka <- 0
  for (i in c(2 : (dolzina - 1))){
    napaka <- napaka + (vrsta[i] - glajena[i]) ** 2
  }
  return (napaka / (dolzina - 1))
  
}

optimalen_alpha <- optimize(izracun_alpha, c(0,1), vrsta = casovna_vrsta)
optimalen_alpha <- optimalen_alpha$minimum

#d)

eksp_opt_glajena <- EG(casovna_vrsta, optimalen_alpha)
eksp_opt_napoved <- eksp_opt_glajena[60]
graf_gold <- ts.plot(casovna_vrsta,
                     eksp_opt_glajena,
                     xlab='Čas',
                     ylab ="Cena zlata v eurih", 
                     main = "Eksponentno glajenje minimalna napaka",
                     col = c("gold", "lawngreen"), 
                     lwd = 2)
