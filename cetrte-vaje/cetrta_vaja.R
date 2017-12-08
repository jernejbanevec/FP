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
  for (i in c((k): (length(vrsta) - 1))){ 
    glajeni[i - k + 1] <- (sum(vrsta[(i-k+1) : i]) / k)
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

MSE <- function(vrsta, k){
  T <- length(vrsta)
  napaka <- (1 / (T-k)) * ((vrsta - G(vrsta, k))[k:(T-1)] %*% (vrsta - G(vrsta, k))[k:T-1])
  return (napaka)
}