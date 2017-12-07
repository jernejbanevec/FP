## PRVA NALOGA

S0 <- 50
u <- 1.05
d <- 0.95
U <- 5
R <- 0.03
T <- 3

#Podatki

delnice <- data.frame( c(50.00, 50.00, 50.00, 50.00, 50.00),
                       c(52.50, 52.50, 47.50, 47.50, 52.50),
                       c(49.88, 55.12, 49.88, 45.12, 49.88),
                       c(52.37, 57.88, 47.38, 47.38, 52.37),
                       c(49.75, 60.78, 45.01, 49.75, 54.99),
                       c(52.24, 63.81, 42.76, 52.24, 57.74))
colnames(delnice) <- c("cas_0","cas_1", "cas_2", "cas_3", "cas_4", "cas_5")


#a)

delnice$izplacilo_X <- c(0,0,0,0,0)
for (i in 1:5){
  delnice$izplacilo_X[i] <- max(max(delnice[i, (T+1):(U+1)]) - max(delnice[i, 1:T]), 0)
}

delnice$izplacilo_Y <- c(0,0,0,0,0)
for (i in 1:5){
  delnice$izplacilo_Y[i] <- max(abs(min(delnice[i, (T+1):(U+1)]) - min(delnice[i, 1:T])), 0)
}

#b)

izplacilo <- function(vrsta, T, type){
  if (type == "call") {
    return(max( max(vrsta[(T+1):length(vrsta)]- max(vrsta[1:T])), 0))
  } else if (type == "put"){
    return(max((min(vrsta[(T+1):length(vrsta)]) - min(vrsta[1:T])), 0))
  } else {return("Pazi na vpis tipa!")}
}

## DRUGA NALOGA

#a) 

binomski <- function(S0,u,d,U,R,T,type){
  
  q <- (1+R-d)/(u-d)
  
  razpleti <- hcube(c(rep(2, U)),translation = -1)                # Hiperkocko preoblikujemo z -1, kjer potem dobimo 1 za up in 0 za down
  dobicek_razpleti <- d ** (1-razpleti) * u ** razpleti           # Dobimo vrednosti koeficienta za S0
  dobicek_razpleti <- t(apply(dobicek_razpleti,1, cumprod))       #funkcijo uporabimo na prejšnjih parametrih (kolikšeno je obrestovanje)
  
  obdobje <- U
  k <- rowSums(razpleti)
  verjetnosti <- q ** k * (1-q) ** (obdobje - k)                  #Verjetnosti razpletov
  
  vrednosti <- cbind(S0, S0 * dobicek_razpleti) 
  izplacila <- apply(vrednosti, 1, function(x) izplacilo(x,T,type)) #uporabimo dano funkcijo iz 1.b
  
  upanje <- (izplacila %*% verjetnosti)
  return( upanje / ((1+R)**U) )
  
}

#b)

monte <- function(S0,u,d,U,R,T,type,N){
  
  q <- (1+R-d)/(u-d)
  empiricno <- matrix(rbinom(N*U, 1, q), N, U) #Naredimo matriko iz nakljucnih 1 in 0 (P(0) = q)
  dobicek_razpleti <- d ** (1-empiricno) * u ** empiricno
  dobicek_razpleti <- t(apply(dobicek_razpleti,1, cumprod)) #funkcijo uporabimo na prejšnjih parametrih (kolikšeno je obrestovanje)
  
  vrednosti <- cbind(S0, S0 * dobicek_razpleti) 
  izplacila <- apply(vrednosti, 1, function(x) izplacilo(x,T,type)) #uporabimo dano funkcijo iz 1.b
  
  upanje <- mean(izplacila)
  return( upanje / (1+R)**U )
  
  
}

#Simuliranje vrednosti
sim1 <- monte(60, 1.05, 0.95, 15, 0.01, 8, "put", 10)
sim2 <- monte(60, 1.05, 0.95, 15, 0.01, 8, "put", 100)
sim3 <- monte(60, 1.05, 0.95, 15, 0.01, 8, "put", 1000)


## TRETJA NALOGA

#a)

N1 <- c() 
N2 <- c()
N3 <- c()

M <- 100 #število ponovitev

for (i in c(1:M)){
  
  N1 <- c(N1, monte(60, 1.05, 0.95, 15, 0.01, 8, "put", 10))
  N2 <- c(N2, monte(60, 1.05, 0.95, 15, 0.01, 8, "put", 100))
  N3 <- c(N3, monte(60, 1.05, 0.95, 15, 0.01, 8, "put", 1000))
  
}

# HISTOGRAMI
# N1



histogram_N1 <- hist(N1,
                     breaks = 20, 
                     main = "Monte Carlo: N = 10",
                     xlab = "Premija",
                     ylab = "Pogostost",
                     col = "yellow")

#legend('topright', "Paretova porazdelitev", lty = 1, col = "blue", lwd = 3)