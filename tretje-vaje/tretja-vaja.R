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
    return(max(abs(min(vrsta[(T+1):length(vrsta)]) - min(vrsta[1:T])), 0))
  } else {return("Pazi na vpis tipa!")}
}

## DRUGA NALOGA

#a) 

binomski <- function(S0,u,d,U,R,T,type){
  
  q <- (1+R-d)/(u-d)
  razpleti <- hcube(c(rep(2, U)),translation = -1)
  verjetnosti_razpleti <- q**(1-razpleti)*(1-q)**razpleti
  razpredelnica <- data.frame(rep(0, 2**U))
  colnames(razpredelnica) <- c("Verjetnosti")
  #verjetnosti <- q ** rowSums(razpleti) * (1-q)
}