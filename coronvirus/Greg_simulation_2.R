res <- list()
  for (k in 1:10){
  #Starting over from scratch.  
  #reproduction number 2.28?
  #Start with N people
  N <- 330000000
  #Number of days to simulate
  TTT <- 365
  
  #Simulate number of new infections.  
  #Take those new infections and put them into buckets of death, hospitalization, or regular infected.  
  #Then use an exponential distribution to model length of infection time.  
  # Then move people to recovered  
  
  
  library(Matrix)
  #1 of them is infected
  S <- I <- R <- M <- rep(NA,TTT)
  
  
  S[1] <- N - 1; I[1] <- 1; R[1] <- 0
  
  R0 <- 2.28
  gamma <- 1/14
  beta <- R0*gamma
#  beta/gamma = R0
  
  for (i in 2:TTT){
    
  n_new_I <- max(1,rbinom(1,S[i-1],beta*I[i-1]/N))
  n_new_R <- rbinom(1,I[i-1],gamma)
  
  S[i] <- S[i-1] - n_new_I
  I[i] <- I[i-1] + n_new_I - n_new_R 
  R[i] <- R[i-1] + n_new_R 
  
  }
  
  res[[k]] <- list(S=S, I=I, R=R)
  
  }
  
  plot(as.Date("2020-01-21") + 1:TTT,S, type = "l", col = "blue", ylim = c(0,N))
  points(as.Date("2020-01-21") + 1:TTT,R, type = "l", col = "darkgreen")
  points(as.Date("2020-01-21") + 1:TTT,I, type = "l", col = "red")
  
  for (k in 1:10){
  points(as.Date("2020-01-21") + 1:TTT,res[[k]]$S, type = "l", col = "blue", ylim = c(0,N))
  points(as.Date("2020-01-21") + 1:TTT,res[[k]]$R, type = "l", col = "darkgreen")
  points(as.Date("2020-01-21") + 1:TTT,res[[k]]$I, type = "l", col = "red")

  }