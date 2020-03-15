res <- list()
for (k in 1:10){

#Starting over from scratch.  
#reproduction number 2.28?
#Start with N people
N <- 330000000
#Number of days to simulate
TTT <- 150

#Simulate number of new infections.  
#Take those new infections and put them into buckets of death, hospitalization, or regular infected.  
#Then use an exponential distribution to model length of infection time.  
# Then move people to recovered  


library(Matrix)
#1 of them is infected
S <- I <- R <- M <- rep(NA,TTT)
H_mat <- I_mat <- R_mat <- matrix(0,nrow = TTT, ncol = TTT)

H_mat <- as(H_mat, "sparseMatrix") 
I_mat <- as(I_mat, "sparseMatrix") 
R_mat <- as(R_mat, "sparseMatrix") 

#Let's make a matrix! 
#rows are like day infected and columns are if you are still infects.  

S[1] <- N; I[1] <- 1; R[1] <- 0
#H_mat <- I_mat <- R_mat <- matrix(0,nrow = TTT, ncol = TTT)

#new infections
I_mat[1,1] <- I[1]

#new deaths
M[1] <- rbinom(1,I_mat[1,1],0.02)
#new hospitalizations
H_mat[1,1] <- rbinom(1,I_mat[1,1] - M[1],0.1)


#Ok now everything is initialized.  Now update stuff.  
S[2] <- S[1] - I_mat[1,1]
recovery_time <- round(rexp(I_mat[1,1],1/14))
recovery_time[recovery_time >= TTT] <- TTT 
#Recover time for everyone
I_mat[1,] <- c(rep(0,1-1),(I_mat[1,1] - cumsum(table(factor(recovery_time, levels = 1:TTT)))[1:(TTT-1+1)]))
R_mat[1,] <- c(rep(0,1-1),(cumsum(table(factor(recovery_time, levels = 1:TTT)))[1:(TTT-1+1)]))


for (i in 2:TTT){print(i)
#new infections
  beta <- .16
I_mat[i,i] <- max(1,rbinom(1,S[i],beta*I[i-1]/N))

#new deaths
M[i] <- rbinom(1,I_mat[i,i],0.02)
#new hospitalizations
H_mat[i,i] <- rbinom(1,I_mat[i,i] - M[i],0.1)


#Ok now everything is initialized.  Now update stuff.  

S[i+1] <- S[i] - I_mat[i,i]
recovery_time <- round(rexp(I_mat[i,i],1/14))
recovery_time[recovery_time >= TTT] <- TTT 
#Recover time for everyone
I_mat[i,] <- c(rep(0,i-1),(I_mat[i,i] - cumsum(table(factor(recovery_time, levels = 1:TTT)))[1:(TTT-i+1)]))
R_mat[i,] <- c(rep(0,i-1),(cumsum(table(factor(recovery_time, levels = 1:TTT)))[1:(TTT-i+1)]))

I[i] <- sum(I_mat[,i])

R[i] <- sum(R_mat[,i])

}

res[[k]] <- list(S, I , R)
}


plot(as.Date("2020-01-21") + 1:TTT,res[[1]][[2]], type = "l")
for(k in 2:10){
points(as.Date("2020-01-21") + 1:TTT,res[[k]][[2]], type = "l")
}
