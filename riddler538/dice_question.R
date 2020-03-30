#generate die rolls
dices <- function(prob){sample(1:6,6,prob = prob, replace = TRUE)}

num_rolls <- function(){
prob <- rep(1,6)/6
roll <- 1:6
count <- 0
while(!any(prob == 1)){
count <- count + 1
roll <- dices(prob)
prob <- (table(c(roll,1:6))-1)/6
}
return(count)
}

num_rolls()

dist <- replicate(1000,num_rolls())



#generate die rolls
dices <- function(prob,N){sample(1:N,N,prob = prob, replace = TRUE)}

num_rolls <- function(N){
  prob <- rep(1,N)/N
  roll <- 1:N
  count <- 0
  while(!any(prob == 1)){
    count <- count + 1
    roll <- dices(prob,N)
    prob <- (table(c(roll,1:N))-1)/N
  }
  return(count)
}

num_rolls(N=2)

dist <- replicate(5000,num_rolls(N=3))

#EX <- c()
for (n in 30){print(n)
EX[n] <- mean(replicate(1000,num_rolls(N=n)))
}

plot(1:15, EX, type = "l")
points(1:15, EX, pch = 16)
abline(a = 0, b = 1, col = "red")       

x <- 1:15
mod <- lm(EX ~ x)
predict(mod, x = 1:15)




#Im guessing 7 is the expected value!
hist(dist)


#How would we do this in closed form?  
#N = 2
c(1,1)

c(1,2) 
c(2,1) 

c(2,2)


X  = Number of rolls before we stop.  

P(X = 1) = 1/2
P(X = 2) = (1/2)^2

P(X = i) = (1/2)^i

E(X) = sum(i*(1/2)^i)

sum((1:100)*(1/2)^(1:100))

N = 2, E[X] = 2
N = 6, E[X] approx 9???

####N = 3
1,1,1 : 1
2,2,2 : 1
3,3,3 : 1

1,1,2 : 3   (2/3)^3 + (1/3)^3
1,1,3 : 3   (2/3)^3 + (1/3)^3
2,2,1 : 3   (2/3)^3 + (1/3)^3
2,2,3 : 3   (2/3)^3 + (1/3)^3
3,3,1 : 3   (2/3)^3 + (1/3)^3
3,3,2 : 3   (2/3)^3 + (1/3)^3

1,2,3 : 6   3/27

P(X = 1) = 3/27
P(X = 2) = 18/27*(((2/3)^3 + (1/3)^3)) + 6/27*3/27
P(X = 3) = 
  
  
  
  
P(X = 1) = 3/27
P(X = 2) = (1- P(X <=1 ))*(18/24*(((2/3)^3 + (1/3)^3)) + 6/24*3/27)
P(X = 3) = (1 - P(X<=2 ))



#As Markov Chain? 
#For a two sided die
#State is the number of unique numbers on the die
p0 <- c(0,1)
S <- matrix(c(1,0,1/2,1/2),byrow = TRUE, ncol = 2)

p0%*%(S %^% 10)


#Now do it for a 3 sided die: 
p0 <- c(0,0,1)
S <- matrix(c(1,0,0,(2/3)^3 + (1/3)^3,(1- ((2/3)^3 + (1/3)^3)),0,3/27,18/27,6/27),byrow = TRUE, ncol = 3)

ppp <- c()
for (i in 1:25){
ppp[i] <- (p0%*%(S %^% i))[1,1]
}

sum(c(ppp[1],diff(ppp))*1:25)


#Now do it for a 4 sided die: 
p0 <- c(0,0,0,1)
S <- matrix(c(1,0,0,0,Y,X,0,0,X,X,X,0,4*(1/4)^4, X, X, X),byrow = TRUE, ncol = 4)

ppp <- c()
for (i in 1:25){
  ppp[i] <- (p0%*%(S %^% i))[1,1]
}

sum(c(ppp[1],diff(ppp))*1:25)









