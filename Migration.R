#Taking the uniform distribution [0,1] and increasing N

#Building Tau function before simulate



alpha = 0.1
integ <-function(x,P){(P-1)*(x^(P-1))/(1 - (x^(P-1)))}
Tau <- function(w,N){
  if(N==1){
    return(0)
  }
  else{
    return((1- alpha)*(integrate(integ, lower= 0, upper= w, P = N)$value))
  }
}

#Building the selection program

SimulateVCG <- function(W, alpha){
  winner <- which.max(W)
  copy_W <- W
  copy_W[winner] = 0
  transfer <- (1- alpha)*max(copy_W)
  social_welfare = sum(copy_W*alpha) + W[winner] - transfer
  res = c(winner, W[winner], transfer, social_welfare)
  return(res)
}

SimulateRandom <- function(W, alpha){
  winner <- sample(1:length(W),1)
  copy_W <- W
  copy_W[winner] = 0
  social_welfare = sum(copy_W*alpha) + W[winner]
  res = c(winner, W[winner], social_welfare)
  return(res)
}

SimulateWOA <- function(W, alpha){
  agents <- length(W)
  winner <- which.max(W)
  copy_W <- W
  copy_W[winner] = 0
  transfer <- unlist(lapply(W,Tau, N = agents))
  social_welfare = sum(copy_W*alpha) + W[winner] - sum(transfer) 
  res = c(winner, W[winner], transfer[winner], sum(transfer), social_welfare)
  return(res)
}

Simulatescenario <- function(W,alpha){
  VCG <- SimulateVCG(W,alpha)
  Random <- SimulateRandom(W,alpha)
  WOA <- SimulateWOA(W,alpha)
  
  res <- c(VCG[4], Random[3], WOA[5])
  return(res)
}

#simulating with a loop

max_agents = 50
SW_VCG = c()
SW_Random = c()
SW_WOA = c()
for (n in 1:max_agents){
  data = c()
  for (rep in 1:100){
    N = n
    W <- runif(N)
     
    res <- Simulatescenario(W,alpha)
    data <- rbind(data, res)
  }
  print(alpha*sum(W))
  data_mean <- colMeans(data)
  SW_VCG = c(SW_VCG, data_mean[1])
  SW_Random = c(SW_Random, data_mean[2])
  SW_WOA = c(SW_WOA, data_mean[3])
}



plot(1:max_agents,SW_VCG, type = 'b', col="blue", pch="^", ylim = c(-1, 1.4), 
     main= paste("Expected Social Welfare (alpha = ",as.character(alpha),")"),
     ylab = "SW", xlab = "Agents", lty=1)
points(SW_Random, pch="*", col="red", lty=2)
points(SW_WOA, pch="+", col="brown", lty=3)
legend(1,1.4,c("Random","VCG","WOA"), col = c("red","blue","brown"), pch =c("*","^","+") )
