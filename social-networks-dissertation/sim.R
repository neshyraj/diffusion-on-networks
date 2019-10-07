#Function to simulate data
#Inputs same as before except:

#k: number of preferences
sim_data <- function(a,b,n,k,p){
  #Random w draw from prior
  w_sim <- matrix(rgamma(n*p,a,b), nrow = n, ncol = p)
  #Calculate full lambda_ij
  lambda_sim <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n){
    for (j in 1:n){
      if (i != j){
        sum_c <- 0
        for (c in 1:p){
          sum_c <- sum_c + w_sim[i,c]*w_sim[j,c]
        }
        lambda_sim[i,j] <- sum_c
      }
      else {
        lambda_sim[i,j] <- 0
      }
    }
  }
  #Calculate full Y_ij
  y_sim <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n){
    for (j in 1:n){
      if (i != j){
        y_sim[i,j] <- rexp(1,lambda_sim[i,j]) 
      }
      else {
        y_sim[i,j] <- 0
      }
    }
  }
  #Find top k rankings from Y
  index <- 1:n
  y_sim <- rbind(index,y_sim)
  rank_sim <- matrix(0, nrow = n, ncol = k)
  for (i in 1:n){
    for (r in 2:(k+1)){
      rank_sim[i,r-1] <- y_sim[1,][y_sim[i+1,] == sort(y_sim[i+1,])[r]]
    }
  }
  #Return list of similated w and associated ranked preferences
  return(list(w_sim,rank_sim))
}
