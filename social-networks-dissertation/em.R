#The below code is to run the EM algorithm
#in equation (2.20)

#Function to calculate E[alpha_ic] term in numerator 
#Inputs are:
#ind: Individual i for w_ic update
#com: Community c for w_ic update
#w1: Matrix of parameters w (must be of class matrix)
#rho: Data where each row is the list for individual i and each #column is the rank of the preference. The entries are the other #individuals. For instance, if row 3 has entries 5,11,7 in 
#columns 1,2,3 respectively, this indicates individual 3 has 
#listen individuals 5,11,7 as their top three friends in that 
#order.
a_ic <- function(ind,com,w1,rho){
  #Get size of k, p and n
  num_k <- sum(rho[ind,] > 0)
  num_c <- ncol(w1)
  n <- nrow(w1)
  #Loop for sum over j an element of rho_i 
  sum_a1 <- 0
  for (j in 1:n){
    if (j %in% rho[ind,]){
      sum_c1 <- 0
      for (c in 1:num_c){
        sum_c1 <- sum_c1 + w1[ind,c]*w1[j,c]
      }
      #Full sum
      sum_a1 <- sum_a1 + (w1[ind,com]*w1[j,com])/sum_c1
    }
    else {
      next
    }
  }
  #Loop for sum over j such that i is an element of rho_j
  sum_a2 <- 0
  for (j in 1:n){
    if (ind %in% rho[j,]){
      sum_c2 <- 0
      for (c in 1:num_c){
        sum_c2 <- sum_c2 + w1[ind,c]*w1[j,c]
      }
      #Full sum
      sum_a2 <- sum_a2 + (w1[ind,com]*w1[j,com])/sum_c2
    }
    else {
      next
    }
  }
  return(sum_a1 + sum_a2)
}

#Function to calculate E[beta_ic] term in denominator 
#Inputs same as E[alpha_ic] except:
#w2: Matrix of parameters to update simultaneously, as explained
#after equation (2.20)
b_ic <- function(ind,com,w1,w2,rho){
  num_k <- sum(rho[ind,] > 0)
  num_c <- ncol(w1)
  n <- nrow(w1)
  #Loops for first argument in sum from m=1 to k
  sum_b1 <- 0
  for (m in 1:num_k){
    if (m == 1){
      rho_im <- NULL
    }
    else {
      rho_im <- rho[ind,1:(m-1)]
    }
    sum_num1 <- 0
    sum_den1 <- 0
    for (j in 1:n){
      if (j != ind && !j %in% rho_im){
        sum_num1 <- sum_num1 + w2[j,com]
        sum_c1 <- 0
        for (c in 1:num_c){
          sum_c1 <- sum_c1 + w1[ind,c]*w1[j,c]
        }
        sum_den1 <- sum_den1 + sum_c1
      }
      else {
        next
      }
    }
    #Full sum
    sum_b1 <- sum_b1 + (sum_num1/sum_den1)
  }
  #Loops for second argument in sum from m=1 to k
  sum_b2 <- 0
  for (m in 1:num_k){
    sum_j <- 0
    for (j in 1:n){
      if (m == 1){
        rho_jm <- NULL
      }
      else {
        rho_jm <- rho[j,1:(m-1)]
      }
      if (j != ind){
        if (!ind %in% rho_jm){
          num2 <- w2[j,com]
        }
        else {
          num2 <- 0
        }
        sum_den2 <- 0
        for (r in 1:n){
          if (r != j && !r %in% rho_jm){
            sum_c2 <- 0
            for (c in 1:num_c){
              sum_c2 <- sum_c2 + w1[j,c]*w1[r,c]
            }
            sum_den2 <- sum_den2 + sum_c2
          }
          else {
            next
          }
        }
        sum_j <- sum_j + (num2/sum_den2)
      }
      else {
        next
      }
    }
    #Full sum
    sum_b2 <- sum_b2 + sum_j
  }
  return(sum_b1 + sum_b2)
}

#Function to update one w_ic with EM
#Inputs same as before, but with a and b for hyperparameters
em_ic <- function(ind,com,a,b,w1,w2,rho){
  num_update <- a - 1 + a_ic(ind,com,w1,rho)
  den_update <- b + b_ic(ind,com,w1,w2,rho)
  return(num_update/den_update)
}

#Function to calculate the log-likelihood
#Inputs same as before
likelihood <- function(w2,rho){
  n <- nrow(w2)
  num_c <- ncol(w2)
  lambda <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n){
    for (j in 1:n){
      if (i != j){
        sum_c <- 0
        for (c in 1:num_c){
          sum_c <- sum_c + w2[i,c]*w2[j,c] 
        }
        lambda[i,j] <- sum_c
      }
      else {
        lambda[i,j] <- 0
      }
    }
  }
  prod_i <- 1
  for (i in 1:n){
    prod_m <- 1
    num_pref <- sum(rho[i,] > 0)
    for (m in 1:num_pref){
      nom <- rho[i,m]
      if (m == 1){
        rho_im <- NULL
      }
      else {
        rho_im <- rho[i,1:(m-1)]
      }
      sum_j <- 0
      for (j in 1:n){
        if (j != i && !j %in% rho_im){
          sum_j <- sum_j + lambda[i,j]
        }
        else {
          next
        }
      }
      prod_m <- prod_m*(lambda[i,nom]/sum_j)
    }
    prod_i <- prod_i*prod_m
  }
  return(log(prod_i))
}

#Function to run EM algorithm
#Inputs same as before except:
#thresh: threshold to stop algorithm
#time: number of iterations to run
run_em <- function(a,b,w1,w2,rho,thresh,time){
  like <- NULL
  for (t in 1:time){
    for (i in 1:nrow(w1)){
      for (c in 1:ncol(w1)){
        w2[i,c] <- em_ic(i,c,a,b,w1,w2,rho)
      }
    }
    if (sum(abs(w1-w2)<thresh,na.rm=TRUE) == length(w1)){
      break
    }
    else {
      w1 <- w2
    }
    like <- append(like,likelihood(w2,rho))
  }
  return(list(w1,like))
}
