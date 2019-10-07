#The below code is to run the MCMC algorithm
#in equations (2.21) to (2.23)

install.packages("stats")
install.packages("coda")
library(stats)
library(coda)

#Function to calculate one Z_im
#Inputs same as before except:

#prefm: preference m for indivudal ind
z_im <- function(ind,prefm,rho,w1){
  #Get size of n and p
  n <- nrow(w1)
  num_c <- ncol(w1)
  #Loops to sum over j
  sum_j <- 0
  if (prefm == 1){
    rho_im <- NULL
  }
  else {
    rho_im <- rho[ind,1:(prefm - 1)]
  }
  for (j in 1:n){
    if (j != ind && !j %in% rho_im){
      sum_c <- 0
      for (c in 1:num_c){
        sum_c <- sum_c + w1[ind,c]*w1[j,c]
      }
      sum_j <- sum_j + sum_c
    }
    else {
      next
    }
  }
  #Return value from exponential distribution with rate sum_j
  return(rexp(1,sum_j))
}

#Function to calculate one V_ij
#Inputs same as before except:

#ind_j: index of j an element of rho_i
#Note V is stored as an n x n matrix, but only k
#entries per row are used at each nominated j 
v_ij <- function(ind,ind_j,rho,w1){
  #Get size of p
  num_c <- ncol(w1)
  #Loop to calculate sum over c for denominator
  sum_den <- 0
  for (c in 1:num_c){
    sum_den <- sum_den + w1[ind,c]*w1[ind_j,c]
  }
  #Calculate all probabilities for discrete distribution
  probs <- list()
  for (c in 1:num_c){
    probs[[c]] <- (w1[ind,c]*w1[ind_j,c])/sum_den
  }
  #Sample integer from 1 to p from distribution with
  #calculated probabilities
  draw <- sample(x = 1:num_c, 1, replace = TRUE, prob = probs[1:num_c])
  return(draw)
}

#Function to calculate one w_ic
#Inputs same as before except:

#z: Matrix z from current iteration. n x k matrix
#v: Matrix v from current iteration. n x n matrix
w_ic <- function(ind,com,a,b,z,v,w2,rho){
  #Get size of p, k and n
  num_c <- ncol(w2)
  num_pref <- sum(rho[ind,] > 0)
  n <- nrow(w2)
  #Loops to calculate full numerator ie rate parameter
  sum_num <- 0
  for (j in 1:n){
    if (j %in% rho[ind,]){
      sum_num <- sum_num + as.numeric(v[ind,j]==com)
    }
    else {
      next
    }
  }
  for (j in 1:n){
    if (ind %in% rho[j,]){
      sum_num <- sum_num + as.numeric(v[j,ind]==com)
    }
    else {
      next
    }
  }
  #Full numerator
  alpha <- a + sum_num
  #Loops to calculate full denominator ie shape parameter
  sum_den1 <- 0
  sum_den2 <- 0
  for (m in 1:num_pref){
    if (m==1){
      rho_im <- NULL
    }
    else {
      rho_im <- rho[ind,1:(m-1)]
    }
    for (j in 1:n){
      if (j != ind && !j %in% rho_im){
        sum_den1 <- sum_den1 + z[ind,m]*w2[j,com]
      }
      else {
        next
      }
    }
    for (j in 1:n){
      if (m==1){
        rho_jm <- NULL
      }
      else {
        rho_jm <- rho[j,1:(m-1)]
      }
      if (!ind %in% rho_jm){
        delta <- 1
      }
      else {
        delta <- 0
      }
      if (j != ind){
        sum_den2 <- sum_den2 + z[j,m]*w2[j,com]*delta
      }
      else {
        next
      }
    }
  }
  #Full denominator
  beta <- b + sum_den1 + sum_den2
  #Return sample from gamma distribution 
  return(rgamma(1,alpha,beta))
}

#Function to run MCMC algorithm
#Inputs same as before
run_mcmc <- function(a,b,z,v,w1,w2,rho,time){
  #Find size of n and p
  n <- nrow(w1)
  num_c <- ncol(w1)
  #Empty list to store each MCMC iteration
  mcmc_list <- list()
  #Sample new z
  for (t in 1:time){
    for (i in 1:n){
      num_pref <- sum(rho[i,]>0)
      for (m in 1:num_pref){
        z[i,m] <- z_im(i,m,rho,w1)
      }
    }
    #Sample new v
    for(i in 1:n){
      for (j in 1:n){
        if (j %in% rho[i,]){
          v[i,j] <- v_ij(i,j,rho,w1)
        }
      }
    }
    #Sample new w
    for (i in 1:n){
      for (c in 1:num_c){
        w2[i,c] <- w_ic(i,c,2,3,z,v,w2,rho)
      }
    }
    w1 <- w2
    #Store run in list indexed by iteration number
    mcmc_list[[t]] <- w1
  }
  return(mcmc_list)
}

#Function to return the point estimates from MCMC by
#calculating the mean of each parameters output
#Inputs are:

#list_mcmc: A list with each item being one timestep
#of the MCMC algorithm output by run_mcmc
#n: Number of individuals in the network
#p: Number of communities
#burn: Number of burn in samples ie first x samples discarded
#max_iter: Total number of iterations. Should be equal to
#number of items/iterations in list object
mcmc_mean <- function(list_mcmc,n,p,burn,max_iter){
  w_mean_mcmc <- matrix(0, nrow = n, ncol = p)
  for (i in 1:n){
    for (c in 1:p){
      w_av <- NULL
      for (t in burn:max_iter){
        w_av <- append(w_av,list_mcmc[[t]][i,c]) 
      }
      w_mean_mcmc[i,c] <- mean(w_av)
    }
  }
  return(w_mean_mcmc)
}

#Function to plot autocorrelation and traceplot
#Input same as before except:

#atd: Type of plot. 1 for autocorrelation, 2 for traceplot
#3 for marginal posterior distribution
atd_plot <- function(lst_mcmc,ind,com,burn,max_iter,atd){
  vect <- NULL
  for (t in burn:max_iter){
    vect <- append(vect,lst_mcmc[[t]][ind,com])
  }
  if (atd == 1){
    acf(vect)
  }
  else if (atd == 2){
    traceplot(as.mcmc(vect)) 
  }
  else {
    plot(density(vect), xlim = c(0,(mean(vect)+3*sd(vect))))
    abline(v = mean(vect), col = "blue")
  }
}
