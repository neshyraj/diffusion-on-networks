#Create rho_train as duplicate of data rho
rho_train <- rho
#Select random monks to zero truncate preferences
rand_monks <- sample(1:18, replace = FALSE, 6)
for (i in rand_monks){
  if (sum(rho[i,]>0) == 3){
    rho_train[i,3] <- 0
  }
  else {
    rho_train[i,4] <- 0
  }
}

#Initialize w
w1 <- matrix(runif(72),nrow = 18, ncol = 4)
w2 <- w1

#Run EM algorithm to estimate w
#Store results in w_fit
w_run <- run_em(2,3,w1,w2,rho_train,0.0001,200)
w_fit <- w_run[[1]]
log_fit <- as.matrix(w_run[[2]],nrow=length(w_run[[2]],ncol=1))

#Calculate lambda_hat
lambda_fit <- matrix(0, nrow = 18, ncol = 18)
for (i in 1:18){
  for (j in 1:18){
    if (i != j){
      sum_c <- 0
      for (c in 1:ncol(w_fit)){
        sum_c <- sum_c + w_fit[i,c]*w_fit[j,c]
      }
      lambda_fit[i,j] <- sum_c
    }
    else {
      lambda_fit[i,j] <- 0
    }
  }
}

#Find prediction based on top lambda
rho_pred <- rho_train
for (i in 1:18){
  #Loop through all i to single out rand_monks
  if (i %in% rand_monks){
    pred_cur <- 0
    lamb_cur <- 0
    #Check all lambda_ij
    for (j in 1:18){
      if (!j %in% rho_train[i,]){
        pred <- j
        lamb <- lambda_fit[i,j]
        #Set to lambda only if greater than
        #current lambda
        if (lamb > lamb_cur){
          pred_cur <- pred
          lamb_cur <- lamb
        }
      }
      else {
        next
      }
    }
    #pred_cur associated with highest lambda_cur
    if (sum(rho[i,]>0) == 3){
      rho_pred[i,3] <- pred_cur
    }
    else {
      rho_pred[i,4] <- pred_cur
    }
  }
  else {
    next
  }
}

#Function to sample from distribution of preferences
#for a single individual
#Inputs same as before except:

#pref: Preference number
#lambda: Estimated lambda
#samp: Number of samples from distribution
dist_pref <- function(ind,pref,lambda,rho,samp){
  #Initialize probability vector
  probs <- matrix(0, nrow = length(lambda), ncol = 1)
  sum_den <- 0
  #Loop to get sum of denominator
  for (j in 1:nrow(lambda)){
    if (j != ind && !j %in% rho[ind,1:(pref-1)]){
      sum_den <- sum_den + lambda[ind,j]
    }
    else {
      next
    }
  }
  #Loop to fill probability vector
  for (j in 1:nrow(lambda)){
    if (j != ind && !j %in% rho[ind,1:(pref-1)]){
      probs[j] <- lambda[ind,j]/sum_den
    }
    else {
      next
    }
  }
  prefs <- NULL
  pr <- NULL
  #Create data frame with potential monk index
  #and associated probability
  for (i in 1:nrow(lambda)){
    if (probs[i] == 0){
      next
    }
    else {
      prefs <- append(prefs,i)
      pr <- append(pr,probs[i])
    }
  }
  pref <- cbind(prefs,pr)
  #Sample from distribution of potential preferences
  dist <- sample(x = pref[,1], samp, replace = TRUE, prob = pref[,2])
  return(list(pref,dist))
}
