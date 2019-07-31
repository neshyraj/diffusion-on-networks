#Trimesters for each village for diffusion centrality from paper
#Approx. number of trimesters for which program was run in each village
ti <- c(9,10,5,10,5,2,9,3,9,10,9,9,6,7,8,8,7,7,7,8,8,7,6,5,6,5,6,6,6,6,7,5,4,5,4,6,6,6,6,6,6,6,6)

#To run diffusion simulations. Initial infected/contagious ie leaders who adopt and not adopt. 
#This version only allows adoption once, but can still pass information
#g - network as an igraph object
#tf - time to run
#p_a_c - probability to adopt given neighbor adopted and in same caste
#p_a_d - probability to adopt given neighbor adopted and in different caste 
#p_n_c - probability to adopt given neighbor did not adopt and in same caste
#p_n_d - probability to adopt given neighbor did not adopt and in different caste
#model - logistic regression model trained on pre-determined set of leaders
diffusion <- function(g,tf,p_a_c,p_a_d,p_n_c,p_n_d,model){
  contagious <- as.vector(V(gra[[g]])[V(gra[[g]])$name %in% leaders[[g]] & V(gra[[g]])$adopt == 1])
  infected <- as.vector(V(gra[[g]])[V(gra[[g]])$name %in% leaders[[g]] & V(gra[[g]])$adopt == 2])
  recovered <- NULL
  village <- household[household$village == g,]
  drop <- c("village","HHnum_in_village","leader","rooftype1","rooftype2","rooftype3","rooftype4","rooftype5","room_no","bed_no","electricity","latrine","ownrent")
  village <- village[, !names(village) %in% drop]
  #Probability to adopt. Would be linear model depending on vertex
  #timestep
  informed <- rep(list(vector()),nrow(conadjmat[[g]]))
  clust_time <- list()
  adopt_rate <- NULL
  for (t in 1:tf){
    if (length(contagious) == 0){
      break
    }
    for (v in contagious){
      len_v <- length(neighbors(gra[[g]],V(gra[[g]])[v]))
      inf_v <- length(informed[[v]])
      if (len_v == inf_v && !v %in% recovered){
        recovered <- append(recovered,v)
      }
      else if (len_v > inf_v){
        for (i in as.vector(neighbors(gra[[g]],V(gra[[g]])[v]))){
          p <- predict(model, newdata = village[as.numeric(substr(as_ids(V(gra[[g]])[i]),2,5)),],type = 'response')
          if ((!i %in% contagious && !i %in% infected)){
            if (V(gra[[g]])[v]$caste == V(gra[[g]])[i]$caste){
              if (runif(1,0,1) < p_n_c && runif(1,0,1) < p){
                infected <- append(infected,i)
                informed[[v]] <- append(informed[[v]],i)
                informed[[i]] <- append(informed[[i]],v)
              }
              else if (runif(1,0,1) < p_n_c && runif(1,0,1) > p){
                contagious <- append(contagious,i)
                informed[[v]] <- append(informed[[v]],i)
                informed[[i]] <- append(informed[[i]],v)
              }
              else if (runif(1,0,1) > p_n_c){
                next
              }
            }
            else if (V(gra[[g]])[v]$caste != V(gra[[g]])[i]$caste){
              if (runif(1,0,1) < p_n_d && runif(1,0,1) < p){
                infected <- append(infected,i)
                informed[[v]] <- append(informed[[v]],i)
                informed[[i]] <- append(informed[[i]],v)
              }
              else if (runif(1,0,1) < p_n_d && runif(1,0,1) > p){
                contagious <- append(contagious,i)
                informed[[v]] <- append(informed[[v]],i)
                informed[[i]] <- append(informed[[i]],v)
              }
              else if (runif(1,0,1) > p_n_d){
                next
              }
            }
          }
          else if (i %in% contagious && !i %in% informed[[v]]){
            if (V(gra[[g]])[v]$caste == V(gra[[g]])[i]$caste){
              if (runif(1,0,1) < p_n_c){
                informed[[v]] <- append(informed[[v]],i)
                informed[[i]] <- append(informed[[i]],v)
              }
              else if (runif(1,0,1) > p_n_c){
                next
              }
            }
            else if (V(gra[[g]])[v]$caste != V(gra[[g]])[i]$caste){
              if (runif(1,0,1) < p_n_d){
                informed[[v]] <- append(informed[[v]],i)
                informed[[i]] <- append(informed[[i]],v)
              }
              else if (runif(1,0,1) > p_n_d){
                next
              }
            }
          }
          else {
            next
          }
        }
      }
    }
    for (v in infected){
      len_v <- length(neighbors(gra[[g]],V(gra[[g]])[v]))
      inf_v <- length(informed[[v]])
      if (len_v == inf_v){
        break
      }
      else if (len_v > inf_v){
        for (i in as.vector(neighbors(gra[[g]],V(gra[[g]])[v]))){
          p <- predict(model, newdata = village[as.numeric(substr(as_ids(V(gra[[g]])[i]),2,5)),],type = 'response')
          if ((!i %in% contagious && !i %in% infected)){
            if (V(gra[[g]])[v]$caste == V(gra[[g]])[i]$caste){
              if (runif(1,0,1) < p_a_c && runif(1,0,1) < p){
                infected <- append(infected,i)
                informed[[v]] <- append(informed[[v]],i)
                informed[[i]] <- append(informed[[i]],v)
              }
              else if (runif(1,0,1) < p_a_c && runif(1,0,1) > p){
                contagious <- append(contagious,i)
                informed[[v]] <- append(informed[[v]],i)
                informed[[i]] <- append(informed[[i]],v)
              }
              else if (runif(1,0,1) > p_a_c){
                next
              }
            }
            else if (V(gra[[g]])[v]$caste != V(gra[[g]])[i]$caste){
              if (runif(1,0,1) < p_a_d && runif(1,0,1) < p){
                infected <- append(infected,i)
                informed[[v]] <- append(informed[[v]],i)
                informed[[i]] <- append(informed[[i]],v)
              }
              else if (runif(1,0,1) < p_a_d && runif(1,0,1) > p){
                contagious <- append(contagious,i)
                informed[[v]] <- append(informed[[v]],i)
                informed[[i]] <- append(informed[[i]],v)
              }
              else if (runif(1,0,1) > p_a_d){
                next
              }
            }
          }
          else if (i %in% contagious && !i %in% informed[[v]]){
            if (V(gra[[g]])[v]$caste == V(gra[[g]])[i]$caste){
              if (runif(1,0,1) < p_a_c){
                informed[[v]] <- append(informed[[v]],i)
                informed[[i]] <- append(informed[[i]],v)
              }
              else if (runif(1,0,1) > p_a_c){
                next
              }
            }
            else if (V(gra[[g]])[v]$caste != V(gra[[g]])[i]$caste){
              if (runif(1,0,1) < p_a_d){
                informed[[v]] <- append(informed[[v]],i)
                informed[[i]] <- append(informed[[i]],v)
              }
              else if (runif(1,0,1) > p_a_d){
                next
              }
            }
          }
          else {
            next
          }
        }
      }
    }
    adopt_rate <- append(adopt_rate,length(infected)/nrow(adjmat[[g]]))
    clust_adopt <- NULL
    for (i in 1:length(unique(clusts[[g]]$community))){
      tot <- length(V(gra[[g]])[clusts[[g]]$community == i])
      per <- length(V(gra[[g]])[V(gra[[g]]) %in% infected & clusts[[g]]$community == i])
      clust_adopt <- rbind(clust_adopt, per/tot)
    }
    clust_time[[t]] <- clust_adopt
  }
  return(list(clust_time,adopt_rate))
}

#Different runs of diffusion model
#runs_new_const <- list()
for (i in 1:50){
  runs_new_const[[i]] <- diffusion(30,ti[30],0.35,0.35,0.05,0.05,model_new)
}

#runs_com_const <- list()
for (i in 1:50){
  runs_com_const[[i]] <- diffusion(30,ti[30],0.35,0.35,0.05,0.05,model_com)
}

#runs_new_alt <- list()
for (i in 1:50){
  runs_new_alt[[i]] <- diffusion(30,ti[30],0.42,0.28,0.06,0.04,model_new)
}

#runs_com_alt <- list()
for (i in 1:50){
  runs_com_alt[[i]] <- diffusion(30,ti[30],0.42,0.28,0.06,0.04,model_com)
}

#Diffusion centrality calculation as given in paper by Banerjee et al.
diff_cent <- function(g,t){
  a <- as.matrix(get.adjacency(g, type = "both"))
  q <- 1/max(eigen(a)$values)
  a1 <- a
  a <- q*a
  run_sum <- a
  for (i in 1:(t-1)){
    a <- a1 %*% (q*a)
    run_sum <- a + run_sum
  }
  d <- as.vector(colSums(run_sum))
  return(d)
}

#Regress adoption on diffusion centrality to test relationship significance
mean_diff_lead <- NULL
for (i in 1:43){
  d <- diff_cent(gra[[i]],ti[i])
  gra[[i]] <- set_vertex_attr(gra[[i]], "diff", value = d)
  mean_diff_lead <- rbind(mean_diff_lead, mean(vertex_attr(gra[[i]],"diff",V(gra[[i]])[V(gra[[i]])$leader == 2])))
}
