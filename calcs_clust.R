#Calculate adoption rates on original networks
adoption <- NULL
for (i in 1:43){
  adoption = rbind(adoption, length(V(gra[[i]])[V(gra[[i]])$adopt == 2])/length(V(gra[[i]])[V(gra[[i]])$adopt]))
}

#Calculate connectedness of network from original adjacency matrix
install.packages("sna")
library(sna)
conn <- NULL
for (i in 1:43){
  conn <- rbind(conn, connectedness(adjmat[[i]]))
}

#Same for new matrices
conn_new <- NULL
for (i in 1:43){
  conn_new <- rbind(conn_new, connectedness(conadjmat[[i]]))
}

#Calculate assortativity on connected graph
assort <- NULL
for (i in 1:43){
  a_leader <- assortativity_nominal(gra[[i]], types = V(gra[[i]])$leader)
  a_adopt <- assortativity_nominal(gra[[i]], types = V(gra[[i]])$adopt)
  a_rel <- assortativity_nominal(gra[[i]], types = V(gra[[i]])$religion)
  a_cas <- assortativity_nominal(gra[[i]], types = V(gra[[i]])$caste)
  a_own <- assortativity_nominal(gra[[i]], types = V(gra[[i]])$owned)
  a_lat <- assortativity_nominal(gra[[i]], types = V(gra[[i]])$latrine)
  a_elec <- assortativity_nominal(gra[[i]], types = V(gra[[i]])$electric)
  assort <- rbind(assort, cbind(a_leader,a_adopt,a_rel,a_cas,a_own,a_lat,a_elec))
  rm(a_leader,a_adopt,a_rel,a_cas,a_own,a_lat,a_elec)
}

#Mixing matrix
#input mix is output from running mixing matrix given by: https://gist.github.com/chengjun/2410446
#output is corrected, symmetrical mixing matrix for our analysis
mixmat_correct <- function(mix,adjmat){
  size <- nrow(mix)
  mat <- matrix(0, nrow = nrow(mix), ncol = ncol(mix))
  sum <- sum(adjmat)
  for (i in 1:size){
    for (j in 1:size){
      if (i == j){
        mat[i,j] <- 2*mix[i,j]/sum
      }
      else if (i < j){
        mat[i,j] <- (mix[i,j] + mix[j,i])/sum
        mat[j,i] <- (mix[i,j] + mix[j,i])/sum
      }
      else{
        next
      }
    }
  }
  return(mat)
}
mix_adopt <- list()
mix_leader <- list()
mix_rel <- list()
mix_cas <- list()
for (i in 1:43){
  mix_adopt[[i]] <- mixmat_correct(mixmat(gra[[i]], "adopt"), conadjmat[[i]])
  mix_leader[[i]] <- mixmat_correct(mixmat(gra[[i]], "leader"), conadjmat[[i]])
  mix_rel[[i]] <- mixmat_correct(mixmat(gra[[i]], "religion"), conadjmat[[i]])
  mix_cas[[i]] <- mixmat_correct(mixmat(gra[[i]], "caste"), conadjmat[[i]])
}

#Louvain modularity clustering for several resolution parameters
library("NetworkToolbox")
clust_ass <- list()
clust_qnum <- list()
for (j in 1:6){
  clust_assig <- NULL #stores actual clusters at each gamma
  clust_q <- NULL #want gamma, modularity and number clusters
  for (i in seq(0.05,1,0.05)){
    clust <- louvain(conadjmat[[j]],i)
    clust_assig <- cbind(clust_assig, as.matrix(clust$community))
    clust_q <- rbind(clust_q, (cbind(i, clust$Q, length(unique(clust$community)))))
  }
  clust_ass[[j]] <- clust_assig
  clust_qnum[[j]] <- clust_q
}

#Louvain with default resolution of 1
clusts <- list()
for (i in 1:43){
  clusts[[i]] <- louvain(conadjmat[[i]])
}

#Spectral clustering. Groups for number of unique node attributes. Based on number of castes.
spec <- list()
for (i in 1:43){
  spec[[i]] <- spectralClustering(conadjmat[[i]],length(unique(V(gra[[i]])$caste)))
}

#Configuration model for graph and adjacency (rewiring but preserving degrees)
#Purpose to check robustness of communities with variation of information given in variationinfo
gra_config <- list()
configadjmat <- list()
for(i in 1:43){
  gra_config[[i]] <- rewire(gra[[i]], with = keeping_degseq(loops = FALSE, niter = vcount(gra[[i]]) * 10))
  configadjmat[[i]] <- as.matrix(get.adjacency(gra_config[[i]],type="both"))
}

#Clusters for configuration model
clusts_con <- list() 
for (i in 31:43){
  clusts_con[[i]] <- louvain(configadjmat[[i]])
}

#Append assortativity and modularity to one frame
mod <- NULL
for (i in 1:43){
  mod <- rbind(mod,clusts[[i]]$Q)
}
assort_adopt <- data.frame(cbind(assort,mod,adoption))

#Look at attributes of nodes in communities
comm_summary <- function(j){
  comb <- NULL
  for (i in 1:length(unique(clusts[[j]]$community))){
    gt <- induced_subgraph(gra[[j]],V(gra[[j]])[clusts[[j]]$community == i])
    gt <- set_vertex_attr(gt, "diff", value = diff_cent(gt,ti[j]))
    temp <- NULL
    comm <- i
    nodes <- length(V(gra[[j]])[clusts[[j]]$community == i])
    per_total <- length(V(gra[[j]])[clusts[[j]]$community == i])/length(V(gra[[j]]))
    per_tot_adopt <- length(V(gra[[j]])[clusts[[j]]$community == i & V(gra[[j]])$adopt == 2])/length(V(gra[[j]])[V(gra[[j]])$adopt == 2])
    per_com_adopt <- length(V(gt)[V(gt)$adopt == 2])/length(V(gt))
    per_tot_leader <- length(V(gra[[j]])[clusts[[j]]$community == i & V(gra[[j]])$leader == 2])/length(V(gra[[j]])[V(gra[[j]])$leader == 2])
    rat_lead <- per_tot_leader/per_total
    assort_rel <- assortativity_nominal(gt,V(gt)$religion)
    assort_cas <- assortativity_nominal(gt,V(gt)$caste)
    mean_deg <- mean(igraph::degree(gt))
    mean_deg_lead <- mean(igraph::degree(gt,V(gt)[V(gt)$leader == 2]))
    mean_dist <- mean_distance(gt)
    mean_diff <- mean(vertex_attr(gt,"diff"))
    mean_diff_lead <- mean(vertex_attr(gt, "diff", V(gt)[V(gt)$leader == 2]))
    rat_diff <- mean_diff_lead/mean_diff
    mean_clo <- mean(igraph::closeness(gt, V(gt), normalized = TRUE))
    lead_clo <- mean(igraph::closeness(gt, V(gt)[V(gt)$leader == 2], normalized = TRUE))
    rat_clo <- lead_clo/mean_clo
    temp <- cbind(comm,nodes,per_total,per_tot_adopt,per_com_adopt,per_tot_leader,rat_lead,assort_rel,assort_cas,mean_deg,mean_deg_lead,mean_dist,mean_clo,mean_diff,mean_diff_lead,rat_diff,lead_clo,rat_clo)
    comb <- rbind(comb,temp)
  }
  return(comb)
}
comm_sum <- list()
for (i in 1:43){
  comm_sum[[i]] <- comm_summary(i)
}

#Combine all comm summ
comm_sum_comb <- NULL
for ( i in 1:43){
  comm_sum_comb <- rbind(comm_sum_comb,cbind(i,comm_sum[[i]]))
}

#Percentage of different castes in each community
caste_per <- list()
for (i in 1:43){
  cas <- NULL
  for (j in 1:length(unique(clusts[[i]]$community))){
    cas_1 <- NULL
    for (k in unique(V(gra[[i]])$caste)){
      per <- length(V(gra[[i]])[V(gra[[i]])$caste == k & clusts[[i]]$community == j])/length(V(gra[[i]])[clusts[[i]]$community == j])
      cas_1 <- cbind(cas_1,per)
    }
    cas <- rbind(cas,cas_1)
  }
  caste_per[[i]] <- cas
}

#Percentage of different religions in each community
rel_per <- list()
for (i in 1:43){
  rel <- NULL
  for (j in 1:length(unique(clusts[[i]]$community))){
    rel_1 <- NULL
    for (k in unique(V(gra[[i]])$religion)){
      relper <- length(V(gra[[i]])[V(gra[[i]])$religion == k & clusts[[i]]$community == j])/length(V(gra[[i]])[clusts[[i]]$community == j])
      rel_1 <- cbind(rel_1,relper)
    }
    rel <- rbind(rel,rel_1)
  }
  rel_per[[i]] <- rel
}

#Max percent of religion in each community
max_rel <- list()
for (i in 1:43){
  rel <- NULL
  for (j in 1:nrow(rel_per[[i]])){
    rel <- rbind(rel,max(rel_per[[i]][j,]))
  }
  max_rel[[i]] <- rel
}

#Same for caste
max_cas <- list()
for (i in 1:43){
  cas <- NULL
  for (j in 1:nrow(caste_per[[i]])){
    cas <- rbind(cas,max(caste_per[[i]][j,]))
  }
  max_cas[[i]] <- cas
}

#Putting it all in one column
rel_dom <- NULL
for (i in 1:43){
  rel_dom <- rbind(rel_dom,max_rel[[i]])
}

#Same for caste
cas_dom <- NULL
for (i in 1:43){
  cas_dom <- rbind(cas_dom,max_cas[[i]])
}
