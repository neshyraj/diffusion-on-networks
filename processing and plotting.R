#Import household characteristics as a data frame. Duplicate to edit values to be compatible with assortativity for adopt and leader
setwd("/Users/Neshy/Desktop/Oxford/Hilary/Mathematics and Data Science for Development/Project/social/adjacency matrices")
household <- read.csv("householdcharacteristics.csv", header = TRUE)
household[household$ownrent == "LEASED", 14] <- "RENTED"
household_2 <- household
household_2[household_2$adopt == 1, 16] <- 2
household_2[household_2$adopt == 0, 16] <- 1
household_2[household_2$leader == 1, 15] <- 2
household_2[household_2$leader == 0, 15] <- 1

#Import adjacency matrix for all villages and convert to adjacency matrix
adjmat_files <- paste("adjmat",1:43,".csv",sep="")
adjmat <- lapply(adjmat_files, read.csv, header = FALSE)
adjmat <- lapply(adjmat, as.matrix)

#Characteristics for each village
vil_char <- list()
for (i in 1:43){
  vil_char[[i]] <- household_2[household_2$village == i,]
}

#Convert adjacency matrix to igraph object
library("igraph")
gra <- list()
for (i in 1:43){
  gra[[i]] <- graph_from_adjacency_matrix(adjmat[[i]], mode = "undirected")
}

#Set vertex attributes for analysis
for (i in 1:43){
  gra[[i]] <- set_vertex_attr(gra[[i]], "adopt", value = vil_char[[i]][,16])
  gra[[i]] <- set_vertex_attr(gra[[i]], "leader", value = vil_char[[i]][,15])
  gra[[i]] <- set_vertex_attr(gra[[i]], "religion", value = vil_char[[i]][,3])
  gra[[i]] <- set_vertex_attr(gra[[i]], "caste", value = vil_char[[i]][,4])
  gra[[i]] <- set_vertex_attr(gra[[i]], "owned", value = vil_char[[i]][,14])
  gra[[i]] <- set_vertex_attr(gra[[i]], "latrine", value = vil_char[[i]][,13])
  gra[[i]] <- set_vertex_attr(gra[[i]], "electric", value = vil_char[[i]][,12])
}

#Calculate adoption rates on original networks
adoption <- NULL
for (i in 1:43){
  adoption = rbind(adoption, length(V(gra[[i]])[V(gra[[i]])$adopt == 2])/length(V(gra[[i]])[V(gra[[i]])$adopt]))
}

#Delete vertices with zero degree, not a leader and not adopted
for (i in 1:43){
  gra[[i]] <- delete_vertices(gra[[i]], V(gra[[i]])[igraph::degree(gra[[i]]) == 0 & V(gra[[i]])$leader == 1 & V(gra[[i]])$adopt == 1])
}

#Convert new graph back to adjacency matrix for certain measures
conadjmat <- list()
for (i in 1:43){
  conadjmat[[i]] <- as.matrix(get.adjacency(gra[[i]],type="both"))
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

#Plot network by some attributes
plot(gra[[1]], vertex.size = 3, vertex.label = NA, vertex.color = V(gra[[1]])$adopt, layout = layout_nicely)

#Mixing matrix
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

#Just run Louvain with default resolution.
clusts <- list()
for (i in 1:43){
  clusts[[i]] <- louvain(conadjmat[[i]])
}

#Plot by communities for any network
plot(gra[[10]], vertex.size = 3, vertex.label = NA, vertex.color = clusts[[10]]$community, layout = layout_nicely)

#Spectral clustering. Groups for number of unique node attributes. Based on caste
#PROBABLY NOT WORTH IT
spec <- list()
for (i in 1:43){
  spec[[i]] <- spectralClustering(conadjmat[[i]],length(unique(V(gra[[i]])$caste)))
}

#Configuration model for graph and adjacency (rewiring but preserving degrees)
gra_config <- list()
configadjmat <- list()
for(i in 1:43){
  gra_config[[i]] <- rewire(gra[[i]], with = keeping_degseq(loops = FALSE, niter = vcount(gra[[i]]) * 10))
  configadjmat[[i]] <- as.matrix(get.adjacency(gra_config[[i]],type="both"))
}

#Clusts for configuration model
#clusts_con <- list() #so i dont accidentially fuckin run it
for (i in 31:43){
  clusts_con[[i]] <- louvain(configadjmat[[i]])
}

#Compute variation of information
install.packages("mcclust")

#Append assortativity and modularity to one frame
mod <- NULL
for (i in 1:43){
  mod <- rbind(mod,clusts[[i]]$Q)
}
assort_adopt <- data.frame(cbind(assort,mod,adoption))

#Look at scatterplots for assortativity vs adoption
install.packages("ggplot2")
scatter.smooth(x=assort_adopt[,2],y=assort_adopt[,5], xlab = "Assortativity on Caste", ylab = "Modularity", col = "blue")

#Run regression on the different assortativty
summary(lm(V9 ~ a_own, data = assort_adopt))
V(gra[[10]])[igraph::degree(gra[[10]]) == 0]

#Run regression on modularity vs adoption and modularity vs assortativity
#Low p-value on caste and religion vs modularity suggests that these are the seperations driving the community structure
reg_cas <- lm(V9 ~ a_cas, data = assort_adopt)
with(assort_adopt,plot(a_cas,V9,col = "blue",xlab = "Strength of Same Caste Preference", ylab = "Microfinance Participation Rate",main = "Adoption Rate by Caste Preference"))
clip(0.15,0.75,0,1)
abline(reg_cas)
summary(lm(V9 ~ a_cas, data = assort_adopt))

#Plot of network 10 original
{gra_10 <- graph_from_adjacency_matrix(adjmat[[10]], mode = "undirected")
  gra_10 <- set_vertex_attr(gra_10, "adopt", value = vil_char[[10]][,16])
  gra_10 <- set_vertex_attr(gra_10, "leader", value = vil_char[[10]][,15])
  plot(gra_10, vertex.size = 7, vertex.label = V(gra_10)$leader, vertex.color = V(gra_10)$adopt, layout = layout_nicely)}

#Look at community detection and look at nodes within communities
#First try with village 21
plot(gra[[21]], vertex.size = 10, vertex.label = vertex_attr(gra[[21]],"adopt"), vertex.color = clusts[[21]]$community)
plot(gra[[30]], vertex.size = 10, vertex.label = vertex_attr(gra[[30]],"adopt"), vertex.color = vertex_attr(gra[[30]],"adopt"))

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

plot(comm_sum_comb[comm_sum_comb$nodes != 1,8],comm_sum_comb[,6])
summary(lm(per_com_adopt ~ rat_lead, data = data.frame(comm_sum_comb)))

#plot both religion and caste assortativity against adoption
plot(x=assort_adopt[,4], y=assort_adopt[,9], xlim = range(-0.1,1),xlab = "Assortativity", ylab = "Adoption Rate", col = "blue")
points(assort_adopt[,3],assort_adopt[,9], col = "red")
abline(lm(V9 ~ a_cas, data = assort_adopt), col = "blue")
abline(lm(V9 ~ a_rel, data = assort_adopt), col = "red")
summary(lm(V9 ~ a_cas, data = assort_adopt))

#Plot of induced subgraph to visualize
plot(induced_subgraph(gra[[21]],V(gra[[21]])[clusts[[21]]$community == 1]), impl = "auto", vertex.size = 3, vertex.label = NA, vertex.color = vertex_attr(induced_subgraph(gra[[21]],V(gra[[21]])[clusts[[21]]$community == 1]),"adopt"))

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

#First find diffusion centrality on a community level and assign to each node at the network level
for (i in 1:43){
  for (j in 1:length(unique(clusts[[i]]$community))){
    gt <- induced_subgraph(gra[[i]],V(gra[[i]])[clusts[[i]]$community == j])
    d <- diff_cent(gt,ti[i])
    gra[[i]] <- set_vertex_attr(gra[[i]], "com_diff",V(gra[[i]])[clusts[[i]]$community == j], value = d)
  }
}

#Number of leaders in each village
num_leaders <- NULL
for (i in 1:43){
  num <- length(V(gra[[i]])[V(gra[[i]])$leader == 2])
  num_leaders <- rbind(num_leaders,num)
}

#Number of leaders needed for each community to be proportional to size
#Round to nearest integer
num_needed_com <- list()
for (i in 1:43){
  num <- NULL
  for (j in 1:length(unique(clusts[[i]]$community))){
    n <- round(comm_sum[[i]][j,3]*num_leaders[i])
    num <- rbind(num,n)
  }
  num_needed_com[[i]] <- num
}

#Select leaders for each village based on leader choosing criteria
leaders <- list()
for (i in 1:43){
  leads <- NULL
  for (j in 1:length(unique(clusts[[i]]$community))){
    if (length(V(gra[[i]])[clusts[[i]]$community == j]) > 1){
      gt <- induced_subgraph(gra[[i]],V(gra[[i]])[clusts[[i]]$community == j])
      top <- (-(sort(-vertex_attr(gt,"com_diff",V(gt)))))[num_needed_com[[i]][j]]
      verts <- vertex_attr(gt,"name",V(gt)[V(gt)$com_diff >= top])
      leads <- append(leads, verts)
    }
    else {
      next
    }
  }
  leaders[[i]] <- leads
}

#Select leaders for each village incorporating caste/religion as well
leader_com <- list()
for (i in 1:43){
  leads <- NULL
  if (isTRUE(assort_adopt[i,4] > assort_adopt[i,3]) || is.na(assort_adopt[i,3])){
    for (c in 1:length(unique(clusts[[i]]$community))){
      if (length(V(gra[[i]])[clusts[[i]]$community == c]) > 1){
        gt <- induced_subgraph(gra[[i]],V(gra[[i]])[clusts[[i]]$community == c])
        for (o in unique(V(gt)$caste)){
          num <- trunc((length(V(gt)[V(gt)$caste == o])/length(V(gt)))*num_needed_com[[i]][c])
          top <- (-(sort(-vertex_attr(gt,"com_diff",V(gt)[V(gt)$caste == o]))))[num]
          verts <- vertex_attr(gt,"name",V(gt)[V(gt)$caste == o & V(gt)$com_diff >= top])
          leads <- append(leads,verts)
        }
      }
    }
  }
  else {
    for (c in 1:length(unique(clusts[[i]]$community))){
      if (length(V(gra[[i]])[clusts[[i]]$community == c]) > 1){
        gt <- induced_subgraph(gra[[i]],V(gra[[i]])[clusts[[i]]$community == c])
        for (o in unique(V(gt)$religion)){
          num <- trunc((length(V(gt)[V(gt)$religion == o])/length(V(gt)))*num_needed_com[[i]][c])
          top <- (-(sort(-vertex_attr(gt,"com_diff",V(gt)[V(gt)$religion == o]))))[num]
          verts <- vertex_attr(gt,"name",V(gt)[V(gt)$com_diff >= top])
          leads <- append(leads,verts)
        }
      }
    }
  }
  leader_com[[i]] <- leads
}

#Combine all leaders and query characteristics to train logistic regression model on. Manually add Christian leaders.
all_leaders <- NULL
for (i in 1:43){
  village <- household[household$village == i,]
  lead_num <- as.numeric(substr(leaders[[i]],2,10))
  l <- village[lead_num,]
  all_leaders <- rbind(all_leaders,l)
}
all_leaders <- rbind(all_leaders,household[1008,c(3,4,16)],household[1288,c(3,4,16)],household[1559,c(3,4,16)],household[2088,c(3,4,16)],household[3066,c(3,4,16)],household[4179,c(3,4,16)],household[7214,c(3,4,16)],household[9099,c(3,4,16)])
drop <- c("village","HHnum_in_village","leader","rooftype1","rooftype2","rooftype3","rooftype4","rooftype5","room_no","bed_no","electricity","latrine","ownrent")
all_leaders <- all_leaders[, !names(all_leaders) %in% drop]

#Combine all leaders and query characteristics incorporating caste/rel breakdown
all_com_leaders <- NULL
for (i in 1:43){
  village <- household[household$village == i,]
  lead_num <- as.numeric(substr(leader_com[[i]],2,10))
  l <- village[lead_num,]
  all_com_leaders <- rbind(all_com_leaders,l)
}
all_com_leaders <- rbind(all_com_leaders,household[1008,c(3,4,16)],household[1288,c(3,4,16)],household[1559,c(3,4,16)],household[2088,c(3,4,16)],household[3066,c(3,4,16)],household[4179,c(3,4,16)],household[7214,c(3,4,16)],household[9099,c(3,4,16)])
all_com_leaders <- all_com_leaders[, !names(all_com_leaders) %in% drop]

#Remaining villages for choosing leaders considering caste/religion breakdown
#Still trained model on all leaders in all villages though
rem_vils <- c(4,7,14,15,16,17,18,19,20,21,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43)

#Original leaders
orig_leaders <- household[household$leader == 1,]
orig_leaders <- orig_leaders[, !names(orig_leaders) %in% drop]

#Logistic model trained on each data set
model_orig <- glm(adopt ~ .,family = binomial(link = 'logit'), data = orig_leaders)
model_new <- glm(adopt ~ .,family = binomial(link = 'logit'), data = all_leaders)
model_com <- glm(adopt ~ .,family = binomial(link = 'logit'), data = all_com_leaders)

#Get remaining household adoption
adopt_new_alt1 <- adopt_new_alt[rem_vils]
adopt_new_const1 <- adopt_new_const[rem_vils]

#Runs of diffusion model
new_const_all <- list()
for (i in 1:43){
  runsnewconst <- list()
  for (t in 1:5){
    runsnewconst[[t]] <- diffusion(i,ti[i],0.35,0.35,0.05,0.05,model_new)
  }
  new_const_all[[i]] <- runsnewconst
}

adopt_new_const <- NULL
for (i in 1:43){
  adopt_new_const <- append(adopt_new_const,diffusion_2(i,ti[i],0.35,0.35,0.05,0.05,model_new))
}

adopt_new_alt <- NULL
for (i in 1:43){
  adopt_new_alt <- append(adopt_new_alt,diffusion_2(i,ti[i],0.42,0.28,0.06,0.04,model_new))
}

adopt_com_const <- NULL
for (i in rem_vils){
  adopt_com_const <- append(adopt_com_const,diffusion_2(i,ti[i],0.35,0.35,0.05,0.05,model_com))
}

adopt_com_alt <- NULL
for (i in rem_vils[26:31]){
  adopt_com_alt <- append(adopt_com_alt,diffusion_2(i,ti[i],0.42,0.28,0.06,0.04,model_com))
}

#Two plots of actual vs simulated adoption
#Plot for constant probability with both leader sets
plot(adopt_new_const1, adoption[rem_vils],xlab = "Simulated Adoption Rate", ylab = "Actual Adoption Rate", col = "red", xlim = c(0,0.4),ylim = c(0,0.4))
points(adopt_com_const, adoption[rem_vils], col = "blue")
points(adopt_new_alt1, adoption[rem_vils], col = "green")
points(adopt_com_alt, adoption[rem_vils], col = "black")
abline(0,1)

#Plot for alternate probability with both leader sets
plot(adopt_new_alt1, adoption[rem_vils],xlab = "Simulated Adoption Rate", ylab = "Actual Adoption Rate", col = "red", xlim = c(0,0.4),ylim = c(0,0.4))
points(adopt_com_alt, adoption[rem_vils], col = "blue")
abline(0,1)

#Plot by community
View(runs_com_alt[[44]][[1]])
View(runs_new_alt[[34]][[1]])

#Read in aggregated data from lists 
datcom <- read.csv("/Users/Neshy/Desktop/Oxford/Hilary/Mathematics and Data Science for Development/Project/datacom.csv", header = FALSE)
datnew <- read.csv("/Users/Neshy/Desktop/Oxford/Hilary/Mathematics and Data Science for Development/Project/datanew.csv", header = FALSE)

#PLot community adoption over each time step
plot(NULL, xlim = c(1,6), ylim = c(0,0.3), xlab = "Time Step", ylab = "Community Level Adoption Rate")
lines(1:6,datcom[1,],col ="orange")
lines(1:6,datcom[2,],col = "red")
lines(1:6,datcom[3,],col = "green")
lines(1:6,datcom[4,],col = "blue")
lines(1:6,datcom[5,],col = "black")
lines(1:6,datcom[6,],col = "yellow")
lines(1:6,datcom[7,],col ="purple")
points(1:6,datcom[1,],col ="orange")
points(1:6,datcom[2,],col = "red")
points(1:6,datcom[3,],col = "green")
points(1:6,datcom[4,],col = "blue")
points(1:6,datcom[5,],col = "black")
points(1:6,datcom[6,],col = "yellow")
points(1:6,datcom[7,],col ="purple")
legend(1,0.3,legend = c("Com. 1", "Com. 2", "Com. 3", "Com. 4", "Com. 5", "Com. 6", "Com. 7"), col = c("orange","red","green","blue","black","yellow","purple"), lty = 1,cex = 0.6)