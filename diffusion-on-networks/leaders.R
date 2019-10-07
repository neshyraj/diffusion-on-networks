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
for (i in rem_vils[1:31]){
  adopt_com_alt <- append(adopt_com_alt,diffusion_2(i,ti[i],0.42,0.28,0.06,0.04,model_com))
}
