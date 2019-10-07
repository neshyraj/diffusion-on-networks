#Plot network by some attributes
plot(gra[[1]], vertex.size = 3, vertex.label = NA, vertex.color = V(gra[[1]])$adopt, layout = layout_nicely)

#Plot by communities for any network
plot(gra[[10]], vertex.size = 3, vertex.label = NA, vertex.color = clusts[[10]]$community, layout = layout_nicely)

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
#Example with villages 21 and 30
plot(gra[[21]], vertex.size = 10, vertex.label = vertex_attr(gra[[21]],"adopt"), vertex.color = clusts[[21]]$community)
plot(gra[[30]], vertex.size = 10, vertex.label = vertex_attr(gra[[30]],"adopt"), vertex.color = vertex_attr(gra[[30]],"adopt"))

#plot both religion and caste assortativity against adoption
plot(x=assort_adopt[,4], y=assort_adopt[,9], xlim = range(-0.1,1),xlab = "Assortativity", ylab = "Adoption Rate", col = "blue")
points(assort_adopt[,3],assort_adopt[,9], col = "red")
abline(lm(V9 ~ a_cas, data = assort_adopt), col = "blue")
abline(lm(V9 ~ a_rel, data = assort_adopt), col = "red")
summary(lm(V9 ~ a_cas, data = assort_adopt))

#Plot of induced subgraph to visualize
plot(induced_subgraph(gra[[21]],V(gra[[21]])[clusts[[21]]$community == 1]), impl = "auto", vertex.size = 3, vertex.label = NA, vertex.color = vertex_attr(induced_subgraph(gra[[21]],V(gra[[21]])[clusts[[21]]$community == 1]),"adopt"))

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

#Plot community adoption over each time step
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
