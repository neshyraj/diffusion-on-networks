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

#Delete vertices with zero degree, not a leader and not adopted
for (i in 1:43){
  gra[[i]] <- delete_vertices(gra[[i]], V(gra[[i]])[igraph::degree(gra[[i]]) == 0 & V(gra[[i]])$leader == 1 & V(gra[[i]])$adopt == 1])
}

#Convert new graph back to adjacency matrix for certain measures
conadjmat <- list()
for (i in 1:43){
  conadjmat[[i]] <- as.matrix(get.adjacency(gra[[i]],type="both"))
}
