#To calculate variaton of information
install.packages("mcclust")
vi <- function(g){
v_orig <- NULL
v_con <- NULL
for (i in 1:1000){
  c <- membership(cluster_louvain(rewire(gra[[g]], with = keeping_degseq(niter = i))))
  v_orig <- rbind(v_orig,vi.dist(membership(cluster_louvain(gra[[g]])),c))
  con <- membership(cluster_louvain(rewire(gra_config[[g]], with = keeping_degseq(niter = i))))
  v_con <- rbind(v_con,vi.dist(membership(cluster_louvain(gra_config[[g]])),con))
}
return(cbind(v_orig,v_con))
}

plot(NULL, xlim = range(1,1000), ylim = range(-0.1,6), xlab = "Number of Rewired Edges", ylab = "Variation of Information")
lines(x=1:1000,y=v_orig)
lines(x=1:1000,y=v_con,col = "blue")
