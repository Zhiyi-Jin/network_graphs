library(igraph)
library(sna)
library(networkdata)
library(kableExtra)
library(network)

#（1）Descriptive Network Statistics
#1-Brunson Corporate Leadership
#turn into network object
brunson_corporate_leadership_m <- as_adjacency_matrix(brunson_corporate_leadership,
                                                      sparse = FALSE)

#make a table
table1 <- c()
table1$Name <- c("Brunson Corporate Leadership")
table1$Density <- gden(brunson_corporate_leadership_m)
table1$Ave_degree <- mean(degree(brunson_corporate_leadership_m))
table1$Ave_indegree <- NA
table1$Ave_outdegree <- NA
table1$Sd_degree <- sd(degree(brunson_corporate_leadership_m))
table1$Sd_indegree <- NA
table1$Sd_outdegree <- NA
table1$Reciprocity <- NA
table1$Transitivity <- gtrans(brunson_corporate_leadership_m, measure = "weak")
table1$Ave_geodesic <- average.path.length(brunson_corporate_leadership, directed = FALSE)
table1$Connected <- is.connected(brunson_corporate_leadership_m, connected = "weak") 
table1$Isolates <- length(isolates(brunson_corporate_leadership_m))
table1 <- data.frame(table1)

#2-Radoslaw Email Network
radoslaw_email_m <- as_adjacency_matrix(radoslaw_email, sparse = FALSE)

table2 <- c()
table2$Name <- c("Radoslaw Email Network")
table2$Density <- gden(radoslaw_email_m)
table2$Ave_degree <- mean(degree(radoslaw_email_m))
table2$Ave_indegree <- mean(sna::degree(radoslaw_email_m, cmode = "indegree"))
table2$Ave_outdegree <- mean(sna::degree(radoslaw_email_m, cmode = "outdegree"))
table2$Sd_degree <- sd(degree(radoslaw_email_m))
table2$Sd_indegree <- sd(sna::degree(radoslaw_email_m, cmode = "indegree"))
table2$Sd_outdegree <- sd(sna::degree(radoslaw_email_m, cmode = "outdegree"))
table2$Reciprocity <- reciprocity(radoslaw_email)
table2$Transitivity <- gtrans(radoslaw_email_m, measure = "weak") 
table2$Ave_geodesic <- average.path.length(radoslaw_email, directed = TRUE)
table2$Connected <- is.connected(radoslaw_email_m, connected = "weak")
table2$Isolates <- length(isolates(radoslaw_email_m))
table2 <- data.frame(table2)

#3-Residence Hall Friendship
hall_m <- as_adjacency_matrix(hall, sparse = FALSE)

table3 <- c()
table3$Name <- c("Residence Hall Friendship")
table3$Density <- gden(hall_m)
table3$Ave_degree <- mean(degree(hall_m))
table3$Ave_indegree <- mean(sna::degree(hall_m, cmode = "indegree"))
table3$Ave_outdegree <- mean(sna::degree(hall_m, cmode = "outdegree"))
table3$Sd_degree <- sd(degree(hall_m))
table3$Sd_indegree <- sd(sna::degree(hall_m, cmode = "indegree"))
table3$Sd_outdegree <- sd(sna::degree(hall_m, cmode = "outdegree"))
table3$Reciprocity <- reciprocity(hall)
table3$Transitivity <- gtrans(hall_m, measure = "weak")
table3$Ave_geodesic <- average.path.length(hall, directed = TRUE)
table3$Connected <- is.connected(hall_m, connected = "weak")
table3$Isolates <- length(isolates(hall_m))
table3 <- data.frame(table3)

table4 <- rbind(table1, table2, table3)

#make a summary table
options(knitr.kable.NA = "-")
kable(table4,
      digits = 2,
      col.names = c("Network", "Density", "Average Degree", "Average Indegree", 
                    "Average Outdegree", "SD Degree", "SD Indegree", "SD Outdegree",
                    "Reciprocity", "Transitivity", "Average Shortest Path Length",
                    "Connected", "Isolates"),
      caption = "Table 1: Descriptive Network Statistics") %>% 
  kable_classic_2(full_width = F)

#（2）Degree distribution
par(mfrow=c(1,3))
d.1 <- degree(brunson_corporate_leadership_m)
hist(d.1, col = "blue",
     xlab = "Degree", ylab = "Frequency",
     main = "Brunson Corporate Leadership")

d.2 <- degree(radoslaw_email_m)
hist(d.2, col = "purple",
     xlab = "Degree", ylab = "Frequency",
     main = "Radoslaw Email Network")

d.3 <- degree(hall_m)
hist(d.3, col = "red",
     xlab = "Degree", ylab = "Frequency",
     main = "Residence Hall Friendship")

#（3）Visualization of the networks
plot(brunson_corporate_leadership,
     edge.width = 1,
     vertex.label = "",
     vertex.size = 4,
     layout = layout_with_fr(brunson_corporate_leadership),
     main = "Brunson Corporate Leadership") 


plot(radoslaw_email, 
     edge.arrow.size = .03, 
     vertex.size = 4,
     layout = layout_with_fr(radoslaw_email),
     vertex.label = "",
     main = "Radoslaw Email Network")

plot(hall, 
     edge.arrow.size = .03, 
     vertex.size = 4,
     vertex.label = "",
     layout = layout_with_fr(hall),
     main = "Residence Hall Friendship")

#（4）Community detection
#community detection for Brunson Corporate Leadership
b <- fastgreedy.community(brunson_corporate_leadership)
length(b)
sizes(b)

plot(b, brunson_corporate_leadership,
     edge.width = 1,
     vertex.size = 10,
     vertex.label = "",
     layout = layout_with_fr(brunson_corporate_leadership),
     main = "Brunson Corporate Leadership(Fast-Greedy)")

#community detection for Radoslaw Email Network
r <- walktrap.community(radoslaw_email)
length(r)
sizes(r)

plot(r, radoslaw_email, 
     edge.arrow.size = .05, 
     vertex.size = 6,
     layout = layout_with_fr(radoslaw_email),
     vertex.label = "",
     main = "Radoslaw Email Network(Walktrap)")

#community detection for Residence Hall Friendship network
h <- walktrap.community(hall)
length(h)
sizes(h)

plot(h, hall, 
     edge.arrow.size = .03, 
     vertex.size = 4,
     vertex.label = "",
     edge.color = "grey",
     layout = layout_with_fr(hall),
     main = "Residence Hall Friendship(Walktrap)")

#（5）Symmetrize the data
n2 <- as.undirected(radoslaw_email)
n3 <- as.undirected(hall)

r1 <- walktrap.community(n2)
length(r1)
sizes(r1)

h1 <- walktrap.community(n3)
length(h1)
sizes(h1)

par(mfrow=c(1,2))
plot(r, radoslaw_email, 
     edge.arrow.size = .05, 
     vertex.size = 6,
     layout = layout_with_fr(radoslaw_email),
     vertex.label = "",
     main = "Radoslaw Email Network")

plot(r1, radoslaw_email, 
     edge.arrow.size = .05, 
     vertex.size = 6,
     layout = layout_with_fr(radoslaw_email),
     vertex.label = "",
     main = "Symmetrized")

par(mfrow=c(1,2))
plot(h, hall, 
     edge.arrow.size = .03, 
     vertex.size = 4,
     vertex.label = "",
     edge.color = "grey",
     layout = layout_with_fr(hall),
     main = "Residence Hall Friendship")

plot(h1, hall, 
     edge.arrow.size = .03, 
     vertex.size = 4,
     vertex.label = "",
     edge.color = "grey",
     layout = layout_with_fr(hall),
     main = "Symmetrized")

#（6）Random graph “counterpart”
#Brunson Corporate Leadership network
c1 <- erdos.renyi.game(n = length(V(brunson_corporate_leadership)),
                       p.or.m = length(E(brunson_corporate_leadership)),
                       type = "gnm",
                       directed = FALSE,
                       loops = FALSE)
plot(c1,
     vertex.size = 4,
     vertex.label = "",
     layout = layout_with_fr(c1),
     main = "Brunson Corporate Leadership(counterpart)")

#Radoslaw Email Network
c2 <- erdos.renyi.game(n = length(V(radoslaw_email)),
                       p.or.m = length(E(radoslaw_email)),
                       type = "gnm",
                       directed = TRUE,
                       loops = FALSE)

plot(c2,
     edge.arrow.size = .03, 
     vertex.size = 4,
     vertex.label = "",
     layout = layout_with_fr(c2),
     main = "Radoslaw Email Network(counterpart)")

#Residence Hall Friendship network
c3 <- erdos.renyi.game(n = length(V(hall)),
                       p.or.m = length(E(hall)),
                       type = "gnm",
                       directed = TRUE,
                       loops = FALSE)

plot(c3,
     edge.arrow.size = .03, 
     vertex.size = 4,
     vertex.label = "",
     layout = layout_with_fr(c3),
     main = "Radoslaw Email Network(counterpart)")
     
#（7）Small network test
#Brunson Corporate Leadership network
t1 <- c()
c1_m <- as_adjacency_matrix(c1, sparse = FALSE)
t1$Counterpart <- c("Brunson Corporate Leadership")
t1$Ave_shortest_path <- average.path.length(c1, directed = FALSE)
t1$Transitivity <- gtrans(c1_m, measure = "weak")
t1 <- data.frame(t1)

#Radoslaw Email Network
t2 <- c()
c2_m <- as_adjacency_matrix(c2, sparse = FALSE)
t2$Counterpart <- c("Radoslaw Email Network")
t2$Ave_shortest_path <- average.path.length(c2, directed = TRUE)
t2$Transitivity <- gtrans(c2_m, measure = "weak")
t2 <- data.frame(t2)

#Residence Hall Friendship network
t3 <- c()
c3_m <- as_adjacency_matrix(c3, sparse = FALSE)
t3$Counterpart <- c("Residence Hall Friendship")
t3$Ave_shortest_path <- average.path.length(c3, directed = TRUE)
t3$Transitivity <- gtrans(c3_m, measure = "weak")
t3 <- data.frame(t3)

t4 <- rbind(t1, t2, t3)

kable(t4,
      digits = 2,
      col.names = c("Random Graph", "Average Shortest Path Length", "Transitivity"),
      caption = "Table 2: Descriptive Network Statistics(Random graph counterpart)") %>% 
  kable_classic_2(full_width = T)

#（8）Structural equivalence blockmodels
#Brunson Corporate Leadership
#transform the matrix into a network object
bcl_n <- as.network(brunson_corporate_leadership_m, directed = FALSE)

#performs a hierarchical clustering analysis
bcl_n.clustering <- equiv.clust(bcl_n,
                                equiv.fun = "sedist", cluster.method = "complete")
#plot the resulting dendrogram
plot(bcl_n.clustering, cex = 0.85, hang = -1,
     main = "Cluster Dendrogram - Brunson Corporate Leadership")

#where to cut
abline(h = 25, col = "red")

#Based on the cluster dendrogram, I try one partition with 3 positions
se.blockmodel.3 <- blockmodel(bcl_n, bcl_n.clustering, k = 3)

#matching block membership information with the original data set
se.position.3 <- 1 + se.blockmodel.3$block.membership[match(1:nrow(brunson_corporate_leadership_m), se.blockmodel.3$order.vector)]

#plot the network with the block colouring
set.seed(123)
gplot(bcl_n, vertex.col = se.position.3, 
      displaylabels = TRUE,
      label.cex = 0.8,
      edge.len = 5,
      edge.col = "grey")

#Radoslaw Email Network
re_n <- as.network(radoslaw_email_m, directed = TRUE)
re_n.clustering <- equiv.clust(re_n,
                               equiv.fun = "sedist", cluster.method = "complete")
plot(re_n.clustering, cex = 0.6, hang = -1,
     main = "Cluster Dendrogram - Radoslaw Email Network")
abline(h = 130, col = "red")

se.blockmodel.6 <- blockmodel(re_n, re_n.clustering, k = 6)
se.position.6 <- 1 + se.blockmodel.6$block.membership[match(1:nrow(radoslaw_email_m), se.blockmodel.6$order.vector)]

set.seed(456)
gplot(re_n, vertex.col = se.position.6,
      edge.len = 5,
      edge.col = "grey")

#Residence Hall Friendship network
h_n <- as.network(hall_m, directed = TRUE)
h_n.clustering <- equiv.clust(h_n,
                              equiv.fun = "sedist", cluster.method = "complete")
plot(h_n.clustering, cex = 0.5, hang = -1,
     main = "Cluster Dendrogram - Residence Hall Friendship")
abline(h = 80, col = "red")

se.blockmodel.7 <- blockmodel(h_n, h_n.clustering, k = 7)
se.position.7 <- 1 + se.blockmodel.7$block.membership[match(1:nrow(hall_m), se.blockmodel.7$order.vector)]

plot(density(se.blockmodel.7$block.membership), main = "Density distribution of block membership")

gplot(h_n, vertex.col = se.position.7,
      edge.col = "grey",
      mode = "mds")
 
gplot(se.blockmodel.7$block.model, gmode = "graph",
      label = rownames(se.blockmodel.7$block.model), edge.lwd = se.blockmodel.7$block.model)



