########################################
### Lab 1 - Graphical Models
### By Hannes Bengtsson
### TDDE15 - Advanced Machine Learning
### Linköping University
########################################

########################################
################ Task 1 ################
########################################

# Comparison of two BN:s structure learned from the same dataset
# We use the bnlearn and Rgraphviz packages

# Install and import packages
install.packages("bnlearn")
install.packages("Rgraphviz")
library(bnlearn)
library(Rgraphviz)

# Import the asia dataset
data("asia") 

# Initiate hill climb
hc1 <- hc(asia, score="bic")
hc2 <- hc(asia, score="aic")

# Plotting the graphs
graphviz.plot(hc1, shape = "circle")
graphviz.plot(hc2, shape = "circle")

# Plotting the BN:s equivalent classes 
graphviz.plot(cpdag(hc1), shape = "circle")
graphviz.plot(cpdag(hc2), shape = "circle")

# Checking for v-structures i.e., unsheilded colliders
# see the cpdag package for further potential comparisons
vstructs(hc1)
vstructs(hc2)

# List all the edges
arcs(hc1)
arcs(hc2)

# Compares the edges between the two BN:s
all.equal(hc1,hc2)

# Highlights differences between the two BN:s equivalence classes
graphviz.compare(cpdag(hc1), cpdag(hc2), shape="circle")


########################################
################ Task 2 ################
########################################

# Learning the structure and parameters for the Asia dataset using
# exact inference and predicting values for the S-node in the test data
# Furthermore, we compare our findings against the true structure

# Divide data
n<-dim(asia)[1]
set.seed(12345)
id<-sample(1:n, floor(n*0.8))
train<-asia[id,]
test<-asia[-id,]

# Install gRain
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("RBGL", force=TRUE)
BiocManager::install("Rgraphviz")
BiocManager::install("gRain")

library(gRain)

### Learning structure and parameters using exact inference ####################

# Learn the structure using Hill Climbing algorithm
bn_structure <- hc(train)
graphviz.plot(bn_structure)

# Estimate parameters of the Bayesian Network
bn_parameters <- bn.fit(bn_structure, train, method="mle")
bn_grain <- as.grain(bn_parameters)
bn_grain <- compile(bn_grain)

# Initialize an empty vector to store posterior probabilities
posterior_probs <-numeric(nrow(test))

# Find the posterior probabilities for each observation in test
# 1 to 1000 since test contains 1000 observations
for(i in 1:1000){
  # Set the actual values for A, T, L, B, E, X, and D as evidence for obs i
  evidence <-setEvidence(bn_grain,nodes=c("A","T","L","B","E","X","D"),
                         states=as.character(unlist(test[i, -2])))
  # Calculate the prob for nodes not in the evidence, i.e., node S
  prob <- querygrain(evidence)
  posterior_probs[i] <- prob$S["yes"]
}

# Classify based on the most likely class
pred <- ifelse(posterior_probs > 0.5, "yes", "no")
confusion_matrix <- table(Predicted = pred, Actual = test$S)

# Calculate true/false positives/negatives
TP <- confusion_matrix["yes", "yes"]
TN <- confusion_matrix["no", "no"]
FN <- confusion_matrix["no", "yes"]
FP <- confusion_matrix["yes", "no"]

# Print the confusion matrix
confusion_matrix

### Compare exact inference results with true Asia BN ##########################

# The true structure of the Asia BN
dag <- model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")

graphviz.plot(dag)
graphviz.compare(cpdag(dag), cpdag(bn_structure))
# Blue dashed or red line highlights the same thing
# What we get depends on the order of the dags in the graphviz.compare function

dag.grain <- as.grain(bn.fit(dag,train))
dag.grain <- compile(dag.grain)

posterior_probs <-numeric(nrow(test))

for(i in 1:1000){
  evidence <-setEvidence(dag.grain,nodes=c("A","T","L","B","E","X","D"),
                         states=as.character(unlist(test[i, -2])))
  prob <- querygrain(evidence)
  posterior_probs[i] <- prob$S["yes"]
}

# Classify based on the most likely class
pred <- ifelse(posterior_probs > 0.5, "yes", "no")
confusion_matrix <- table(Predicted = pred, Actual = test$S)

# Print the confusion matrix
confusion_matrix


########################################
################ Task 3 ################
########################################

S_mb <- mb(bn_structure, node="S") # Returns MB for S

bn_structure <- hc(train)
bn_parameters <- bn.fit(bn_structure, train, method="mle")
bn_grain <- as.grain(bn_parameters)

filtered_test <- test[4:5] # Test data for MB

posterior_probs <-numeric(nrow(filtered_test))

for(i in 1:1000){
  evidence <-setEvidence(bn_grain,nodes=c(S_mb),
                         states=as.character(unlist(filtered_test[i,])))
  prob <- querygrain(evidence)
  posterior_probs[i] <- prob$S["yes"]
}

# Classify based on the most likely class
pred <- ifelse(posterior_probs > 0.5, "yes", "no")
confusion_matrix <- table(Predicted = pred, Actual = test$S)

# Print the confusion matrix
confusion_matrix
sum(diag(confusion_matrix))/sum(confusion_matrix) # Accuracy

########################################
################ Task 4 ################
########################################

# Using a Naive Bayes classifier for the same task as in task 2

# Create Naive Bayes classifier, i.e., S points to all other nodes which is 
# assumed to be independent of each other, hence "naive"
e <-  empty.graph(c("A","S","T","L","B","E","X","D"))
# e <- model2network("[S][A|S][T|S][L|S][B|S][E|S][X|S][D|S]", 
                   #ordering =  c("A","S","T","L","B","E","X","D")) # Alt.

class(e)
graphviz.plot(e)
arc <-matrix(c("S", "A", "S", "T", "S", "L", "S", "B","S", "E", "S", "X", "S", "D"), 
                 ncol = 2, byrow = TRUE,
                 dimnames = list(NULL, c("from", "to")))

arcs(e) <- arc
graphviz.plot(e) # Visualization

e.grain <- as.grain(bn.fit(e,train))
e.grain <- compile(e.grain)

posterior_probs <-numeric(nrow(test))
for(i in 1:1000){
  evidence <-setEvidence(e.grain,nodes=c("A","T","L","B","E","X","D"),
                         states=as.character(unlist(test[i, -2])))
  prob <- querygrain(evidence)
  posterior_probs[i] <- prob$S["yes"]
}


# Classify based on the most likely class
pred <- ifelse(posterior_probs > 0.5, "yes", "no")
confusion_matrix <- table(Predicted = pred, Actual = test$S)

# Print the confusion matrix
confusion_matrix
sum(diag(confusion_matrix))/sum(confusion_matrix)
