############################################
################ EXERCISE 1 ###############
###########################################

install.packages("bnlearn")
library(bnlearn)

data("asia")

# Initiate hill climb
hc1 <- hc(asia, score="bic")
hc2 <- hc(asia, score="aic")

install.packages("Rgraphviz")
library(Rgraphviz)

# Plotting the graphs
graphviz.plot(hc1)
graphviz.plot(hc2)
graphviz.plot(cpdag(hc1))
graphviz.plot(cpdag(hc2))

# Checking the structure
vstructs(hc1)
vstructs(hc2)
arcs(hc1)
arcs(hc2)

all.equal(hc1,hc2)
graphviz.compare(cpdag(hc1), cpdag(hc2))

############################################
################ EXERCISE 2 ###############
###########################################

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

# Learn the structure using Hill Climbing algorithm
bn_structure <- hc(train)
graphviz.plot(bn_structure)

# Estimate parameters of the Bayesian Network
bn_parameters <- bn.fit(bn_structure, train, method="mle")
bn_grain <- as.grain(bn_parameters)

# Initialize an empty vector to store posterior probabilities
posterior_probs <-numeric(nrow(test))

# Find the posterior probabilities
for(i in 1:1000){
  evidence <-setEvidence(bn_grain,nodes=c("A","T","L","B","E","X","D"),states=as.character(unlist(test[i, -2])))
  prob <- querygrain(evidence)
  posterior_probs[i] <- prob$S["yes"]
}

# Classify based on the most likely class
predicted_classes <- ifelse(posterior_probs > 0.5, "yes", "no")

results <- data.frame(ground_truth = test$S, predicted = predicted_classes)
confusion_matrix <- table(results$predicted,results$ground_truth)

# Calculate true/false positives/negatives
true_positive <- confusion_matrix["yes", "yes"]
false_positive <- confusion_matrix["no", "yes"]
true_negative <- confusion_matrix["no", "no"]
false_negative <- confusion_matrix["yes", "no"]

# Print the confusion matrix
confusion_matrix

dag <- model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")

graphviz.plot(dag)
graphviz.compare(cpdag(dag), cpdag(bn_structure))

dag.grain <- as.grain(bn.fit(dag,train))

posterior_probs <-numeric(nrow(test))

for(i in 1:1000){
  evidence <-setEvidence(dag.grain,nodes=c("A","T","L","B","E","X","D"),states=as.character(unlist(test[i, -2])))
  prob <- querygrain(evidence)
  posterior_probs[i] <- prob$S["yes"]
}

# Classify based on the most likely class
predicted_classes <- ifelse(posterior_probs > 0.5, "yes", "no")

# Create a data frame with ground truth and predicted values
results <- data.frame(ground_truth = test$S, predicted = predicted_classes)

# Calculate the confusion matrix
confusion_matrix <- table(results$predicted,results$ground_truth)

# Calculate true/false positives/negatives
true_positive <- confusion_matrix["yes", "yes"]
false_positive <- confusion_matrix["no", "yes"]
true_negative <- confusion_matrix["no", "no"]
false_negative <- confusion_matrix["yes", "no"]

# Print the confusion matrix
confusion_matrix

############################################
################ EXERCISE 3 ###############
###########################################

S_mb <- mb(bn_structure, node="S")

bn_structure <- hc(train)
bn_parameters <- bn.fit(bn_structure, train, method="mle")
bn_grain <- as.grain(bn_parameters)

filtered_test <- test[4:5]

posterior_probs <-numeric(nrow(filtered_test))

for(i in 1:1000){
  evidence <-setEvidence(bn_grain,nodes=c(S_mb),states=as.character(unlist(filtered_test[i,])))
  prob <- querygrain(evidence)
  posterior_probs[i] <- prob$S["yes"]
}

predicted_classes <- ifelse(posterior_probs > 0.5, "yes", "no")

results <- data.frame(ground_truth = test$S, predicted = predicted_classes)

confusion_matrix <- table(results$predicted ,results$ground_truth)

true_positive <- confusion_matrix["yes", "yes"]
false_positive <- confusion_matrix["no", "yes"]
true_negative <- confusion_matrix["no", "no"]
false_negative <- confusion_matrix["yes", "no"]

# Print the confusion matrix
confusion_matrix

############################################
################ EXERCISE 4 ###############
###########################################

e <-  empty.graph(c("A","S","T","L","B","E","X","D"))
class(e)
graphviz.plot(e)
arc <-matrix(c("S", "A", "S", "T", "S", "L", "S", "B","S", "E", "S", "X", "S", "D"), 
                 ncol = 2, byrow = TRUE,
                 dimnames = list(NULL, c("from", "to")))

arcs(e) <- arc
graphviz.plot(e)

e.grain <- as.grain(bn.fit(e,train))

posterior_probs <-numeric(nrow(test))

for(i in 1:1000){
  evidence <-setEvidence(e.grain,nodes=c("A","T","L","B","E","X","D"),states=as.character(unlist(test[i, -2])))
  prob <- querygrain(evidence)
  posterior_probs[i] <- prob$S["yes"]
}

# Classify based on the most likely class
predicted_classes <- ifelse(posterior_probs > 0.5, "yes", "no")

# Create a data frame with ground truth and predicted values
results <- data.frame(ground_truth = test$S, predicted = predicted_classes)

# Calculate the confusion matrix
confusion_matrix <- table(results$predicted,results$ground_truth)

# Calculate true/false positives/negatives
true_positive <- confusion_matrix["yes", "yes"]
false_positive <- confusion_matrix["no", "yes"]
true_negative <- confusion_matrix["no", "no"]
false_negative <- confusion_matrix["yes", "no"]

# Print the confusion matrix
confusion_matrix