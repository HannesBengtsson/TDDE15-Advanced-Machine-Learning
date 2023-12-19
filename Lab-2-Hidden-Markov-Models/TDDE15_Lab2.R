########################################
### Lab 2 - Hidden Markov Models
### By Hannes Bengtsson
### TDDE15 - Advanced Machine Learning
### Linköping University
########################################

# Install and import necessary packages
install.packages("HMM")
install.packages("entropy")
install.packages("tidyverse")
library(HMM)
library(entropy)
library(ggplot2)

############################################
################ EXERCISE 1 ###############
###########################################
# Model the behavior or a robot that walks in a ring divided in 10 sectors
# Use a Hidden Markov Model

# Actual states
states <- c("S1","S2","S3","S4","S5","S6","S7","S8","S9","S10")

# Observed states via the device
symbols <- c("S1","S2","S3","S4","S5","S6","S7","S8","S9","S10")

# Starting probabilities for the states
start_probs <- rep(0.1,10)

# 10x10 matrix with probability of movement
# Rows from, col to, i.e., trans_matrix[from,to], [row,col]
trans_probs <- matrix(data = c(0.5, 0.5, 0, 0, 0, 0, 0, 0, 0, 0,
                               0, 0.5, 0.5, 0, 0, 0, 0, 0, 0, 0,
                               0, 0, 0.5, 0.5, 0, 0, 0, 0, 0, 0,
                               0, 0, 0, 0.5, 0.5, 0, 0, 0, 0, 0,
                               0, 0, 0, 0, 0.5, 0.5, 0, 0, 0, 0,
                               0, 0, 0, 0, 0, 0.5, 0.5, 0, 0, 0,
                               0, 0, 0, 0, 0, 0, 0.5, 0.5, 0, 0,
                               0, 0, 0, 0, 0, 0, 0, 0.5, 0.5, 0,
                               0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0.5,
                               0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0.5),
                      nrow = 10, ncol = 10, byrow = TRUE, 
                      dimnames = list(states, states))

# 10x10 matrix with probability of device result
# Rows state, col emission 
# i.e., emssision_probs[Actual state, Emit state],[row,col]
emission_probs <- matrix(data = c(0.2, 0.2, 0.2, 0, 0, 0, 0, 0, 0.2, 0.2,
                                0.2, 0.2, 0.2, 0.2, 0, 0, 0, 0, 0, 0.2,
                                0.2, 0.2, 0.2, 0.2, 0.2, 0, 0, 0, 0, 0,
                                0, 0.2, 0.2, 0.2, 0.2, 0.2, 0, 0, 0, 0,
                                0, 0, 0.2, 0.2, 0.2, 0.2, 0.2, 0, 0, 0,
                                0, 0, 0, 0.2, 0.2, 0.2, 0.2, 0.2, 0, 0,
                                0, 0, 0, 0, 0.2, 0.2, 0.2, 0.2, 0.2, 0,
                                0, 0, 0, 0, 0, 0.2, 0.2, 0.2, 0.2, 0.2,
                                0.2, 0, 0, 0, 0, 0, 0.2, 0.2, 0.2, 0.2,
                                0.2, 0.2, 0, 0, 0, 0, 0, 0.2, 0.2, 0.2),
                       nrow = 10, ncol = 10, byrow = TRUE,
                       dimnames = list(states, symbols))

# Initiation of the Hidden Markov Model
robot <- initHMM(States = states, Symbols = symbols, startProbs = start_probs, 
        transProbs = trans_probs, emissionProbs = emission_probs)

############################################
################ EXERCISE 2 ###############
###########################################

# Robot simulations of 100 time steps
set.seed(12345)
robot_simulation <- simHMM(robot,100)
observations <- robot_simulation$observation

############################################
################ EXERCISE 3 ###############
###########################################

# Calculating the filtered probability distributions using the forward function
# Exp takes the logarithmic output from the forward function and convert them 
# into natural numbers
# Prob.table with margin 2 normalizes the filtered probability distributions 
# for each time step i.e., normalizes each column in the filtered matrix
# Using prop.table returns the same result as when looping over all columns 
# and taking each value in a column and divide it by the sum of all values 
# in that column (col if margin = 2)
filtered <- prop.table(exp(forward(robot, observations)),margin=2)

# To compute the smoothed probability distributions we can use the posterior function
# or we can combine the forward and backward functions, multiplying alpha and beta
# from the forward function alpha is obtained and from backward beta is obtained
smoothed <- posterior(robot,observations)
smoothed <- prop.table(exp(forward(robot, observations)) 
                       * exp(backward(robot,observations)),
                       margin=2)

# Most probable path using the Viterbi algorithm
most_prob_path <- viterbi(robot, observations)

############################################
################ EXERCISE 4 ###############
###########################################

# Obtain the state with the highest probability for each observation
filtered_pred <- rownames(filtered)[apply(filtered,2,which.max)]
smoothed_pred <- rownames(smoothed)[apply(smoothed,2,which.max)]

# Accuracy function
accuracy <- function(X,X1){
  n <- length(X)
  return(sum(diag(table(X,X1)))/n)
}

# Accuracy calculations
filtered_acc <- accuracy(robot_simulation$states,filtered_pred)
smoothed_acc <- accuracy(robot_simulation$states,smoothed_pred)
viterbi_acc <- accuracy(robot_simulation$states,most_prob_path)
data.frame(Filtered_Accuracy = filtered_acc,
           Smoothed_Accuracy = smoothed_acc,
           Viterbi_Accuaracy = viterbi_acc)

############################################
################ EXERCISE 5 ###############
###########################################

# Robot simulations of 100 time steps
set.seed(67890)
robot_simulation <- simHMM(robot,100)
observations <- robot_simulation$observation

# Calculating the filtered probability distributions using the forward function
filtered <- prop.table(exp(forward(robot, observations)),margin=2)

# Compute the smoothed probability distributions using the posterior function
smoothed <- posterior(robot,observations)

# Most probable path using the Viterbi algorithm
most_prob_path <- viterbi(robot, observations)

# Obtain the state with the highest probability for each observation
filtered_pred <- rownames(filtered)[apply(filtered,2,which.max)]
smoothed_pred <- rownames(smoothed)[apply(smoothed,2,which.max)]

# Accuracy calculations
filtered_acc <- accuracy(robot_simulation$states,filtered_pred)
smoothed_acc <- accuracy(robot_simulation$states,smoothed_pred)
viterbi_acc <- accuracy(robot_simulation$states,most_prob_path)
data.frame(Filtered_Accuracy = filtered_acc,
           Smoothed_Accuracy = smoothed_acc,
           Viterbi_Accuaracy = viterbi_acc)

############################################
################ EXERCISE 6 ###############
###########################################

# Computation of the entropy for the filtered distributions
e_filtered <-numeric(ncol(filtered))
for(i in 1:100){
  e_filtered[i] <-  entropy.empirical(filtered[,i])
}

# Save the entropy of the filtered distributions with the corresponding time step
data <- data.frame(entropy=e_filtered, observation=seq(1,100))

# Plot the entropy of the filtered distributions
ggplot(data = data)+
  geom_line(aes(y=entropy,x=observation))+
  ylab("Entropy") + xlab("Number of observations") + 
  labs(colour="Data type", title="Entropy of the filtered distributions")

############################################
################ EXERCISE 7 ###############
###########################################

# Probabilities for the 100th time step combined with 50/50 chance of moving
# If the trans_matrix contains NA instead of 0 we get NA in the matrix multiplication
# Therefore we need to use 0 when initiating the trans_matrix for the HMM
filtered[,100]
smoothed[,100] # Same probability distribution as filtered, see LE5
prob_101 <- as.vector(filtered[,100] %*% trans_matrix)

### Alternative initiation of HMM parameters ##

# Alternative way to initialize transition probabilities
# trans_matrix <- matrix(0, nrow = 10, ncol = 10)
# diag(trans_matrix) <- 0.5
# diag(trans_matrix[,-1]) <- 0.5
# trans_matrix[10,1] <- 0.5
# dimnames(trans_matrix) <- list(states, states)
# 
# # Alternative way to initialize emission probabilities
# device_matrix <- matrix(0, nrow = 10, ncol = 10)
# emit_prob <- rep(0.2,5)
# device_matrix[1,-4:-8] <- emit_prob
# device_matrix[2,-5:-9] <- emit_prob
# device_matrix[3,-6:-10] <- emit_prob
# device_matrix[4,2:6] <- emit_prob
# device_matrix[5,3:7] <- emit_prob
# device_matrix[6,4:8] <- emit_prob
# device_matrix[7,5:9] <- emit_prob
# device_matrix[8,6:10] <- emit_prob
# device_matrix[9,-2:-6] <- emit_prob
# device_matrix[10,-3:-7] <- emit_prob
# dimnames(device_matrix) <- list(states, states)
# # Rows state, col emission, i.e., trans_matrix[Actual state, Emit state], [row,col]
# device_probs <- matrix(device_matrix, nrow=10, ncol=10, byrow=FALSE,
#                        dimnames = list(states, symbols))
