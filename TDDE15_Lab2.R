# Install and import necessary packages
install.packages("HMM")
library(HMM)

install.packages("entropy")
library(entropy)

install.packages("tidyverse")
library(ggplot2)
############################################
################ EXERCISE 1 ###############
###########################################

# Actual states
states <- c("S1","S2","S3","S4","S5","S6","S7","S8","S9","S10")

# Observed states via the device
symbols <- c("S1","S2","S3","S4","S5","S6","S7","S8","S9","S10")

# Starting probabilities for the states
start_probs <- rep(0.1,10)

# Create a 10x10 matrix with probability of movement
trans_matrix <- matrix(0, nrow = 10, ncol = 10)
diag(trans_matrix) <- 0.5
diag(trans_matrix[,-1]) <- 0.5
trans_matrix[10,1] <- 0.5

# Rows from, col to, i.e., trans_matrix[from,to], [row,col]
trans_matrix
trans_probs <- matrix(trans_matrix, nrow=10, ncol=10, byrow=FALSE,
                      dimnames = list(states,
                                      states))

# Create a 10x10 matrix with probability of device result
device_matrix <- matrix(0, nrow = 10, ncol = 10)
emit_prob <- rep(0.2,5)
device_matrix[1,-4:-8] <- emit_prob
device_matrix[2,-5:-9] <- emit_prob
device_matrix[3,-6:-10] <- emit_prob
device_matrix[4,2:6] <- emit_prob
device_matrix[5,3:7] <- emit_prob
device_matrix[6,4:8] <- emit_prob
device_matrix[7,5:9] <- emit_prob
device_matrix[8,6:10] <- emit_prob
device_matrix[9,-2:-6] <- emit_prob
device_matrix[10,-3:-7] <- emit_prob

# Rows state, col emission, i.e., trans_matrix[Actual state, Emit state], [row,col]
device_probs <- matrix(device_matrix, nrow=10, ncol=10, byrow=FALSE,
                       dimnames = list(states,
                                       states))

# Initiation of the Hidden Markov Model
robot <- initHMM(States = states, Symbols = symbols, startProbs = start_probs, 
        transProbs = trans_probs, emissionProbs = device_probs)

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

# Calculation of alpha and beta values
alpha <- exp(forward(robot, observations))
beta <- exp(backward(robot, observations))

# Functions for calculating the filtered probability distributions
filtering <- function(alpha){
  f <- matrix(nrow=dim(alpha)[1], ncol=dim(alpha)[2], 
              dimnames = list(states,seq(from=1,to=100)))
  for(i in 1:ncol(f)){
    f[,i] <-  alpha[,i]/sum(alpha[,i])
  }
  return(prop.table(f, margin=2))
}

# Functions for calculating the smoothed probability distributions
smoothing <- function(alpha,beta){
  s <- matrix(nrow=dim(alpha)[1], ncol=dim(alpha)[2], 
              dimnames = list(states,seq(from=1,to=100)))
  for(i in 1:ncol(s)){
    s[,i] <-  alpha[,i]*beta[,i]/sum(alpha[,i]*beta[,i])
  }
  return(prop.table(s, margin=2))
}

# Calculation of probabilities
filtered <- filtering(alpha)
smoothed <- smoothing(alpha,beta)

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

############################################
################ EXERCISE 5 ###############
###########################################

# Robot simulations of 100 time steps
set.seed(67890)
robot_simulation <- simHMM(robot,100)
observations <- robot_simulation$observation

# Calculation of alpha and beta values
alpha <- exp(forward(robot, observations))
beta <- exp(backward(robot, observations))


# Calculation of probabilities
filtered <- filtering(alpha)
smoothed <- smoothing(alpha,beta)

# Most probable path using the Viterbi algorithm
most_prob_path <- viterbi(robot, observations)

# Obtain the state with the highest probability for each observation
filtered_pred <- rownames(filtered)[apply(filtered,2,which.max)]
smoothed_pred <- rownames(smoothed)[apply(smoothed,2,which.max)]

# Accuracy calculations
filtered_acc <- accuracy(robot_simulation$states,filtered_pred)
smoothed_acc <- accuracy(robot_simulation$states,smoothed_pred)
viterbi_acc <- accuracy(robot_simulation$states,most_prob_path)

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
smoothed[,100]
prob_101 <- as.vector(filtered[,100] %*% trans_matrix)

