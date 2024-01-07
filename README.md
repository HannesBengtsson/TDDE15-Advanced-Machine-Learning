# 🤖 TDDE15 – Advanced Machine Learning

TDDE15 is a Machine Learning course taught at Linköping University, Sweden. I attended it during the fall of 2023. The course cover four main topics that all have one lab associated with it: Graphical Models, Hidden Markov Models, Reinforcement learning, and Gaussian Process Regression and Classification. Please see further details for each topic below. 

---

### [Lab 1](https://github.com/HannesBengtsson/TDDE15-Advanced-Machine-Learning/tree/main/Lab-1-Graphical-Models) – Graphical Models

A summary of the lab is presented below, but for further details, please checkout my [lab report](https://github.com/HannesBengtsson/TDDE15-Advanced-Machine-Learning/blob/main/Lab-1-Graphical-Models/Lab1_Notes.pdf), [implementation](https://github.com/HannesBengtsson/TDDE15-Advanced-Machine-Learning/blob/main/Lab-1-Graphical-Models/TDDE15_Lab1.R), and the [lab instructions in full](https://github.com/HannesBengtsson/TDDE15-Advanced-Machine-Learning/blob/main/Lab-1-Graphical-Models/Lab1_Description.pdf).

**Exercise 1 – Hill-Climbing Algorithm:** Explored the hill-climbing algorithm showing that it can generate non-equivalent Bayesian network structures and explained why this happens.

**Exercise 2 – Learn a BN:** Learned a Bayesian network from 80% of the dataset, including structure and parameters. Used the learned network to classify the remaining 20% of the data into two classes. Reported the confusion matrix and compared my results to the true BN.

**Exercise 3 – Classify based on the Markov Blanket:** Classified the target variable based only on its Markov blanket and reported the confusion matrix.

**Exercise 4 – Naive Bayes Classifier:** Repeated the classification from exercise 2 using a manually created naive Bayes classifier as a Bayesian network.

#

### [Lab 2](https://github.com/HannesBengtsson/TDDE15-Advanced-Machine-Learning/tree/main/Lab-2-Hidden-Markov-Models) – Hidden Markov Models

A summary of the lab is presented below, but for further details, please checkout my [lab report](https://github.com/HannesBengtsson/TDDE15-Advanced-Machine-Learning/blob/main/Lab-2-Hidden-Markov-Models/Lab2_Notes.pdf), [implementation](https://github.com/HannesBengtsson/TDDE15-Advanced-Machine-Learning/blob/main/Lab-2-Hidden-Markov-Models/TDDE15_Lab2.R), and the [lab instructions in full](https://github.com/HannesBengtsson/TDDE15-Advanced-Machine-Learning/blob/main/Lab-2-Hidden-Markov-Models/Lab2_Description.pdf).

**Exercise 1 – Build a Hidden Markov Model (HMM):** I built a Hidden Markov Model (HMM) to model a robot moving around a ring divided into 10 sectors.

**Exercise 2 – Simulate the HMM:** Simulated the HMM for 100 time steps, representing the robot's movements and observations.

**Exercise 3 – Filtering, smoothing, and most probable path:** The observations from the simulation was used to compute the filtered and smoothed probability distributions, as well as the most probable path. 

**Exercise 4 – Accuracy evaluation:** Compared the accuracy of the filtered and smoothed probability distributions, as well as the most probable path.

**Exercise 5 – Repeat exercise 2-4:** Repeated exercise 2-4 using different simulated samples. Comparing the results, the smoothed distributions were more accurate than the other prediction methods. 

**Exercise 6 – Assess knowledge gain over time:** Explored whether more observations led to better knowledge of the robot's location by calculating the entropy of the filtered distributions.

**Exercise 7 – Predict hidden states beyond the observed data:** Predicted the probabilities of the hidden states for time step 101.

#

### [Lab 3](https://github.com/HannesBengtsson/TDDE15-Advanced-Machine-Learning/tree/main/Lab-3-Reinforcement-Learning) – Reinforcement Learning

A summary of the lab is presented below, but for further details, please checkout my [lab report](https://github.com/HannesBengtsson/TDDE15-Advanced-Machine-Learning/blob/main/Lab-3-Reinforcement-Learning/Lab3_Notes.pdf), [implementation](https://github.com/HannesBengtsson/TDDE15-Advanced-Machine-Learning/blob/main/Lab-3-Reinforcement-Learning/TDDE15_Lab3.R), and the [lab instructions in full](https://github.com/HannesBengtsson/TDDE15-Advanced-Machine-Learning/blob/main/Lab-3-Reinforcement-Learning/Lab3_Description.pdf).

**Exercise 1 – Q-Learning algorithm implementation:** Implemented Q-Learning in a grid-world environment. 

**Exercise 2 – Q-Learning parameter analysis:** Explored Q-Learning's progress, policy optimality, and handling of multiple reward paths. Further I investigated how the ε, β, and γ parameters affected the learned policy. 

**Exercise 3 – REINFORCE algorithm analysis:** Explored the REINFORCE algorithm within grid-world environments, focusing on the agent's ability to reach random goal positions. Evaluated the learned policies for various goal positions, both within and beyond the training set, to assess the algorithm's performance.

#

### [Lab 4](https://github.com/HannesBengtsson/TDDE15-Advanced-Machine-Learning/tree/main/Lab-4-Gaussian-Processes) – Gaussian Processes

A summary of the lab is presented below, but for further details, please checkout my [lab report](https://github.com/HannesBengtsson/TDDE15-Advanced-Machine-Learning/blob/main/Lab-4-Gaussian-Processes/TDDE15-Lab4-GP.pdf), [implementation](https://github.com/HannesBengtsson/TDDE15-Advanced-Machine-Learning/blob/main/Lab-4-Gaussian-Processes/TDDE15_Lab4.R), and the [lab instructions in full](https://github.com/HannesBengtsson/TDDE15-Advanced-Machine-Learning/blob/main/Lab-4-Gaussian-Processes/TDDE15-Lab4-GP.pdf).

**Exercise 1 – GP regression implementation:** Implemented an algorithm simulating the posterior mean and variance.

**Exercise 2-4 – Plot and compare posteriors:** Plotted different prior observations together with their posterior mean and variance. 

**Exercise 5 – Update hyperparameters:** Repeated exercise 4 with updated hyperparameters. 

**Exercise 6 – GP regression with kernlab:** Implemented a square exponential kernel and familarized with the kernlab package in R. 

**Exercise 7 – Predicting temperatures:** Predicted temperatures in Tullinge as a function of time from the first observation using a GP.

**Exercise 8 – Plot findings:** Plotted the temperature observations together with posterior mean and 95% probability (pointwise) bands. 

**Exercise 9 – Predicting temperatures:** Predicted temperatures in Tullinge as a function of the day of the year using a GP and repeated exercise 8 for this new model.

**Exercise 10 – Logically periodic kernel:** Implemented a logically periodic kernel, repeated exercise 7-8, and discussed the results. 

**Exercise 11 – GP classification:** Classified bank fraud using a GP classification model, plotted countours of the prediction probabilities, and analyzed performance. 

**Exercise 12 – Extend previous model:** Increased the number of features for our GP classification model and compared the models performance. 
