# The objective of this script is to generate simulated data for an incoming experiment. Let's start to draw a data set that is going be used at the end
# of the script to analyse with a statistical test. 

# ID of the participants (repeated twice because there will be two observations)
id=factor(rep(1:1000, each=2))

# Now the two treatments
treat <- factor(rep(c("a","s"),times=2))

# Now we create the data set
datasocial <- data.frame(id,treat)

# Some of the participants will be immigrants - let's say 25%. Let's assign the dispersal status to the participants
datasocial$dispersal <- sample(c("n", "d"), length(levels(datasocial$id)), prob = c(0.75, 0.25), T)[as.numeric(datasocial$id)]

# Same for sex of the participants
datasocial$sex <- sample(c("f","m"), length(levels(datasocial$id)), prob = c(0.50,0.50), T)[as.numeric(datasocial$id)]

# Now these participants will play the game. Let's assume that each treatment has 10 rounds and we want to record the outcome of each round.
library(data.table)
library(splitstackshape)
dataraw <- expandRows(datasocial, count=10, count.is.col=FALSE)


# We want native and dispersers to play socially with a certain probability, so let's create a column that defines such probability.
for (i in 1:20000) {
  dataraw$dispersal_prob[[i]] <- if (dataraw$dispersal[[i]]=="n") {
    0.90
  } else {0.85}
}

# Same for sex.
for (i in 1:20000) {
  dataraw$sex_prob[[i]] <- if (dataraw$sex[[i]]=="f") {
    0.87
  } else {0.90}
}

# In the first 10 rounds - the Asocial game - individuals can decide for option A (0) or for option B (1). Option B starts at value 0.55$ with 0.25 probability
# and the value has increments of 0.05. Option A has value 0.50$ with 1 probability. I made the calculations for the optimal behaviour in a separate document
# so that the script looks less cumbersome. Here we are assuming that the individuals behave optimally. In reality, individuals are likely to take a riskier
# option more often. This might mean that the difference between treatments isn't that big, if at all. But we are more interested in how being native/disperser
# and women/men affects the decision to act socially, so this shouldn't be a problem.
dataraw$sample_asocial_prob1 = rep(c(1-0.22,1-0.23,1-0.25,1-0.26,1-0.27,1-0.29,1-0.30,1-0.31,1-0.32,1-0.33,0,0,0,0,0,0,0,0,0,0),times=1000)
dataraw$sample_asocial_prob2 = rep(c(0.22,0.23,0.25,0.26,0.27,0.29,0.30,0.31,0.32,0.33,0,0,0,0,0,0,0,0,0,0),times=1000)

# Now the probability of NOT acting socially (value 0) in the 10 rounds of the Social game is one minus the probability of acting socially given the dispersal status times
# the probability of acting socially given the sex. Accordingly, the probability of acting socially (value 1) in the 10 rounds of the social game is the probability 
# of acting socially given the dispersal status times the probability of acting socially given the sex. There are some constraints to act socially - participants needs to
# act both asocially and socially at least once. Furthermore, behaving socially can incur a cost if the other play doesn't play socially. This ensures that participants 
# are not compelled to act socially every time (given that this action has a better payoff)
for (i in 1:20000) {
  dataraw$sample_social_prob1[[i]] <- if (dataraw$treat[[i]]=="a") {
    0
  } else {1 - dataraw$dispersal_prob[[i]]*dataraw$sex_prob[[i]]}
}
for (i in 1:20000) {
  dataraw$sample_social_prob2[[i]] <- if (dataraw$treat[[i]]=="a") {
    0
  } else {dataraw$dispersal_prob[[i]]*dataraw$sex_prob[[i]]}
}

#Now we make them play the game.
for (i in 1:20000) {
  dataraw$risk_sensitivity[[i]] <- if (dataraw$treat[[i]]=="a") {
    sample(c(0, 1), prob = c(dataraw$sample_asocial_prob1[[i]],dataraw$sample_asocial_prob2[[i]]), T)}
  else {
    sample(c(0, 1), prob = c(dataraw$sample_social_prob1[[i]],dataraw$sample_social_prob2[[i]]), T)
  }}

# Now we want to move these results back to the original data set to be analysed.
result <- aggregate(dataraw$risk_sensitivity,by=list(dataraw$id,dataraw$treat),sum)
result <- result[order(result$Group.1) , ]
datasocial$risk_sensitivity <- result$x 

# To make sure that no participants are always behaving asocially or socially, let's convert every 10 to 9 and 0 to 1 in the Social treatment
for (i in 1:2000) {
  datasocial$risk_sensitivity[[i]] <- if (datasocial$treat[[i]]=="s" & datasocial$risk_sensitivity[[i]]==10) {
    9
  } else {datasocial$risk_sensitivity[[i]]}
}
for (i in 1:2000) {
  datasocial$risk_sensitivity[[i]] <- if (datasocial$treat[[i]]=="s" & datasocial$risk_sensitivity[[i]]==0) {
    1
  } else {datasocial$risk_sensitivity[[i]]}
}

#Now let's start to analyse the data
library(ordinal)
datasocial$risk_sensitivity <- as.factor(datasocial$risk_sensitivity)
test <- clmm2(risk_sensitivity ~ treat + sex + dispersal + sex*dispersal + treat*dispersal + treat*sex,random = id, data = datasocial, Hess=TRUE)
summary(test)

# Now let's create a function that does all of this in one step.
library(magrittr)
library(dplyr)  
library(data.table)
library(splitstackshape)
library(ordinal)
library(broom.mixed)
library(purrr)
library(ggplot2)

sim_experiment <- function ( n_sample = 1000,
                             n_sample20 = n_sample*20,
                             n_sample2 = n_sample*2,
                             prop_disp = 0.25,
                             prop_fem = 0.50,
                             disp_probability = 0.85,
                             nondisp_probability = 0.90,
                             fem_probability = 0.87,
                             mal_probability = 0.90) {

  id=factor(rep(1:n_sample, each=2))
  treat <- factor(rep(c("a","s"),times=2))
  datasocial <- data.frame(id,treat)
  datasocial$dispersal <- sample(c("n", "d"), length(levels(datasocial$id)), prob = c(1-prop_disp, prop_disp), T)[as.numeric(datasocial$id)]
  datasocial$sex <- sample(c("f","m"), length(levels(datasocial$id)), prob = c(prop_fem,1-prop_fem), T)[as.numeric(datasocial$id)]
  dataraw <- expandRows(datasocial, count=10, count.is.col=FALSE)
  for (i in 1:n_sample20) {
    dataraw$dispersal_prob[[i]] <- if (dataraw$dispersal[[i]]=="n") {
      nondisp_probability
    } else {disp_probability}
  }
  for (i in 1:n_sample20) {
    dataraw$sex_prob[[i]] <- if (dataraw$sex[[i]]=="f") {
      fem_probability
    } else {mal_probability}
  }
  dataraw$sample_asocial_prob1 = rep(c(1-0.22,1-0.23,1-0.25,1-0.26,1-0.27,1-0.29,1-0.30,1-0.31,1-0.32,1-0.33,0,0,0,0,0,0,0,0,0,0),times=n_sample)
  dataraw$sample_asocial_prob2 = rep(c(0.22,0.23,0.25,0.26,0.27,0.29,0.30,0.31,0.32,0.33,0,0,0,0,0,0,0,0,0,0),times=n_sample)
  for (i in 1:n_sample20) {
    dataraw$sample_social_prob1[[i]] <- if (dataraw$treat[[i]]=="a") {
      0
    } else {1 - dataraw$dispersal_prob[[i]]*dataraw$sex_prob[[i]]}
  }
  for (i in 1:n_sample20) {
    dataraw$sample_social_prob2[[i]] <- if (dataraw$treat[[i]]=="a") {
      0
    } else {dataraw$dispersal_prob[[i]]*dataraw$sex_prob[[i]]}
  }
  for (i in 1:n_sample20) {
    dataraw$risk_sensitivity[[i]] <- if (dataraw$treat[[i]]=="a") {
      sample(c(0, 1), prob = c(dataraw$sample_asocial_prob1[[i]],dataraw$sample_asocial_prob2[[i]]), T)}
    else {
      sample(c(0, 1), prob = c(dataraw$sample_social_prob1[[i]],dataraw$sample_social_prob2[[i]]), T)
    }}
  result <- aggregate(dataraw$risk_sensitivity,by=list(dataraw$id,dataraw$treat),sum)
  result <- result[order(result$Group.1) , ]
  datasocial$risk_sensitivity <- result$x 
  for (i in 1:n_sample2) {
    datasocial$risk_sensitivity[[i]] <- if (datasocial$treat[[i]]=="s" & datasocial$risk_sensitivity[[i]]==10) {
      9
    } else {datasocial$risk_sensitivity[[i]]}
  }
  for (i in 1:n_sample2) {
    datasocial$risk_sensitivity[[i]] <- if (datasocial$treat[[i]]=="s" & datasocial$risk_sensitivity[[i]]==0) {
      1
    } else {datasocial$risk_sensitivity[[i]]}
  }
  datasocial$risk_sensitivity <- as.factor(datasocial$risk_sensitivity)
  test <- clmm2(risk_sensitivity ~ treat + sex + dispersal + sex*dispersal + treat*dispersal + treat*sex,random = id, data = datasocial, Hess=TRUE)
  return(test)
}

# Let's run the simulation with the default values

sim_experiment() %>% summary()

# Now with different values
sim_experiment(n_sample = 2000,
               prop_disp = 0.10,
               prop_fem = 0.35,
               disp_probability = 0.75,
               nondisp_probability = 0.90,
               fem_probability = 0.75,
               mal_probability = 0.90) %>% summary()

# Let's do the power analysis now

tidy_output_clmm = function(fit){
  results = as.data.frame(coefficients(summary(fit)))
  colnames(results) = c("estimate","std.error","statistic","p.value")
  results %>% tibble::rownames_to_column("term")
}

sim_experiment_power <- function(rep) {
  s <- sim_experiment(n_sample = 1000,
                      prop_disp = 0.35,
                      prop_fem = 0.50,
                      disp_probability = 0.75,
                      nondisp_probability = 0.90,
                      fem_probability = 0.75,
                      mal_probability = 0.90)
  tidy_output_clmm(s) %>% mutate(rep=rep)
}

my_power <- map_df(1:10, sim_experiment_power)

ggplot(my_power, aes(estimate, color = term)) +
  geom_density() +
  facet_wrap(~term, scales = "free")

my_power %>% group_by(term) %>% summarise(power = mean(p.value < 0.05))









