sapply(c("stringr", "dplyr", "data.table", "purrr", "foreach", "doParallel", "ggplot2"), require, character=TRUE)
#registerDoParallel(cores=round(detectCores()*2/3))
# Params to potentially manipulate: 
# magnitude (like the SCZ studies)   
# probability, especially lucas-newman lab work and general idea worry = less precise prob est  

########################### Set Up Experiment ###########################
states <- c("ab", "cd", "ef", "gh")
every_stim <- c("a", "b", "c", "d", "e", "f", "g", "h")
better_state <- c("left", "left", "right", "right")
# Create a key to access every trial and output pos or neg reward with the appropriate probability  
key <- data.table("stim"=every_stim, 
                  "pos_or_neg"=c(1, 1, 1, 1, -1, -1, -1, -1) , 
                  "p_nz"=c(.9, .8, .2, .1, .9, .8, .2, .1)) # Probability of non-zero outcome
# To determine if made the optimal choice on a trial
state_key <- data.table(states, "better_choice"=better_state)
######################################################################

############ Create actor-critic simulation testbed  ##################
# Critic value matrix = just values for the 4 states
c_values <- rep(0, length(states))
AC_PE <- 0
critic_dynamics <- list("critic_values"=c_values, "AC_PE"=AC_PE) # Package up the critic's dynamics
# Action weights comprising weights for the two available actions in each of the 4 states
a_weights <- matrix(rep(0, length(states)*2), nrow=length(states))
q_vals <- matrix(rep(0, length(states)*2), nrow=length(states))
######################################################################
 
############# # Set up some learning and choice functions  ###########
## Learning ## 
UpdateCriticValue <- function(c_values, sidx, critic_LR, outcome, helpers, verbose=NULL) {
  ### Update the appropriate critic value based on the state we're in ###
  # Calc's a PE based on state value. Then passes this to actor to update its weights 
  # Args: sidx=state value
  c_values <- as.numeric(unlist(c_values))
  AC_PE <- as.numeric(outcome - c_values[sidx]) #as.numeric(unlist(c_values))[sidx]
  c_values[sidx] <- as.numeric(c_values[sidx] + critic_LR * AC_PE)
  if (!is.null(verbose)) cat("\n c_values", as.numeric(unlist(c_values))[sidx])
list("critic_values"=c_values, "AC_PE"=AC_PE)
} 
UpdateActorWeights <- function(a_weights, sidx, action, actor_LR, AC_PE, helpers) {
  ### Update the action weight only in the wake of a pos. PE. Args: sidx=state value ###
  # ** Check this.. If I understood from Geana paper this is only when PE is non-negative
  if (AC_PE > 0) a_weights[sidx, action] <- a_weights[sidx, action] + actor_LR * AC_PE
a_weights  
}
CalcQVals <- function(q_vals, q_LR, sidx, action, outcome, helpers) {
  ### Calc Q vals tracking full reward info. Args: sidx=state value ###
  qPE <- outcome - q_vals[sidx, action]
  q_vals[sidx, action] <- q_vals[sidx, action] + q_LR * qPE
q_vals
}

MixACAndQVals <- function(qv_row, aw_row, q_learner_prop, helpers) {
  ### Mix q values and AC values for this state outputting hybrid values ###
  mix_weight <- (1 - q_learner_prop) * aw_row + qv_row * q_learner_prop
  #browser()
mix_weight  
} 
# **Not implementing decay yet because just starting with training phase

## Choice ## 

CalcSoftmaxProbLeft <- function(values, beta, helpers) {
  ### Softmax choice fx. Outputs chance of picking left stimulus ###
exp(beta * values[1])/sum(exp(beta * values[1]), exp(beta * values[2]))  
}
MixLeftChoiceWRandom <- function(left_c_prob_sm, lapsiness, helpers) {
  ### Mix the left choice probability with nondirected random choice (reflecting lapsing) with 
  # contribution scaled by lapsines ###
(1 - lapsiness) * left_c_prob_sm + lapsiness * .5
}
######################################################################
RunATrainPhase <- function(states, key, state_key, helpers, verbose=NULL) {
  ### Run through one training phase ###
  ############# # Set up a training experiment  ###########
  # Unpack stuff we'll need from helpers 
  params <- helpers[["params"]]
  beta <- params[["beta"]]
  lapsiness <- params[["lapsiness"]]
  q_learner_prop <- params[["q_learner_prop"]]
  actor_LR <- params[["actor_LR"]] 
  q_LR <- params[["q_LR"]] 
  critic_LR <- params[["critic_LR"]]
  training_df <- helpers[["training_df"]]
  sim_or_opt <- helpers[["sim_or_opt"]]
  
  stim_set <- key$stim # Vectorize 
  trial_keeper <- list()
  
  ## Loop through trials ##
  for (tidx in seq_along(training_trials)) {
    sidx <- as.numeric(which(as.character(train_df$stimuli)[tidx]==states)) # State index
    state <- states[sidx] # Character repr of state 
    if (!is.null(verbose)) cat("\n ### Trial ", tidx, "####",
                               "\n ---State", state, "---")
    helpers[["trial_n"]] <- tidx
    ## Mix values and find choice probs ## 
    
    mix_values <- MixACAndQVals(q_vals[sidx, ], a_weights[sidx, ], q_learner_prop, helpers)
    left_prob_sm <- CalcSoftmaxProbLeft(mix_values, beta)
    # ** to do check choice probs 
    left_full_prob <- MixLeftChoiceWRandom(left_prob_sm, lapsiness)
    
    if (sim_or_opt==1) {
      ## Simulate choice.. 
      choice <- ifelse(left_full_prob < runif(1, 0, 1), "left", "right")
      action <- ifelse(choice=="left", 1, 2) # Just a numerical code for choice
      if (choice=="left") stim <- unlist(map(str_extract_all(state, boundary("character")), 1))
      if (choice=="right") stim <- unlist(map(str_extract_all(state, boundary("character")), 2))
      # Find if the choice was correct ie. optimal this trial 
      correct <- ifelse(choice==as.character(state_key[states==state, "better_choice"]), 1, 0)
      
      # .. and outcome ##
      row_idx <- which(stim_set==stim)
      p_nz <- key[row_idx, "p_nz"]
      pos_or_neg <- key[row_idx, "pos_or_neg"]
      outcome_str <- ifelse(p_nz < runif(1, 0, 1), "non_zero", "zero")
      # Outcomes are probabilistically 0 so check if non-zero..
      if (as.character(outcome_str)=="non_zero") {
        # If non-zero, assign correct / incorrect outcome 
        outcome <- ifelse(pos_or_neg==1, 1, -1) #.05, -.05) 
      } else { 
        outcome <- 0
      }
    }
    if (!is.null(verbose)) cat("\n Mix values", mix_values,
                               "\n Prob left choice", left_prob_sm,
                               "\n Mixed values ", unlist(mix_values),
                               "\n Full choice prob (softmax + undirected)", unlist(left_full_prob),
                               "\n Choose", choice,
                               "\n Correct?     ", ifelse(correct, "Yes!", "Nooope"),
                               "\n Probability of non-zero outcome", unlist(p_nz),
                               "\n Outcome", outcome)
    
    ## Learn based on result ## 
    # Critic who has RPEs just on state values.. 
    critic_out <- UpdateCriticValue(critic_dynamics["critic_values"], sidx, critic_LR, outcome)
    AC_PE <- unlist(critic_out["AC_PE"])
    # .. and actor who computes on s,a pairs but just has access to the critic's values 
    a_weights <- UpdateActorWeights(a_weights, sidx, action, actor_LR, AC_PE) 
    
    q_vals <- CalcQVals(q_vals, q_LR, sidx, action, outcome) 
    
    if (!is.null(verbose)) { cat("\n           A-C             |       Q v \n")
      write.table(format(cbind(a_weights, q_vals), justify="right"),
                  row.names=F, col.names=F, quote=F) }
    
    ## Package up outputs this trial ##
    trial_keeper[[tidx]] <- data.table(tidx,
                                       state,
                                       choice,
                                       "outcome"=as.numeric(outcome),
                                       correct)                                       
  }
  ######################################################################
  output <- trial_keeper %>% bind_rows()
output  
}
## Initializations ##
n_trials <- 400
helpers <- list() # List of stuff we'll need to shuttle around fxs 
params <- list() # Free pars
params[["beta"]] <- 20
params[["lapsiness"]] <- .025
params[["q_learner_prop"]] <- .9
params[["actor_LR"]] <- .1
params[["q_LR"]] <- .01
params[["critic_LR"]] <- .1
helpers[["params"]] <- params
helpers[["sim_or_opt"]] <- 1 # Simulate (1) or optimize (2)? (opt not yet built)
training_trials <- unlist(lapply(1:n_trials, function(x) sample(states, 1))) # 1 training phase
train_df <- data.frame("trial"=1:n_trials, "stimuli"=training_trials)
helpers[["train_df"]] <- train_df

# More settings
parallelize <- 1
verbose <- 1 # Trial-wise print out?

iters <- 80
if (parallelize) {
  out <- foreach(i=1:iters) %dopar% RunATrainPhase(states, key, state_key, helpers, 1)   
} else {
  out <- foreach(i=1:iters) %do% RunATrainPhase(states, key, state_key, helpers, 1)   
}

for (iter in 1:iters) out[[iter]]$iter <- iter # Label iteration
out_dt <- do.call(rbind, out)

# Not improving over time so still seems to be a bug
summs <- out_dt %>% group_by(tidx) %>% summarize(m=mean(correct))# %>% tail(50)
ggplot(summs, aes(x=tidx, y=m)) + geom_line() #+ facet_wrap(~ iter)

