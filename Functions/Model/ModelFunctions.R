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
########################### HEADING ###########################
###################################################################### Iterate through trials ##
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
  n_trials <- helpers[["n_trials"]]
  #training_df <- helpers[["training_df"]]
  training_trials <- unlist(lapply(1:n_trials, function(x) sample(states, 1))) # 1 training phase
  sim_or_opt <- helpers[["sim_or_opt"]]
  
  stim_set <- key$stim # Vectorize 
  trial_keeper <- list()
  
  ## Loop through trials ##
  for (tidx in 1:n_trials) {
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
        #**Setting these arbitrarily, need to check on what's done. Also will need to change for 
        # magnitude manip. 
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