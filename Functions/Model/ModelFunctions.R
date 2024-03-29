############# # Set up some learning and choice functions  ###########
## Learning ## 
UpdateCriticValue <- function(c_values, sidx, critic_LR, outcome, helpers, verbose=NULL) {
  ### Update the appropriate critic value based on the state we're in ###
  # Calc's a PE based on state value. Then passes this to actor to update its weights 
  # Args: sidx=state value
  c_values <- as.numeric(unlist(c_values))
  outcome <- as.numeric(outcome)
  
  AC_PE <- as.numeric(as.numeric(outcome) - c_values[sidx]) #as.numeric(unlist(c_values))[sidx]
  c_values[sidx] <- as.numeric(c_values[sidx] + critic_LR * AC_PE)
  if (verbose)  {
    cat("\n Critic value", as.numeric(unlist(c_values))[sidx])
    cat("\n Critic PE", AC_PE)  
  }
  
list("critic_values"=c_values, "AC_PE"=AC_PE)
} 
UpdateActorWeights <- function(a_weights, sidx, action, actor_LR, AC_PE, helpers) {
  ### Update the action weight only in the wake of a pos. PE. Args: sidx=state value ###
  # ** Check this.. If I understood from Geana paper this is only when PE is positive
  # * Also, notes this should grow without bound unless these are normalized, which is 
  # not yet implemented. May be needed in optimization.
  if (AC_PE > 0) a_weights[sidx, action] <- a_weights[sidx, action] + actor_LR * AC_PE
a_weights  
}
CalcQVals <- function(q_vals, q_LR, sidx, action, outcome, helpers) {
  ### Calc Q vals tracking full reward info. Args: sidx=state value ###
  qPE <- outcome - q_vals[sidx, action]
  #cat("\n Q PE", qPE)
  q_vals[sidx, action] <- q_vals[sidx, action] + q_LR * qPE
q_vals
}
MixACAndQVals <- function(qv_row, aw_row, q_learner_prop, helpers) {
  ### Mix q values and AC values for this state outputting hybrid values ###
  mix_weight <- (1 - q_learner_prop) * aw_row + qv_row * q_learner_prop
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
  #a_weights <- rep(0, 8)
  
  stim_set <- key$stim # Vectorize 
  trial_keeper <- list()
  
  # Critic value matrix = just values for the 4 states
  critic_values <- rep(0, length(states))
  AC_PE <- 0
  #critic_dynamics <- list("critic_values"=c_values, "AC_PE"=AC_PE) # Package up the critic's dynamics
  # Action weights comprising weights for the two available actions in each of the 4 states
  a_weights <- matrix(rep(0, length(states)*2), nrow=length(states))
  q_vals <- matrix(rep(0, length(states)*2), nrow=length(states))
  
  ## Loop through trials ##
  for (tidx in 1:n_trials) {
    sidx <- as.numeric(which(as.character(training_trials)[tidx]==states)) # State index
    state <- states[sidx] # Character repr of state 
    if (verbose) cat("\n ### Trial ", tidx, "####",
                               "\n ---State", state, "---")
    helpers[["trial_n"]] <- tidx
    
    ## Mix values and find choice probs ## 
    mix_values <- MixACAndQVals(q_vals[sidx, ], a_weights[sidx, ], q_learner_prop, helpers)
    left_prob_sm <- CalcSoftmaxProbLeft(mix_values, beta)
    # ** to do check choice probs 
    left_full_prob <- MixLeftChoiceWRandom(left_prob_sm, lapsiness)
    
    if (sim_or_opt==1) {
      ## Simulate choice.. 
      rd <- runif(1, 0, 1)
      if (rd < left_full_prob) { choice <- "left"  } else { choice <- "right" }
      
      #if (choice == "left") {action <- 1} else {action <- 2}
      
      if (choice=="left") {
        # Pull the left part of character to get the stim (eg. "a")
        stim <- unlist(map(str_extract_all(state, boundary("character")), 1))
        # Action index
        action <- 1
      }
      if (choice=="right") {
        stim <- unlist(map(str_extract_all(state, boundary("character")), 2))
        action <- 2
      }
      # Find if the choice was correct ie. optimal this trial 
      
      if (choice==as.character(state_key[states==state, "better_choice"])) { 
        correct <- 1
      } else { 
        correct <- 0
      }
      
      # .. and outcome ##
      row_idx <- which(stim_set==stim)
      p_nz <- as.numeric(key[row_idx, "p_nz"])
      pos_or_neg <- key[row_idx, "pos_or_neg"]
      rd2 <- runif(1, 0, 1)
      if (rd2 < p_nz) {
        outcome_str <- "non_zero"
      } else {
        outcome_str <- "zero"
      }
      # Outcomes are probabilistically 0 so check if non-zero..
      if (as.character(outcome_str)=="non_zero") {
        # If non-zero, assign correct / incorrect outcome 
        #**Setting these arbitrarily, need to check on what's done. Also will need to change for 
        # magnitude manip. 
        if (pos_or_neg==1) {
          outcome <- 1
        } else {
          outcome <- -1
        }
      } else { 
        outcome <- 0
      }
    }
    if (verbose) cat("\n Mix values", mix_values,
                               "\n Prob left choice", left_prob_sm,
                               "\n Mixed values ", unlist(mix_values),
                               "\n Probability of choosing left (softmax + undirected)", unlist(left_full_prob),
                               "\n Choose", choice,
                               "\n Correct?     ", ifelse(correct, "Yes!", "Nooope"),
                               "\n Probability of non-zero outcome", unlist(p_nz),
                               "\n Outcome", outcome)
    ## Learn based on result ## 
    # Critic who has RPEs just on state values.. 
    critic_out <- UpdateCriticValue(critic_values, sidx, critic_LR, outcome, helpers, verbose)
    critic_values <- unlist(critic_out[["critic_values"]])
    AC_PE <- unlist(critic_out[["AC_PE"]])
    # .. and actor who computes on s,a pairs but just has access to the critic's values 
    a_weights <- UpdateActorWeights(a_weights, sidx, action, actor_LR, AC_PE) 
    
    q_vals <- CalcQVals(q_vals, q_LR, sidx, action, outcome) 
    if (verbose) { cat("\n           A-C             |       Q v \n")
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
CalcSoftmaxCertain <- function(values, beta, helpers) {
  ### Softmax choice fx. Outputs chance of picking left stimulus ###
exp(beta * values[1])/sum(exp(beta * values[1]), exp(beta * values[2]))  
}
CalcAltProbCertain <- function(values, helpers, stochasticity=8) {
  ### Softmax choice fx. Outputs chance of picking left stimulus ###
values[1]^(1/stochasticity)/
    sum(values[1]^(1/stochasticity), values[2]^(1/stochasticity))  
}
RunARiskChoicePhase <- function(params, helpers, sim_opt) {
  ### Run through a phase of choice task to sim or opt ###
  #Preselect the order in which will sample certain vs. choice options 
  
  #if (sim_opt=="sim") params <- helpers[["params"]]
  
  if (sim_opt=="sim") {
    risk_tol <- params$risk_tol
    beta <- params$beta  
  } 
  if (sim_opt=="opt") {
    risk_tol <- as.numeric(params["risk_tol"])
    beta <- as.numeric(params["beta"])
  }
  
  # Preallocations for choices and certain and gamble option 
  if (sim_opt=="sim") {
    trial_key <- helpers[["trial_key"]]
    trial_key_sampler <- sample(1:50, 50, replace=FALSE)
    n_trials <- nrow(trial_key)
    choices <- rep(NA, length(trial_key_sampler))
    actions <- rep(NA, length(trial_key_sampler))
    certain_values <- rep(NA, length(trial_key_sampler))
    gamble_values <- rep(NA, length(trial_key_sampler))
  }
  if (sim_opt=="opt") {
    subj_dt <- helpers[["dt"]]
    n_trials <- nrow(subj_dt)
    nll_store <- rep(NA, length(trial_key_sampler))
  } 
  print(params)
  for (tidx in 1:n_trials) {
    ## Set up values this trial ##
    # Pull from trial key or from input dt
    if (sim_opt=="sim") {
      tr_choices <- trial_key[tidx, ] # Get the options this trial
      gamble_value <- certain_value * as.numeric(tr_choices$gain_multipliers) 
    }
    if (sim_opt=="opt") {
      tr_choices <- subj_dt[tidx, ]
      gamble_value <- tr_choices$gamble_values
    } 
    certain_value <- as.numeric(tr_choices$certain/60) # Divide by 60 to bring vals to 0 to 1
    gamble_EV <- gamble_value^risk_tol * gamble_prob
    EVs <- c(certain_value, gamble_EV)
    
    ## Choose ##
    #browser(expr=tidx==2)
    if (sim_opt=="sim") certain_choice_prob <- CalcSoftmaxCertain(EVs, params[["beta"]], helpers)
    if (sim_opt=="opt") {
      #browser(expr=tidx==2)
      if (as.numeric(tr_choices$actions)==1) {
        choice_prob <- CalcSoftmaxCertain(EVs, beta, helpers)
      } else {
        choice_prob <- 1 - CalcSoftmaxCertain(EVs, beta, helpers)
      }
      nll_store[tidx] <- -log(choice_prob)
      #print(nll_store[tidx])
    }
    
    if (sim_opt=="sim") {
      rd <- runif(1, 0, 1)
      #print(rd)
      if (rd < certain_choice_prob) { choice <- "certain" } else { choice <- "gamble" }
      if (choice=="certain") action <- 1 # action idx
      if (choice=="gamble") action <- 2
      ## Store outs ##      
      choices[tidx] <- choice
      actions[tidx] <- action
      certain_values[tidx] <- certain_value
      gamble_values[tidx] <- gamble_value
    }
  
  }
  
  if (sim_opt=="sim") {
    out <- data.table("choices"=choices, 
                      "actions"=actions,
                      "risk_tolerance"=risk_tol,
                      "beta"=beta,
                      "certain_values"=certain_values, 
                      "gamble_values"=gamble_values)
  } 
  if (sim_opt=="opt") {
    out <- sum(nll_store)
  }
out  
}
RunARiskChoicePhaseV2 <- function(params, helpers, sim_opt) {
  ### Run through a phase of choice task to sim or opt ###
  #Preselect the order in which will sample certain vs. choice options 
  
  #if (sim_opt=="sim") params <- helpers[["params"]]
  
  if (sim_opt=="sim") {
    risk_tol <- params$risk_tol
    beta <- params$beta  
  } 
  if (sim_opt=="opt") {
    risk_tol <- as.numeric(params["risk_tol"])
  }
  
  # Preallocations for choices and certain and gamble option 
  if (sim_opt=="sim") {
    trial_key <- helpers[["trial_key"]]
    trial_key_sampler <- sample(1:50, 50, replace=FALSE)
    n_trials <- nrow(trial_key)
    choices <- rep(NA, length(trial_key_sampler))
    actions <- rep(NA, length(trial_key_sampler))
    certain_values <- rep(NA, length(trial_key_sampler))
    gamble_values <- rep(NA, length(trial_key_sampler))
  }
  if (sim_opt=="opt") {
    subj_dt <- helpers[["dt"]]
    n_trials <- nrow(subj_dt)
    nll_store <- rep(NA, length(trial_key_sampler))
  } 
  print(params)
  for (tidx in 1:n_trials) {
    ## Set up values this trial ##
    # Pull from trial key or from input dt
    if (sim_opt=="sim") {
      tr_choices <- trial_key[tidx, ] # Get the options this trial
      gamble_value <- certain_value * as.numeric(tr_choices$gain_multipliers) 
    }
    if (sim_opt=="opt") {
      tr_choices <- subj_dt[tidx, ]
      gamble_value <- tr_choices$gamble_values
    } 
    certain_value <- as.numeric(tr_choices$certain/60) # Divide by 60 to bring vals to 0 to 1
    gamble_EV <- gamble_value^risk_tol * gamble_prob
    EVs <- c(certain_value, gamble_EV)
    
    ## Choose ##
    #browser(expr=tidx==2)
    if (sim_opt=="sim") certain_choice_prob <- CalcAltProbCertain(EVs, helpers)
    if (sim_opt=="opt") {
      #browser(expr=tidx==2)
      if (as.numeric(tr_choices$actions)==1) {
        choice_prob <- CalcAltProbCertain(EVs, helpers)
      } else {
        choice_prob <- 1 - CalcAltProbCertain(EVs, helpers)
      }
      nll_store[tidx] <- -log(choice_prob)
      #print(nll_store[tidx])
    }
    
    if (sim_opt=="sim") {
      rd <- runif(1, 0, 1)
      #print(rd)
      if (rd < certain_choice_prob) { choice <- "certain" } else { choice <- "gamble" }
      if (choice=="certain") action <- 1 # action idx
      if (choice=="gamble") action <- 2
      ## Store outs ##      
      choices[tidx] <- choice
      actions[tidx] <- action
      certain_values[tidx] <- certain_value
      gamble_values[tidx] <- gamble_value
    }
    
  }
  
  if (sim_opt=="sim") {
    out <- data.table("choices"=choices, 
                      "actions"=actions,
                      "risk_tolerance"=risk_tol,
                      "beta"=beta,
                      "certain_values"=certain_values, 
                      "gamble_values"=gamble_values)
  } 
  if (sim_opt=="opt") {
    out <- sum(nll_store)
  }
out  
}
