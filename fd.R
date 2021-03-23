RunARiskChoicePhase <- function(helpers, sim_opt) {
  ### Run through a phase of choice task to sim or opt ###
  #Preselect the order in which will sample certain vs. choice options 
  
  params <- helpers[["params"]]
  risk_tol <- params$risk_tol
  beta <- params$beta
  browser()
  # Preallocations for choices and certain and gamble option 
  if (sim_opt=="sim") {
    trial_key <- helpers[["trial_key"]]
    trial_key_sampler <- sample(1:50, 50, replace=FALSE)
    n_trials <- nrow(trial_key)
    choices <- rep(NA, length(trial_key_sampler))
    certain_values <- rep(NA, length(trial_key_sampler))
    gamble_values <- rep(NA, length(trial_key_sampler))
  }
  if (sim_opt=="opt") {
    subj_dt <- helpers[["dt"]]
    n_trials <- nrow(subj_dt)
    nll_store <- rep(NA, length(trial_key_sampler))
  } 
  
  for (tidx in 1:n_trials) {
    ## Set up values this trial ##
    # Pull from trial key or from input dt
    if (sim_opt=="sim") tr_choices <- trial_key[tidx, ] # Get the options this trial
    if (sim_opt=="opt") tr_choices <- subj_dt[tidx, ]
    certain_value <- as.numeric(tr_choices$certain/60) # Divide by 60 to bring vals to 0 to 1
    gamble_value <- certain_value * as.numeric(tr_choices$gain_multipliers)  
    gamble_EV <- gamble_value^x * gamble_prob
    EVs <- c(certain_value, gamble_EV)
    
    ## Choose ##
    certain_choice_prob <- CalcSoftmaxCertain(EVs, params[["beta"]], helpers)
    if (sim_opt=="sim") {
      rd <- runif(1, 0, 1)
      if (rd < certain_choice_prob) { choice <- "certain"  } else { choice <- "risk" }
      if (choice=="certain") action <- 1 # action idx
      if (choice=="gamble") action <- 2
    }
    
    ## Store outs ##      
    choices[tidx] <- choice
    certain_values[tidx] <- certain_value
    gamble_values[tidx] <- gamble_value
  }
  
  if (sim_opt=="sim") {
    out <- data.table("choices"=choices, 
                      "risk_tolerance"=x, 
                      "certain_values"=certain_values, 
                      "gamble_values"=gamble_values)
  } 
  if (sim_opt=="opt") {
    out <- sum(nll_store)
  }
out  
}
