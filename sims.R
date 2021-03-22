sapply(c("stringr", "dplyr", "data.table", "purrr", "foreach", "doParallel", "ggplot2"), require, character=TRUE)
sf <- function() sapply(paste0("./Functions/", list.files("./Functions/", recursive=TRUE)), source) # Source all fxs
sf()
DefPlotPars()
registerDoParallel(cores=round(detectCores()*2/3))
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
## Initializations ##
n_trials <- 400
helpers <- list() # List of stuff we'll need to shuttle around fxs 
params <- list() # Free pars
params[["beta"]] <- 100
params[["lapsiness"]] <- .025
params[["q_learner_prop"]] <- .9
params[["actor_LR"]] <- .1
params[["q_LR"]] <- .1
params[["critic_LR"]] <- .1
helpers[["params"]] <- params
helpers[["sim_or_opt"]] <- 1 # Simulate (1) or optimize (2)? (opt not yet built)
helpers[["n_trials"]] <- n_trials
training_trials <- unlist(lapply(1:n_trials, function(x) sample(states, 1))) # 1 training phase

# train_df <- data.frame("trial"=1:n_trials, "stimuli"=training_trials)
# helpers[["train_df"]] <- train_df

# More settings
parallelize <- 0
verbose <- 1 # Trial-wise print out?

iters <- 80
if (parallelize) {
  out <- foreach(i=1:iters) %dopar% RunATrainPhase(states, key, state_key, helpers, verbose)   
} else {
  out <- foreach(i=1:iters) %do% RunATrainPhase(states, key, state_key, helpers, verbose)   
}

for (iter in 1:iters) out[[iter]]$iter <- iter # Label iteration
out_dt <- do.call(rbind, out)

# Not improving over time so still seems to be a bug
summs <- out_dt %>% group_by(tidx) %>% summarize(m=mean(correct))
ggplot(summs, aes(x=tidx, y=m)) + geom_line() 

