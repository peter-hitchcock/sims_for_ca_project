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
params[["beta"]] <- 4
params[["lapsiness"]] <- .025
params[["q_learner_prop"]] <- 1
params[["actor_LR"]] <- .1
params[["q_LR"]] <- .1
params[["critic_LR"]] <- .1
helpers[["params"]] <- params
helpers[["sim_or_opt"]] <- 1 # Simulate (1) or optimize (2)? (opt not yet built)
helpers[["n_trials"]] <- n_trials
training_trials <- unlist(lapply(1:n_trials, function(x) sample(states, 1))) # 1 training phase

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

summs <- out_dt %>% group_by(tidx) %>% summarize(m=mean(correct))

# From eyeballing it, seems like there's lot of random variation even w 100 iters and that 
# this param does not lead to much diff in the acc data w these settings 
ggplot(summs, aes(x=tidx, y=m)) + geom_line(size=2) + 
  ga + ap + geom_hline(yintercept=.5) + facet_wrap(~ q_learner_prop) 


##########  SIMS OF KONOVA ET AL. 19 AMBIG RISK TASK ##################
n_trials <- 120
risk_offer <- seq(5, 66, 2)
risk_known_probs <- c(25, 50, 75)

######################################################################