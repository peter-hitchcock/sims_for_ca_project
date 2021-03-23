sapply(c("stringr", "dplyr", "data.table", "purrr", "foreach", "doParallel", "ggplot2"), require, character=TRUE)
sf <- function() sapply(paste0("./Functions/", list.files("./Functions/", recursive=TRUE)), source) # Source all fxs
sf()
DefPlotPars()
registerDoParallel(cores=round(detectCores()*2/3))


# Params to potentially manipulate: 
# magnitude (like the SCZ studies)   
# probability, especially lucas-newman lab work and general idea worry = less precise prob est  

########################### Set Up Experiment ###########################
# states <- c("ab", "cd", "ef", "gh")
# every_stim <- c("a", "b", "c", "d", "e", "f", "g", "h")
# better_state <- c("left", "left", "right", "right")
# # Create a key to access every trial and output pos or neg reward with the appropriate probability  
# key <- data.table("stim"=every_stim, 
#                   "pos_or_neg"=c(1, 1, 1, 1, -1, -1, -1, -1) , 
#                   "p_nz"=c(.9, .8, .2, .1, .9, .8, .2, .1)) # Probability of non-zero outcome
# # To determine if made the optimal choice on a trial
# state_key <- data.table(states, "better_choice"=better_state)
# ######################################################################
# 
# ############ Create actor-critic simulation testbed  ##################
# ## Initializations ##
# n_trials <- 400
# helpers <- list() # List of stuff we'll need to shuttle around fxs 
# params <- list() # Free pars
# params[["beta"]] <- 4
# params[["lapsiness"]] <- .025
# params[["q_learner_prop"]] <- 1
# params[["actor_LR"]] <- .1
# params[["q_LR"]] <- .1
# params[["critic_LR"]] <- .1
# helpers[["params"]] <- params
# helpers[["sim_or_opt"]] <- 1 # Simulate (1) or optimize (2)? (opt not yet built)
# helpers[["n_trials"]] <- n_trials
# training_trials <- unlist(lapply(1:n_trials, function(x) sample(states, 1))) # 1 training phase
# 
# # More settings
# parallelize <- 0
# verbose <- 1 # Trial-wise print out?
# 
# iters <- 80
# if (parallelize) {
#   out <- foreach(i=1:iters) %dopar% RunATrainPhase(states, key, state_key, helpers, verbose)   
# } else {
#   out <- foreach(i=1:iters) %do% RunATrainPhase(states, key, state_key, helpers, verbose)   
# }
# 
# for (iter in 1:iters) out[[iter]]$iter <- iter # Label iteration
# out_dt <- do.call(rbind, out)
# 
# summs <- out_dt %>% group_by(tidx) %>% summarize(m=mean(correct))

# From eyeballing it, seems like there's lot of random variation even w 100 iters and that 
# this param does not lead to much diff in the acc data w these settings 
# ggplot(summs, aes(x=tidx, y=m)) + geom_line(size=2) + 
#   ga + ap + geom_hline(yintercept=.5) + facet_wrap(~ q_learner_prop) 


##### Sims of Rutledge 14 task w risk wo ambiguity variant of Konova choice model ###########################
sim_opt <- "sim"

gamble_prob <- .5
# Rutledge et al 14 - risky gambling task  
# - gain (win/0 risk), loss (lose/0 risk), mixed (win/lose risk) trials  
# going to start w just win for comparability to konova task 
n_trials <- 50
# 5 certain amts and 10 multipliers. Presumably these are randomly chosen 
certain <- c(20, 30, 40, 50, 60) # pence in Rutledge 
gain_multipliers <- c(1.68, 1.82, 2, 2.22, 2.48, 2.8, 3.16, 3.6, 4.2, 5)
# Enumerate options of all trials 
trial_key <- data.table(expand.grid("certain"=certain, "gain_multipliers"=gain_multipliers))

#params[["risk_tol"]] <- 1.2
# Helpers stuff to pass around fxs 
helpers <- list(
  "verbose"=1,
  "trial_key"=trial_key
)

# Simulate varying risk tolerances x
risk_tols <- rep(seq(.4, 2, .1), 8)
if (sim_opt=="sim") {
  
  sim_out <- lapply(risk_tols, function(x) {
    #params <- list()
    params <- data.frame("beta"=4, "risk_tol"=x)
    # params[["beta"]] <- 4
    # params[["risk_tol"]] <- x
    #helpers[["params"]] <- params
    
  out <- RunARiskChoicePhase(params, helpers, sim_opt)
  
  out
  })   
  sim_out
  for (sim in seq_along(risk_tols)) sim_out[[sim]]$sim_ID <- sim # Label each sim
  out_dt_2 <- do.call(rbind, sim_out)
  
  print(table(out_dt_2$risk_tolerance, out_dt_2$choices))
}

sim_opt <- "opt"
tmp <- out_dt_2 %>% filter(sim_ID==1)
params <- data.frame("beta"=4, "risk_tol"=1)
helpers[["dt"]] <- tmp
res <- optim(par=params,
      fn=function(params) { RunARiskChoicePhase(params, helpers, sim_opt) },#RunARiskChoicePhase(helpers, sim_opt),
      method=c("L-BFGS-B"),
      lower=c(1, .1), upper=c(100, 2))
# Optimize for beta and risk tolerance to test recovery 
opt_res_2 <- lapply(split(out_dt_2, out_dt_2$sim_ID), function(x) {
  helpers[["dt"]] <- x 
  res <- optim(par=params,
               fn=function(params) { RunARiskChoicePhase(params, helpers, sim_opt) },#RunARiskChoicePhase(helpers, sim_opt),
               method=c("L-BFGS-B"),
               lower=c(1, .1), upper=c(100, 4))


data.table(unique(x$risk_tolerance), 
           unique(x$beta), data.table(t(res$par), 
           res$value, res$message)) %>% setNames(
             c("beta_sim", "risk_tol_sim", "beta_opt", "risk_tol_opt", "nll", "message")
           )
})
       

tv1 <- data.table(opt_res_2 %>% bind_rows()) # Task variant 1
write.csv(tv1, "./../../data/model_res/dev_sim_param_recov/pr_tv1")
######################################################################

##########  SIMS OF KONOVA ET AL. 19 AMBIG RISK TASK ##################
# n_trials <- 120
# risk_offer <- seq(5, 66, 2)
# risk_known_probs <- c(25, 50, 75)
######################################################################