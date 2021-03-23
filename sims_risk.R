sapply(c("stringr", "dplyr", "data.table", "purrr", "foreach", "doParallel", "ggplot2"), require, character=TRUE)
sf <- function() sapply(paste0("./Functions/", list.files("./Functions/", recursive=TRUE)), source) # Source all fxs
sf()
DefPlotPars()
registerDoParallel(cores=round(detectCores()*2/3))

# Params to potentially manipulate: 
# magnitude (like the SCZ studies)   
# probability, especially lucas-newman lab work and general idea worry = less precise prob est  


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

