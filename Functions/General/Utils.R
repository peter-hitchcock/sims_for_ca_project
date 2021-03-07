DefPlotPars <- function() {
  ### Set up some plot aspects we'll reuse across functions ####
  
  # Notes: Invoked only for side effect of returning general plot pars
  # to the global environment ###
  ga <<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  # legend pars #
  lp <<- theme(legend.text = element_text(size = 20),
               legend.title = element_blank(),
               legend.key.size = unit(2.5, 'lines'))
  
  # turn off legend
  tol <<- theme(legend.position='none')
  
  # axis pars #
  ap <<- theme(axis.text = element_text(size=15),
               axis.title = element_text(size=20))
  # title pars #
  tp <<- theme(plot.title = element_text(size = 20, face='bold', hjust = .5))
  
  stp <<- theme(plot.subtitle = element_text(size=14))
  # color pars #
  cf_vals <<- c('orange', 'purple')
}