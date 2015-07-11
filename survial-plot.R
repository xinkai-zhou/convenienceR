PlotSurv <- function(survfit.obj, conf.int = TRUE, mark.time = TRUE, g.title = "Survival Distribution"){
  # Use ggplot2 to plot survival function estimated from survfit()
  #
  # Args: survfit.obj: an object from function survfit()
  #       conf.int: logical, should confidence interval be plotted? 
  #       mark.time: logical, should censoring mark be plotted?
  #       g.title: title for the final plot
  #   
  # Return: plot of estimated survival curve
  
  library(ggplot2)
  library(scales)
  
  # Construct dataframe from "survfit.obj" ----
  survfit.data <- data.frame(time = survfit.obj$time, 
                             surv = survfit.obj$surv,
                             lower = survfit.obj$lower,
                             upper = survfit.obj$upper,
                             n.censor = survfit.obj$n.censor)
  if("strata" %in% names(survfit.obj)){
    survfit.data$strata = rep(names(survfit.obj$strata), survfit.obj$strata)
  }
  
  # Compute graphical component ----
  col <- ifelse("strata" %in% names(survfit.data), "strata", factor(1))
  g.base <- ggplot(data = survfit.data) + scale_y_continuous(labels = percent) + 
    labs(title = g.title) + geom_step(aes_string(x = "time", y = "surv", colour = col))
  g.confint <- geom_ribbon(aes_string(x = "time", ymin = "lower", 
                                      ymax = "upper", fill = col), alpha = 0.4)
  g.censormark <- geom_point(data = survfit.data[survfit.data$n.censor > 0,], 
                             aes(x = time, y = surv), shape = '|', size = 3)
  
  # Produce plot ----
  if(conf.int & mark.time){
    g.base + g.confint + g.censormark
  }else if(conf.int & !mark.time){
    g.base + g.confint
  }else if(!conf.int & mark.time){
    g.base + g.censormark
  }else{
    g.base
  }
}