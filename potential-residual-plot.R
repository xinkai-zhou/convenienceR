PoRePlot <- function(m, lab.id = rownames(m$model), nlab.po = 1,
                     nlab.re = 1){
  # Produce Potential-Residual plot for linear model (Hadi, 1992)
  #
  # Args: m: object from function lm()
  #       lab.id: text labels for high potential and high residual points
  #       nlab.po, nlab.re: number of high potential (residual) points
  #                         to be labeled. 
  # 
  # Values: Potential-Residual plot
  #
  # Note: (The plot was available in {bstats} package, however, 
  #        the package is no longer available (for R version 3.2.0)) 
  #
  # Reference: 'Regression Analysis by Example' by Chatterjee, Hadi
  #
  # Package Dependence: {ggplot2}
  # Author: Xinkai Zhou
  
  # Load ggplot2
  ifelse("ggplot2" %in% installed.packages()[, "Package"],
         library(ggplot2),
         install.packages("ggplot2", repos="http://cran.cnr.Berkeley.edu/"))
  # Compute potential function
  potential.fun <- hatvalues(m)/(1-hatvalues(m))
  # Compute normalized residual
  d <- resid(m)/sqrt(t(resid(m)) %*% resid(m))
  # Compute residual function
  residual.fun <- ((length(m$coefficients)) * (d^2)) / 
    ((1 - potential.fun) * (1 - d^2))
  # Points above cutoff will be labeled with texts.
  potential.cutoff <- sort(potential.fun, decreasing = T)[nlab.po]
  residual.cutoff <- sort(residual.fun, decreasing = T)[nlab.re]
  cat("High Leverage: ", lab.id[potential.fun >= potential.cutoff], "\n")
  cat("High Residual: ", lab.id[residual.fun >= residual.cutoff], "\n")
  # Produce plot.
  qplot(x = residual.fun, 
        y = potential.fun, 
        label = lab.id, 
        geom = "point") + 
    geom_text(angle = 45, 
              aes(size = ifelse((residual.fun >= residual.cutoff) | 
                                  (potential.fun >= potential.cutoff), 
                                1, 0)), show_guide = F) + 
    labs(title = "Potential-Residual Plot")
}