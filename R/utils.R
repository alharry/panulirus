#' Vanilla theme
#' 
#' 
#' An extremely boring plotting theme that differs slightly from theme_classic in ggplot2
#' @export

theme_vanilla<-function(){
  ggplot2::theme_classic()+
  ggplot2::theme(panel.background=element_blank(),
          strip.background=element_rect(color="white"),
          axis.line.x=element_line(colour="black"),
          axis.line.y=element_line(colour="black"))
}


#' Delta log normal mean
#' 
#' Function for calculating the mean of a zero-inflated log normal variable using the delta method. 
#' Implements the Cox method  (EQ 8) in Fletcher (2008) for calculating the mean and approximate
#' confidence interval for a zero-inflated lognormal variable, or log normal variable when all values 
#' are >0
#' 
#' @param x numeric. The variable of interest. 
#' @param na.rm logical. Should NA values be removed? 
#' @param log logical. If FALSE, the exponent of the mean and confidence intervals is returned
#' @param CI logical. If true returns the standard deviation and upper and lower 95 per cent confidence intervals of the mean 
#' @references Fletcher, D. (2008) Confidence intervals for the mean of the delta-lognormal distribution. \emph{Environ Ecol Stat} 15: 175-189 
#' @return mean The mean of \code{x} adjusted for zero-inflation and lognormal bias
#' @return sd The standard deviation of \code{x} (square root of the variance) adjusted for zero-inflation and lognormal bias
#' @return p Zero inflation factor. The proportion of \code{x}>0
#' @return lower,upper The upper and lower 95 per cent confidence intervals of the mean 
#' @examples 
#' data(sunspots) 
#' delta_mean(sunspots,CI=TRUE)
#' @export


delta_mean<-function (x, na.rm = FALSE, log = FALSE, CI = FALSE) 
{
  if (na.rm == TRUE) {
    x = na.omit(x)
  }
  if (anyNA(x)) {
    stop("Contains NA values. Change na.rm=TRUE")
  }
  m <- length(x[x > 0])
  if (m == 0) {
    warning("No non-zero values for values for x")
    if (CI == FALSE) {
      out <- 0
    }
    else {
      out <- data.frame(mean = 0, sd = 0, p = 1, lower = 0, 
                        upper = 0)
    }
    return(out)
    break
  }
  if (m == 1) {
    warning("Only one non-zero value for x")
    
    n <- length(x)
    p <- m/n
    c = (1 - p)^(n - 1)
    d = 1 + (n - 1) * p
    mean <- log(p) + mean(log(x[x > 0]))
    var <- ((d - c) * (1 - c * d) - m * (1 - c)^2)/(m * (1 - 
                                                           c * d)^2)
    upper <- mean + 2 * sqrt(var)
    lower <- mean - 2 * sqrt(var)
    
    if (CI == FALSE) {
      
      out <- mean
    }
    else {
      out <- data.frame(mean = mean, sd = sqrt(var), p = p, 
                        lower = lower, upper = upper)
    }
  } else {
    n <- length(x)
    p <- m/n
    c = (1 - p)^(n - 1)
    d = 1 + (n - 1) * p
    mean <- log(p) + mean(log(x[x > 0])) + var(log(x[x > 0]))/2
    var <- ((d - c) * (1 - c * d) - m * (1 - c)^2)/(m * (1 - 
                                                           c * d)^2) + var(log(x[x > 0]))/m + var(log(x[x > 0]))^2/(2 * 
                                                                                                                      (m + 1))
    upper <- mean + 2 * sqrt(var)
    lower <- mean - 2 * sqrt(var)
    if (CI == FALSE) {
      out <- mean
    }
    else {
      out <- data.frame(mean = mean, sd = sqrt(var), p = p, 
                        lower = lower, upper = upper)
    }
  }
  if (log == FALSE & CI == FALSE) {
    out <- exp(out)
  }
  else if (log == FALSE & CI == TRUE) {
    out[c(1, 4, 5)] <- exp(out)[c(1, 4, 5)]
  }
  return(out)
}

