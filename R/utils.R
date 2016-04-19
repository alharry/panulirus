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
#' Implements the Cox method in Fletcher (2008) for calculating the mean and approximate
#' confidence interval for a zero-inflated lognormal variable, or log normal variable when all values 
#' are >0
#' 
#' @param x numeric. The variable of interest. 
#' @param na.rm logical. Should NA values be removed? 
#' @param log logical. Don't back-transform i.e. leave in log-space 
#' @param CI logical. If true returns the variance and upper and lower 95 per cent confidence intervals of the mean 
#' @references Fletcher, D. (2008) Confidence intervals for the mean of the delta-lognormal distribution. \emph{Environ Ecol Stat} 15: 175-189 
#' @return mean The mean of \code{x} adjusted for zero-inflation and lognormal bias
#' @return var The variance of \code{x} adjusted for zero-inflation and lognormal bias
#' @return lower,upper The upper and lower 95 per cent confidence intervals of the mean 
#' @examples 
#' data(sunspots) 
#' delta_mean(sunspots,CI=TRUE)
#' @export


delta_mean<-function(x, na.rm=FALSE,log=FALSE,CI=FALSE){
  
  if(na.rm==TRUE){x=na.omit(x)}
  if(anyNA(x)){stop("Contains NA values. Change na.rm=TRUE")}
  
  m<-length(x[x>0])
  n<-length(x) 
  p<-m/n 
  c=(1-p)^(n-1)
  d=1+(n-1)*p
  
  mean<-log(p)+mean(log(x[x>0]))+var(log(x[x>0]))/2
  var<-((d-c)*(1-c*d)-m*(1-c)^2)/(m*(1-c*d)^2)+var(log(x[x>0]))/m+var(log(x[x>0]))^2/(2*(m+1))
  upper<-mean+2*sqrt(var)
  lower<-mean-2*sqrt(var)
  
  if(CI==FALSE){out<-mean}else{out<-data.frame(mean=mean,var=var,lower=lower,upper=upper)}
  
  if(log==FALSE){out<-exp(out)}
  
  return(out)
}