#' Generic barplot
#' 
#' A generic bivariate barplot. Minimally requires x and y variables but can also take additional grouping and paneling variables. 
#' 
#' 
#' @param x x variable. Can be continuous numeric variable (e.g. year) or character (e.g. financial year)
#' @param y y variable. A numeric variable (e.g. catch)
#' @param group A grouping variable (e.g. fishery)
#' @param panel A panelling variable (e.g. species)
#' @param data A \code{data.frame} containing at least columns \code{x} and \code{y} and optionally \code{group} and \code{panel}
#' @param col Should the graph be in colour? (TRUE or FALSE)
#' @examples generic_bar(year, catch, fishery, species, data=data)
#' @return  A ggplot object
#' @export

generic_bar = function(x,y,group=NULL,panel=NULL,data,col=TRUE)
{
  # Prepare data
  arguments <- as.list(match.call())
  new = data.frame(x=eval(arguments$x, data))
  new$y = eval(arguments$y, data)
  if("group"%in%names(as.list(match.call()))){new$group=eval(arguments$group,data)}
  if("panel"%in%names(as.list(match.call()))){new$panel=eval(arguments$panel,data)}
  
  # Plot data
  p<-ggplot(new,aes(x,y))
  
  # Add bars
  if("group"%in%names(new))
  {p<-p+geom_bar(aes(fill=group),colour="black",stat="identity")
  if(col==TRUE)
  {p<-p+scale_fill_brewer(palette = "YlGnBu",direction=-1)}else
  {p<-p+scale_fill_grey()}}else
  {p<-p+geom_bar(colour="black",stat="identity",fill=ifelse(col==TRUE,"cornflowerblue","grey"))}
  
  # Add panel
  if("panel"%in%names(new)){
    p<-p+facet_wrap(~panel)
  }
  
  # Theme
  p<-p+theme_vanilla()+
    ylab("Catch (tonnes)")+xlab("")+
    theme(legend.title=element_blank())
  
  # Axis ticks
  if(is.factor(new$x)){p<-p+theme(axis.text.x=element_text(angle=90,hjust=1))}
  
  return(p)
  
}

#' Generic line plot
#' 
#' A generic bivariate line plot. Minimally requires x and y variables but can also take additional grouping and paneling variables. 
#' 
#' 
#' @param x x variable. Can be continuous numeric variable (e.g. year) or character (e.g. financial year)
#' @param y y variable. A numeric variable (e.g. catch)
#' @param group A grouping variable (e.g. fishery)
#' @param panel A panelling variable (e.g. species)
#' @param data A \code{data.frame} containing at least columns \code{x} and \code{y} and optionally \code{group} and \code{panel}
#' @param col Should the graph be in colour? (TRUE or FALSE)
#' @examples generic_line(year, catch, fishery, species, data=data)
#' @return  A ggplot object
#' @export

generic_line = function(x,y,group=NULL,panel=NULL,data,col=TRUE)
{
  # Prepare data
  arguments <- as.list(match.call())
  new = data.frame(x=eval(arguments$x, data))
  new$y = eval(arguments$y, data)
  if("group"%in%names(as.list(match.call()))){new$group=eval(arguments$group,data)}
  if("panel"%in%names(as.list(match.call()))){new$panel=eval(arguments$panel,data)}
  
  # Plot data
  p<-ggplot(new,aes(x,y))
  
  # Add line
  if("group"%in%names(new))
  {p<-p+geom_line(aes(colour=group))+geom_point(aes(colour=group))
  if(col==TRUE)
  {p<-p+scale_colour_brewer(palette = "BuGn",direction=1)}else
  {p<-p+scale_colour_grey()}}else
  {p<-p+geom_line(colour="black",stat="identity",colour=ifelse(col==TRUE,"cornflowerblue","grey"))}
  
  # Add panel
  if("panel"%in%names(new)){
    p<-p+facet_wrap(~panel)
  }
  
  # Theme
  p<-p+theme_vanilla()+
    ylab("Catch (tonnes)")+xlab("")+
    theme(legend.title=element_blank())
  
  # Axis ticks
  if(is.factor(new$x)){p<-p+theme(axis.text.x=element_text(angle=90,hjust=1))}
  
  return(p)
  
}
