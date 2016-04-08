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
    ylab("Catch (tonnes)")+xlab("")
  
  # Axis ticks
  if(is.factor(new$x)){p<-p+theme(axis.text.x=element_text(angle=90,hjust=1))}
  
  return(p)
  
}

#' Species catch line plot
#' 
#' Generates a line plot of species catch using data in the format of a standard CAES extraction.
#' 
#' 
#' @param data A data frame of a CAES data extraction
#' @param Species Species name, exactly as per `FishCubename` e.g. \code{"Red Emperor"}. May also be a vector e.g. \code{c("Red Emperor", "Goldband Snapper")}
#' @param fishery_code Fishery codes used by CAES e.g \code{"NDS"}. May also be a vector e.g. \code{c("NDS","KTR")}
#' @param fishery_names Desired fishery name(s). Must be same length as \code{fishery_code} e.g. \code{c("Northern Demersal","Northern Demersal")}
#' @param start_year 
#' @param end_year 
#' @param col Should the graph be in colour? (TRUE or FALSE) 
#' @param save Saves a PNG file (TRUE or FALSE) using the size of the current graphics device (i.e. may not work very well)
#' @return  A ggplot object

species_line<-function(data, Species, fishery_code, fishery_names, start_year=min(data$year,na.rm=T), end_year=max(data$year,na.rm=T),col=TRUE,add_cumulative=FALSE,save=FALSE,...){
  
  # Prepare data
  data<-filter(data,FishCubename%in%Species,year%in%c(start_year:end_year))%>%
    mutate(fishery=mapvalues(fishery,from=fishery_code,to=fishery_name))%>%
    mutate(fishery=ifelse(fishery%in%fishery_name,fishery,"Other"))%>%
    group_by(FishCubename,year,fishery)%>%
    summarise(Catch=sum(`live weight (kg)`)/1000)
  
  # Plot data
  p<-ggplot(data,aes(x=year,y=Catch))+
    geom_line(aes(colour=fishery),stat="identity")+
    geom_point(aes(colour=fishery))+
    facet_wrap(~FishCubename)
  
  # Add cumulative catch series
  if(add_cumulative==TRUE){
  data2<-group_by(data,FishCubename,year)%>%summarise(Catch=sum(Catch))%>%mutate(fishery="Total")
    p<-p+geom_line(data=data2,aes(x=year,y=Catch,colour=fishery),linetype="dashed")+
         geom_point(data=data2,aes(x=year,y=Catch,colour=fishery))
  }
  
  # Colour or fill
  if(col==TRUE){p<-p+scale_colour_brewer(palette = "Dark2",direction=-1)}else
  {p<-p+scale_colour_grey()}
  
  # Theme
  p<-p+theme_vanilla()+
    guides(colour=guide_legend(title="Fishery"),linetype=guide_legend("Total"))+
    ylab("Catch (tonnes)")+xlab("")

  # Save image
  if(save==TRUE){ggsave("Plot.png",p)}
  
  return(p)
}


