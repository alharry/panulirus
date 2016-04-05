#' Species catch barplot
#' 
#' Generates a barplot of species catch using data in the format of a standard CAES extraction.
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

species_bar<-function(data, Species, fishery_code, fishery_names, start_year=min(data$year,na.rm=T), end_year=max(data$year,na.rm=T),col=TRUE,save=FALSE,...){
  
  # Prepare data
  data<-filter(data,FishCubename%in%Species,year%in%c(start_year:end_year))%>%
    mutate(fishery=mapvalues(fishery,from=fishery_code,to=fishery_name))%>%
    mutate(fishery=ifelse(fishery%in%fishery_name,fishery,"Other"))%>%
    group_by(FishCubename,year,fishery)%>%
    summarise(Catch=sum(`live weight (kg)`)/1000)
  
  # Plot data
  p<-ggplot(data,aes(x=year,y=Catch))+
    geom_bar(aes(fill=fishery),colour="black",stat="identity")+
    facet_wrap(~FishCubename)
  
  # Colour or fill
  if(col==TRUE){p<-p+scale_fill_brewer(palette = "YlGnBu",direction=-1)}else
    {p<-p+scale_fill_grey()}
  
  # Theme
  p<-p+theme_vanilla()+
    guides(fill=guide_legend(title="Fishery"))+
    ylab("Catch (tonnes)")+xlab("")
  
  # Save image
  if(save==TRUE){ggsave("Plot.png",p)}
  
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

species_line<-function(data, Species, fishery_code, fishery_names, start_year=min(data$year,na.rm=T), end_year=max(data$year,na.rm=T),col=TRUE,save=FALSE,...){
  
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
  
  # Colour or fill
  if(col==TRUE){p<-p+scale_colour_brewer(palette = "Dark2",direction=-1)}else
  {p<-p+scale_colour_grey()}
  
  # Theme
  p<-p+theme_vanilla()+
    guides(colour=guide_legend(title="Fishery"))+
    ylab("Catch (tonnes)")+xlab("")

  # Save image
  if(save==TRUE){ggsave("Plot.png",p)}
  
  return(p)
}


