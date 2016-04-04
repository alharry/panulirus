#' Barplot of species catch for multiple fisheries
#' 
#' @param data A data frame of a CAES data extraction
#' @param Species Species name, exactly as per `FishCubename` e.g. "Red Emperor". May also be a vector e.g. c("Red Emperor", "Goldband Snapper")
#' @param fishery_code Fishery codes used by CAES e.g "NDS". May also be a vector e.g. c("NDS","KTR")
#' @param fishery_names Desired fishery name(s). Must be same length as \code{fishery_code} e.g. c("Northern Demersal","Northern Demersal")
#' @param start_year 
#' @param end_year 
#' @param save Saves a PNG file (TRUE or FALSE)
#' @return  A ggplot object

species_barplot<-function(data, Species, fishery_code, fishery_names, start_year=min(data$year,na.rm=T), end_year=max(data$year,na.rm=T),save=FALSE,...){
  
  # Prepare data
  data<-filter(data,FishCubename%in%Species,year%in%c(start_year:end_year))%>%
    mutate(fishery=mapvalues(fishery,from=fishery_code,to=fishery_name))%>%
    mutate(fishery=ifelse(fishery%in%fishery_name,fishery,"Other"))%>%
    group_by(FishCubename,year,fishery)%>%
    summarise(Catch=sum(`live weight (kg)`)/1000)
  
  # Plot data
  p<-ggplot(data,aes(x=year,y=Catch))+
    geom_bar(aes(fill=fishery),colour="black",stat="identity")+
    facet_wrap(~FishCubename)+
    scale_fill_brewer(palette = "YlGnBu",direction=-1)+
    theme_vanilla()+
    ylab("Catch (tonnes)")
  
  # Save image
  if(save==TRUE){ggsave("Plot.png",p)}
  
  return(p)
}