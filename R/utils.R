#' Vanilla theme
#' 
#' An extremely boring plotting theme that differs slightly from theme_classic in ggplot2
theme_vanilla<-function(){
  ggplot2::theme_classic()+
  ggplot2::theme(panel.background=element_blank(),
          strip.background=element_rect(color="white"),
          axis.line.x=element_line(colour="black"),
          axis.line.y=element_line(colour="black"))
}