#----------------------------------------------------------------------------------------------------------------------------
plotLayers<-function(plot=NULL,
                     plotArgs=NULL,
                     climdata=NULL,
                     climArgs=list(),
                     simDirectory="Simulation1",
                     IOmode="suppress"){
  
  plotArgsDefault<-list()
  plotArgsDefault$title="Scenario Neutral Space"
  plotArgsDefault$xlim=c(-2,2)
  plotArgsDefault$ylim=c(0.7,1.3)
  plotArgsDefault$xtitle="x"
  plotArgsDefault$ytitle="y"
  plotArgsDefault$performancelimits=NULL
  plotArgsDefault$lowfill="red"
  plotArgsDefault$highfill="yellow"
  plotArgsDefault$contour=TRUE
  plotArgsDefault$contourlevels=seq(0.6,0.8,0.01)
  
  if(!is.null(plotArgs)){
    plotArgs=modifyList(plotArgsDefault,plotArgs)
  } else {
    plotArgs=plotArgsDefault
  }
  
  
  if(IOmode=="verbose"){
    path<-file.path(".",simDirectory)
    if(!isTRUE(dir.exists(path))){
      dir.create(path)
    }
    
    path<-file.path(".",simDirectory,"Plots")
    if(!isTRUE(dir.exists(path))){
      dir.create(path)
    }
  }
  
  p1<-plot
  
  x<-plotArgs$xtitle
  y<-plotArgs$ytitle
  
  climArgsDefault<-list()
  # climArgsDefault$title="Scenario Neutral Space"
  # climArgsDefault$xlim=c(-2,2)
  # climArgsDefault$ylim=c(0.7,1.3)
  # climArgsDefault$xtitle=NULL
  # climArgsDefault$ytitle=NULL
  climArgsDefault$performancelimits=NULL
  climArgsDefault$label=NULL
  climArgsDefault$slice=2030
  climArgsDefault$colour="black"
  climArgsDefault$fill=NULL
  
  if(!is.null(climArgs)){
    climArgs=modifyList(climArgsDefault,climArgs)
  } else {
    climArgs=climArgsDefault
  }
  # switch(type=climArgs$colour,
  #        "Performance"={p1<-p1+geom_point(data=climdata,aes(x = climdata[,2], y =climdata[,1],colour=climdata$performance),size=2.5)+
  #                             scale_colour_continuous(limits=c(plotArgs$performancelimits[1], plotArgs$performancelimits[2]),low="red", high="yellow",guide="none")+
  #                             geom_point(data=climdata,aes(x = climdata[,2], y =climdata[,1]),colour="black",size=3,shape=1)},
  #        {p1<-p1+geom_point(data=climdata,aes(x = climdata[,2], y =climdata[,1]),colour="black",size=2.5,shape=16)})
  
  
  
  p1<-p1+ggplot2::geom_point(data=climdata,aes(x = climdata[,x], y =climdata[,y]),colour=climArgs$colour,size=2.5,shape=16)
  #ok here
  
  if(!is.null(climArgs$fill)){
    performance=NULL
    if(climArgs$fill=="performance"){ #ok
      p1<-p1+geom_point(data=climdata,aes(x = climdata[,x], y =climdata[,y],colour=performance),size=2.5)+
        scale_colour_continuous(limits=c(plotArgs$performancelimits[1], plotArgs$performancelimits[2]),low="red", high="yellow",guide="none")+
        geom_point(data=climdata,aes(x = climdata[,x], y =climdata[,y]),colour="black",size=3,shape=1)
    } 
  }
  
  if(!is.null(climArgs$label)){
    p1<-p1+ ggplot2::geom_text(data=climdata,aes(x = climdata[,x], y = climdata[,y],label=climdata[climArgs$label]),hjust=1.3)
  }
  
  
  title <- cowplot::ggdraw() + cowplot::draw_label(plotArgs$title, fontface='bold')
  
  p2<-cowplot::plot_grid(title,p1,nrow=2,rel_heights = c(0.05,0.95))
  
  if(IOmode=="verbose"){
    cowplot::save_plot(paste0(path,"/PlotLayer.pdf"),p2,base_aspect_ratio = 1,base_height = 7)
  }
  
  return(p2)
}

#example
# plotArgs$title="Scenario neutral space with projections overlaid"
# climArgs$fill=NULL
# climArgs$fill="performance"
# plotLayers(plot=tank.plot$plotEdit,
#            plotArgs=plotArgs,
#            climdata=climdata,
#            climArgs=climArgs)