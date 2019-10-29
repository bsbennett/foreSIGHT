
#-------------------------------------------------------------------------
heatPlot<-function(plotArgs=plotArgs,
                   targetMat=targetMat, 
                   attSel=attSel,
                   attPerturb=attPerturb,
                   performance=performance){
  level=NULL
  
  #Not hooked up
  if(is.null(plotArgs$xtitle)){
    plotArgs$xtitle=tagBlender(as.character(attPerturb[2]))
  }
 
  if(is.null(plotArgs$ytitle)){
      plotArgs$ytitle=tagBlender(as.character(attPerturb[1]))
  }
  
  #GET COLUMN NAMES
  x<-attPerturb[2]
  y<-attPerturb[1]
  
  
  targetPerf<-data.frame(targetMat,performance)
  
  p1 <- ggplot() +
        geom_tile(data=targetPerf,aes(x = targetPerf[,x], y = targetPerf[,y],fill = performance)) +
        xlab(tagBlender(x)) +
        ylab(tagBlender(y)) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.title.x=element_text(vjust=2)) +
        theme(axis.title.y=element_text(angle=90, vjust=-0.5)) +
        theme(axis.title=element_text(size=12),axis.text=element_text(size=10)) +
        theme(axis.line=element_blank())+
        theme(plot.margin = unit(c(0,0.2,0.7,0), "cm"))+
        scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        theme(legend.position="bottom",legend.box.margin = margin(-10,-10,-4,-10),legend.justification="centre")+
        theme(legend.title=element_text(size=11),legend.text=element_text(size=10),legend.key.width = unit(2,"cm"))+
        theme(panel.border = element_rect(colour = "black",size=1,linetype="solid",fill=NA))+
        coord_cartesian(xlim=c(plotArgs$xlim[1],plotArgs$xlim[2]),ylim=c(plotArgs$ylim[1],plotArgs$ylim[2]))
  
  if(isTRUE(plotArgs$contour)) {
    p1<-p1+directlabels::geom_dl(data=targetPerf,aes(x = targetPerf[,x], y = targetPerf[,y],z=performance,label=stat(level)), #edited 20/06/2018
            method=list("first.points", "calc.boxes", "enlarge.box",box.color = NA, fill = "transparent", vjust=1.5,hjust=-0.5, "draw.rects"),stat="contour")+  #,breaks=seq(0.6,0.8,0.01)
            stat_contour(data=targetPerf,aes(x = targetPerf[,x], y = targetPerf[,y],z = performance),colour="black") #,breaks=seq(0.6,0.8,0.01)
  }
  
  if(!is.null(plotArgs$performancelimits)){
    p1<-p1+scale_fill_continuous(limits=c(plotArgs$performancelimits[1], plotArgs$performancelimits[2]),low=plotArgs$lowfill, high=plotArgs$highfill)
  } else {
    p1<-p1+scale_fill_gradient(low=plotArgs$lowfill, high=plotArgs$highfill)
  }
  
  if(!is.null(plotArgs$legendtitle)){
    p1<-p1+guides(fill = guide_colorbar(title = plotArgs$legendtitle))
  } else {
    p1<-p1+guides(fill = guide_colorbar(title = "Performance"))
  }
  
  # guides(fill = guide_colorbar(title = "Performance"))

  #Create a title
  title <- cowplot::ggdraw() + cowplot::draw_label(plotArgs$title, fontface='bold')
  #Stitch title to plot
  p2<-cowplot::plot_grid(title,p1,align="v",nrow=2,rel_heights = c(0.05,0.95))

  return(list(plotEdit=p1,plot=p2))

}

#Wrapper for making each OAT panel
plotwrapperoat<-function(data=NULL, plotArgs=NULL){
  p1 <- ggplot(data=data,
               aes(x=data[,1],y=data[,2]))+ #edited 23/6/2018 for ggplot compatability
               geom_line() +
               scale_x_continuous()+
               theme(axis.line=element_blank())+
               theme(panel.border = element_rect(colour = "black",size=1,linetype="solid",fill=NA))+
               xlab(tagBlender(names(data)[1]))+
               ylab(names(data)[2])+
               ylim(plotArgs$performancelimits[1],plotArgs$performancelimits[2])
}

oatplots<-function(targetMat=NULL,
                   attPerturb=NULL,
                   performance=NULL,
                   exSpArgs=NULL,
                   plotArgs=NULL
                   ){
  
  #No. of things perturbed
  nAtts=length(attPerturb)
  
  #stitch
  targetPerf<-data.frame(targetMat[,attPerturb],performance)  
  
  #get indexes at which to chop targetMat 
  chop=rep(NA,(nAtts))
  for(i in 1:nAtts){chop[i]=sum(exSpArgs$samp[1:i])}
  
  #Chop into dataframes
  dflist<-list()
  for(i in 1:nAtts){
    dflist[[i]]=targetPerf[(chop[i]-exSpArgs$samp[i]+1):chop[i],c(i,(nAtts+1))]
  }
  #PLOT OATIES
  myplots <- lapply(X=dflist,FUN=plotwrapperoat,plotArgs)  # new empty list
  p4 <- cowplot::plot_grid(plotlist=myplots,ncol=nAtts)
  return(p4)
}

#EXAMPLE
# oatplots(targetMat=out$target,attPerturb=attPerturb, performance=seq(0,1,1/(sum(exSpArgs$samp)-1)),exSpArgs=exSpArgs)


contourPlots<-function(plotArgs=plotArgs,
                       targetMat=targetMat, 
                       attPerturb=attPerturb,
                       performance=performance
                       ){
  
  level=NULL
  #not hooked up
  if(is.null(plotArgs$xtitle)){
    plotArgs$xtitle=tagBlender(as.character(attPerturb[2]))
  }
  
  if(is.null(plotArgs$ytitle)){
    plotArgs$ytitle=tagBlender(as.character(attPerturb[1]))
  }
  
  #GET COLUMN NAMES
  x<-attPerturb[2]
  y<-attPerturb[1]
  
  targetPerf<-data.frame(targetMat,performance)
  
  p1 <- ggplot() +
    stat_contour(data=targetPerf,aes(x = targetPerf[,x], y = targetPerf[,y],z = performance),colour="black",breaks=plotArgs$contourlevels)+
    scale_colour_continuous(low="red",high="yellow")+
    xlab(tagBlender(x)) +
    ylab(tagBlender(y)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.title.x=element_text(vjust=2)) +
    theme(axis.title.y=element_text(angle=90, vjust=-0.5)) +
    theme(axis.title=element_text(size=12),axis.text=element_text(size=10)) +
    theme(axis.line=element_blank())+
    theme(plot.margin = unit(c(0,0.2,0.2,0), "cm"))+
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    theme(panel.border = element_rect(colour = "black",size=1,linetype="solid",fill=NA))+ #edit 20/6/2018 fill=NA added
    theme(legend.position="bottom",legend.box.margin = margin(-20,-10,-4,-10),legend.justification="centre")+
    theme(legend.title=element_text(size=11),legend.text=element_text(size=10),legend.key.width = unit(2,"cm"))+
    directlabels::geom_dl(data=targetPerf,aes(x = targetPerf[,x], y = targetPerf[,y],z=performance,label=stat(level)),  #edit 20/6/2018
                          method=list("first.points", "calc.boxes", "enlarge.box", 
                          box.color = NA, fill = "transparent", vjust=1.5,hjust=-1, "draw.rects"),stat="contour",breaks=plotArgs$contourlevels)+
    coord_cartesian(xlim=c(plotArgs$xlim[1],plotArgs$xlim[2]),ylim=c(plotArgs$ylim[1],plotArgs$ylim[2]))
  
  
    title <- cowplot::ggdraw() + cowplot::draw_label(plotArgs$title, fontface='bold')
    p2<-cowplot::plot_grid(title,p1,align="v",nrow=2,rel_heights = c(0.05,0.95))

  return(list(plotEdit=p1,plot=p2))
  
}

# binaryPlots<-function(plotArgs=plotArgs,targetMat=targetMat, attSel=attSel,attPrim=attPrim,performance=performance){
#   
#    x<-which(attSel==attPrim[2])
#    y<-which(attSel==attPrim[1])
# 
#   # p1 <- ggplot(df1, aes(x = df1[,x], y = df1[,y], z = performance))+
#   # #  geom_tile(aes(fill=brks)) +
#   #   stat_contour(geom="contour",breaks=c(0.765),color="black",size=3)+
#   #   geom_polygon(data=df_poly,aes(x,y))+
#   #   xlab(attPrim[2]) +
#   #   ylab(attPrim[1])+
#   #   scale_x_continuous(expand=c(0,0),limits=c(-2,2))+
#   #   scale_y_continuous(expand=c(0,0),limits = c(0.9,1.1))
#   # p1
#    
#    conts<-list()
#    conts<-contourLines(x=seq(-8.5,4.5,1),y=seq(0.8,1.4,0.05),z=outmat,levels = c(100))
#    
#    dft<-data.frame(conts[[1]])
#    n<-nrows(dft)
#    dft$x[1]
#    dft$x[n]
#    dft$y[1]
#    dft$y[n]
#    
#    minx<-bounds$x[1]
#    maxx<-bounds$x[2]
#    miny<-bounds$y[1]
#    maxy<-bounds$y[2]
#    
#    outmat<-matrix(NA,nrow=14,ncol=13)
#    k=1
#    for (i in 1:14){  #x
#      for (j in 1:13){ #y
#        outmat[i,j]=df1$out[k]
#        k=k+1
#      }
#    }
#    
#    dft<-data.frame(conts[[1]])
#   
#   ggplot(data=dft,aes(x=x,y=y))+
#     geom_smooth(method="loess",se=F,span=0.5,size=1.3,colour="black")+
#     geom_ribbon(aes(ymin=0.8,ymax=predict(loess(y~x,span=0.5))),alpha=0.3,fill="green")+
#     geom_ribbon(aes(ymin=predict(loess(y~x,span=0.5))),ymax=1.4,alpha=0.3,fill="red")+
#     theme(panel.border = element_rect(colour = "black",size=1,linetype="solid"))
#   
#   dft2[2:25,]=dft
#   dft2[1,]=c(100,-8.5,1.4)
#   dft2[26,]=c(100,4.5,1.4)
#   
#   ggplot(data=dft2,aes(x=x,y=y))+
#     geom_polygon(aes(fill=level),alpha=0.3)+
#     scale_x_continuous(expand=c(0,0),limits=c(-8.5,4.5))+
#     scale_y_continuous(expand=c(0,0),limits = c(0.8,1.4))+
#     scale_color_manual(values="red")+
#     theme(legend.position = "none")
#   
# }

