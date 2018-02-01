##################################
###      PLOTTING SUITE        ###
##################################

#CONTAINS
  # traffic.col - traffic light colour ramp
  # polygon.monthwise() - creates coord.x, coord.y of max and min for monthly data
  # polygon.seasonal() - creates coord.x, coord.y of max and min for seasonal data
  # polygon.annual() - creates coord.x, coord.y of max and min for annual data  
  # simTS.overlayMonthlyObsRange()
  # monthwise.boxplots() - plots stat for each month as a separate boxplot
  # seasonal.boxplots() - plots stats for each season as a separate boxplot
  # annual.boxplots() - plots stats annual stats as boxplot
  # simVobsTS() - plots time series to compare or moving average ts to compare
  # trafficAttPlot() - traffic light plot of attribute performance
  # expSpace2dViz() - plot 2d exposure space
  # frontBoilerPlateInfo() - informative text on the target
  # exposureSlices()


#*****TO DO LATER IF NEEDED *******
  # metricGrid() - comparison of att or annual metrics in grid format (if sampled as a grid)
  # tabulateMetric() - table output function (uses Grobs - see draft version)

#----------------------------
#COLOUR RAMP
traffic.col=c("chartreuse3","gold1","red1")

polygon.monthwise<-function(obsDat=NULL #List with max and min entries of length 12
){
  coord.y=c(obsDat$min[1],obsDat$min[1:12],obsDat$min[12],   # 0-13
            obsDat$max[12],obsDat$max[12:1],obsDat$max[1],   # 13 -0
            obsDat$min[1])                                   # close loop
  coord.x=c(seq(0,13),seq(13,0),0)
  out=list(coord.x=coord.x,coord.y=coord.y)
  return(out)
}

polygon.seasonal<-function(obsDat=NULL #List with max and min entries of length 4
){
  seas.order=c(2,3,4,1)
  coord.y=c(obsDat$min[seas.order[1]],obsDat$min[seas.order[1:4]],obsDat$min[seas.order[4]],   # 0-4
            obsDat$max[seas.order[4]],obsDat$max[seas.order[4:1]],obsDat$max[seas.order[1]],   # 4 -0
            obsDat$min[seas.order[1]])                                 # close loop
  coord.x=c(seq(0,5),seq(5,0),0)
  out=list(coord.x=coord.x,coord.y=coord.y)
  return(out)
}

polygon.annual<-function(obsDat=NULL #List with max and min entries of length 4
){
  coord.y=c(rep(obsDat$min[1],3),   # 
            rep(obsDat$max[1],3),   # 
            obsDat$min[1])                                 # close loop
  coord.x=c(seq(0,2),seq(2,0),0)
  out=list(coord.x=coord.x,coord.y=coord.y)
  return(out)
}

monthwise.boxplots<-function(simDat=NULL, #sim data list of stats (from collate.stat.lib.r)
                             obsDat=NULL, #observed data list if adding polygon
                             compObs=TRUE, #add observed stast
                             metricTag=NULL,  #name of metric plotted
                             ...
){
  #GET Y-RANGES  
  if(compObs==TRUE){
    y.min=min(c(min(simDat$TS,na.rm=TRUE),min(obsDat$TS,na.rm=TRUE)),ma.rm=TRUE)
    y.max=max(c(max(simDat$TS,na.rm=TRUE),max(obsDat$TS,na.rm=TRUE)),na.rm=TRUE)
  }else{
    y.min=min(simDat$TS,na.rm=TRUE)  
    y.max=max(simDat$TS,na.rm=TRUE)
  }
  #BLANK PLOT
  plot(1,type="n",xlim=c(0.2,12.8),ylim=c(y.min,y.max),ylab=metricTag,xlab="Month",xaxt="n")
  graphics::axis(side=1,at=seq(1,12),labels=month.abb)     #note month.abb built into R 
  
  
  #ADD COMPARISON TO OBSERVED DATASET
  if(compObs==TRUE){
    poly=polygon.monthwise(obsDat=obsDat)                                                   #make monthwise polygon
    graphics::polygon(x=poly$coord.x,y=poly$coord.y,col = "lemonchiffon1",border = NA)                #add obs range polygon
    graphics::lines(x=seq(0,13),y=c(obsDat$median[1],obsDat$median[1:12],obsDat$median[12]),lwd=2,col="blue")     #add obs median line
  } 
  
  #ADD BOXPLOT FOR EACH MONTH
  for(m in 1:12){
    boxplot.func(z=simDat$TS[m,],at.pt=m,col="lightgray")   #plot each month separately
  }
  
  #ADD LEGEND
  if(compObs==TRUE){
    graphics::legend("topright",legend = c("sim.","obs. range","obs. med"),horiz = FALSE,seg.len = c(1,1,1.5),
                    col=c(NA,NA,"blue"),fill = c("lightgray","lemonchiffon1",NA),border = c("black","black",NA),
                    lwd = c(NA,NA,2),
                    bty="n" )
  }else{
    graphics::legend("topright",legend = c("sim."),horiz = FALSE,seg.len = c(1),
                    col=c(NA),fill = c("lightgray"),border = c("black"),
                    lwd = c(NA),
                    bty="n" )
  }
}
#tester
#monthwise.boxplots(simDat=tmp,obsDat=tmp,compObs=TRUE,metricTag="monTot")

seasonal.boxplots<-function(simDat=NULL, #sim data list of stats (from collate.stat.lib.r)
                            obsDat=NULL, #observed data list if adding polygon
                            compObs=TRUE, #add observed stast
                            metricTag=NULL,  #name of metric plotted
                             ...
){
  #GET Y-RANGES  
  if(compObs==TRUE){
    y.min=min(c(min(simDat$TS,na.rm=TRUE),min(obsDat$TS,na.rm=TRUE)),ma.rm=TRUE)
    y.max=max(c(max(simDat$TS,na.rm=TRUE),max(obsDat$TS,na.rm=TRUE)),na.rm=TRUE)
  }else{
    y.min=min(simDat$TS,na.rm=TRUE)  
    y.max=max(simDat$TS,na.rm=TRUE)
  }
  #BLANK PLOT
  plot(1,type="n",xlim=c(0.75,4.25),ylim=c(y.min,y.max),ylab=metricTag,xlab="Season",xaxt="n",xaxs="i")
  seas.order=c(2,3,4,1)
  graphics::axis(side=1,at=seq(1,4),labels=c("DJF","MAM","JJA","SON"))   
  
  
  #ADD COMPARISON TO OBSERVED DATASET
  if(compObs==TRUE){
    poly=polygon.seasonal(obsDat=obsDat)                                                   #make monthwise polygon
    graphics::polygon(x=poly$coord.x,y=poly$coord.y,col = "lemonchiffon1",border = NA)                #add obs range polygon
    graphics::lines(x=seq(0,5),y=c(obsDat$median[seas.order[1]],obsDat$median[seas.order[1:4]],obsDat$median[seas.order[4]]),lwd=2,col="blue")     #add obs median line
  } 
  
  #ADD BOXPLOT FOR EACH MONTH
  for(s in 1:4){
    boxplot.func(z=simDat$TS[seas.order[s],],at.pt=s,col="lightgray")   #plot each season separately
  }
  
  #ADD LEGEND
  if(compObs==TRUE){
    graphics::legend("topleft",legend = c("sim.","obs. range","obs. med"),horiz = FALSE,seg.len = c(1,1,1.5),
                     col=c(NA,NA,"blue"),fill = c("lightgray","lemonchiffon1",NA),border = c("black","black",NA),
                     lwd = c(NA,NA,2),
                     bty="n" )
  }else{
    graphics::legend("topleft",legend = c("sim."),horiz = FALSE,seg.len = c(1),
                     col=c(NA),fill = c("lightgray"),border = c("black"),
                     lwd = c(NA),
                     bty="n" )
  }
}
#tester
#seasonal.boxplots(simDat=tmp,obsDat=tmp,compObs=TRUE,metricTag="seasTot")


annual.boxplots<-function(simDat=NULL, #sim data list of stats (from collate.stat.lib.r)
                          obsDat=NULL, #observed data list if adding polygon
                          compObs=TRUE, #add observed stast
                          metricTag=NULL,  #name of metric plotted
                          x.lab=NULL,    #lab for x axis
                          ...
){
  #GET Y-RANGES  
  if(compObs==TRUE){
    y.min=min(c(min(simDat$TS,na.rm=TRUE),min(obsDat$TS,na.rm=TRUE)),ma.rm=TRUE)
    y.max=max(c(max(simDat$TS,na.rm=TRUE),max(obsDat$TS,na.rm=TRUE)),na.rm=TRUE)
  }else{
    y.min=min(simDat$TS,na.rm=TRUE)  
    y.max=max(simDat$TS,na.rm=TRUE)
  }
  #BLANK PLOT
  plot(1,type="n",xlim=c(0.7,1.3),ylim=c(y.min,y.max),ylab=metricTag,xlab="",xaxt="n")
  if(!is.null(x.lab)) axis(side=1,at=1,labels=x.lab)     
  
  #ADD COMPARISON TO OBSERVED DATASET
  if(compObs==TRUE){
    poly=polygon.annual(obsDat=obsDat)                                                   #make annual polygon
    graphics::polygon(x=poly$coord.x,y=poly$coord.y,col = "lemonchiffon1",border = NA)             #add obs range polygon
    graphics::lines(x=seq(0,2),y=rep(obsDat$median[1],3),lwd=2,col="blue")     #add obs median line
  } 
  
  #ADD BOXPLOT
    boxplot.func(z=simDat$TS,at.pt=1,col="lightgray")   #plot each season separately
  
  
  #ADD LEGEND
  if(compObs==TRUE){
    graphics::legend("bottomright",legend = c("sim.","obs. range","obs. med"),horiz = FALSE,seg.len = c(1,1,1.5),
                     col=c(NA,NA,"blue"),fill = c("lightgray","lemonchiffon1",NA),border = c("black","black",NA),
                     lwd = c(NA,NA,2),
                     bty="n" )
  }else{
    graphics::legend("bottomright",legend = c("sim."),horiz = FALSE,seg.len = c(1),
                     col=c(NA),fill = c("lightgray"),border = c("black"),
                     lwd = c(NA),
                     bty="n" )
  }
}

#annual.boxplots(simDat=tmp,obsDat=tmp,compObs=TRUE,metricTag="annTot",x.lab="Point X")

simVobsTS<-function(simTS=NULL,  #timeseries vector
                    obsTS=NULL,  #timeseries vector
                    datInd=NULL, #date Indices
                    varName=NULL,  #variable name
                    asRollAv=NULL #add rolling average line
                    
){
  

  

  
  #PLOT THE TWO SERIES
  if(asRollAv==FALSE){
    #GET Y-RANGE 
    y.min=min(c(min(simTS,na.rm=TRUE),min(obsTS,na.rm=TRUE)),ma.rm=TRUE)
    y.max=max(c(max(simTS,na.rm=TRUE),max(obsTS,na.rm=TRUE)),na.rm=TRUE)
    y.range=c(y.min,y.max)
    
    #BLANK PLOT
    plot(1,type="n",xlim=c(1,length(obsTS)),ylim=y.range,ylab=varName,xlab="Indx",xaxs="i")
    
    graphics::lines(x=seq(1,length(obsTS)),y=obsTS,col="blue") #add obs time series
    graphics::lines(x=seq(1,length(simTS)),y=simTS,col="red")  #add sim timeseries
  }else{
    #PLOT AS ROLLING AVERAGE
    period=30
    rollObs=movingAverage(x=obsTS, n=period, centered=TRUE)  #take moving average of observed timeseries
    rollSim=movingAverage(x=simTS, n=period, centered=TRUE)  #take moving average of observed timeseries
  
    #GET Y-RANGE 
    y.min=min(c(min(rollSim,na.rm=TRUE),min(rollObs,na.rm=TRUE)),ma.rm=TRUE)
    y.max=max(c(max(rollSim,na.rm=TRUE),max(rollObs,na.rm=TRUE)),na.rm=TRUE)
    y.range=c(y.min,y.max)
    
    #BLANK PLOT
    plot(1,type="n",xlim=c(1,length(obsTS)),ylim=y.range,ylab=varName,xlab="Indx",xaxs="i")
    
    graphics::lines(x=seq(1,length(obsTS)),rollObs, col="blue", lwd=2) #add obs timeseries
    graphics::lines(x=seq(1,length(simTS)),rollSim, col="red", lwd=2)  #add sim timeseries
  }
  
  #ADD LEGEND
  if(asRollAv==FALSE){
    graphics::legend("topright",legend = c("sim.","obs."),horiz = FALSE,
                     col=c("red","blue"),lwd = c(2,2),bty="n" )
  }else{
    graphics::legend("topright",legend = c("sim. 30 rolling Av.","obs. 30 rolling Av."),horiz = FALSE,
                     col=c("red","blue"),lwd = c(2,2),bty="n" )
    title(paste("Daily moving average:", period,"day period used",sep=" "))
  } 
}

measure.diff<- function(type=NULL,
                        simPt=NULL,
                        targetPt=NULL,
                        pc.lim=c(5,10),
                        diff.lim=c(0.05,0.1)
                        ){
  #Determine difference and class lims
  switch(type,
         "pc" = {diff.att=pc.calc(sim=simPt,target=targetPt)
                class.lim=pc.lim
                },
         "frac" = {diff.att=pc.calc(sim=simPt,target=targetPt)
                  class.lim=pc.lim
                  },
         "diff" = {diff.att=abs.diff.calc(sim=simPt,target=targetPt)
                  class.lim=diff.lim
                  },
                 {diff.att=pc.calc(sim=simPt,target=targetPt)
                 class.lim=pc.lim}
  )
  out=list(class.lim=class.lim, diff.att=diff.att)
  return(out)
}


trafficAttPlot<-function(attSel=NULL,
                         attPrim=NULL,
                         simPt=NULL,     #simPt in targetType space
                         target=NULL,    #target in targetType space
                         targetType=NULL,
                        ...
){
  pc.lim=c(5,10)             #SET FAIR AT 5-10 AND POOR 10+
  diff.lim=c(0.5,1)
  
  #setup layout as max 4 columns
  col.plot=4
  # n.row=ceiling(length(attSel)/col.plot)
  # #determine if any padding needed
  # nlast=length(attSel)%%col.plot
  # if(nlast==0){nlast=4} #update nlast (if evenly divisible)
  # npad=col.plot-nlast
    
  #par(mfrow=c(n.row,4),oma=c(2,2,2,2),mar=c(3,3,1,1),xpd=TRUE) #xpd=TRUE allow margin plotting
  par(mfrow=c(3,col.plot),oma=c(2,2,2,2),mar=c(3,3,1,1),xpd=TRUE) #xpd=TRUE allow margin plotting
  
  #get indices of primary attributes
  if(length(attPrim)>0){
    get.ind=function(x,y){which(x == y)}
    indPrim=vapply(attPrim,FUN=get.ind,FUN.VALUE=numeric(1),x=attSel,USE.NAMES = FALSE)  #Indices of primary attributes
  }else{
    indPrim=integer(0)
    indSec=seq(1,length(attSel))   #all atributes are secondary
  }
  

  if (!identical(indPrim, integer(0))){
    #SET UP SECONDARY ATTRIBUTE TREATMENT
    nSec=length(attSel)-length(attPrim)    # no. of secondary attributes
    indSec=seq(1,length(attSel))
    
    if(!identical(indPrim, integer(0))){
      indSec=indSec[-indPrim]              # adjust secondary attribute indices 
    }

    #PLOT PRIMARY ATTRIBUTES
    for (i in 1:length(indPrim)){
      # GET PERCENT DIFF or ABS DIFF
      mdiff=measure.diff(type=targetType[indPrim[i]],
                   simPt=simPt[indPrim[i]],
                   targetPt=target[indPrim[i]],
                   pc.lim=pc.lim,
                   diff.lim=diff.lim)

      #PLOT PERCENT/REL DIFF - PERFROMANCE CATEGORY INDICATED BY COLOUR
      plot.attrib.perf.solo(rel.diff=mdiff$diff.att,
                            att.name=attSel[indPrim[i]],
                            perf.lim=mdiff$class.lim,
                            prim.lab="Primary",
                            targetType=targetType[indPrim[i]])
       
    }
  }
  
  if(!identical(indSec, integer(0))){
    #PLOT SECONDARY ATTRIBUTES
    for (i in 1:length(indSec)){
      # GET PERCENT DIFF or ABS DIFF
      mdiff=measure.diff(type=targetType[indSec[i]],
                         simPt=simPt[indSec[i]],
                         targetPt=target[indSec[i]],
                         pc.lim=pc.lim,
                         diff.lim=diff.lim)
      
      #PLOT PERCENT/REL DIFF - PERFROMANCE CATEGORY INDICATED BY COLOUR
      plot.attrib.perf.solo(rel.diff=mdiff$diff.att,
                            att.name=attSel[indSec[i]],
                            perf.lim=mdiff$class.lim,
                            prim.lab="",
                            targetType=targetType[indSec[i]])
    }
  }
  #PADDING PLOTS
  # if(npad>0){
  #   for (i in 1:npad){
  #     plot(1,type="n",ylab="",xlab="",xaxt="n",yaxt="n",frame.plot = FALSE)
  #   }
  # }
}
#TESTER
# windows(height=8.72,width=11.9)
# trafficAttPlot(attSel=c("P_ann_tot_m","P_ann_dyWet_m","P_ann_DSD_m","P_ann_dyWet_99p"),
#                attPrim=c("P_ann_tot_m"),
#                simPt=c(100,100,101,53),
#                target=c(101,101,120,55),
#                targetType=c("pc","diff","pc","frac")
#                  )

simTS.overlayMonthlyObsRange<-function(obsDat=NULL,     #obsData
                                       simTS=NULL,      #simulated timeseries
                                       datInd=NULL,     #date index
                                       label=NULL,       #plot labels
                                       range.mult=0.5
){
  
  #MAKE POLYGON OF MONTHLY RANGES
  Tag="mon_min_dyAll"
  tmp.min=rep(0,datInd$ndays);for(m in 1:12){tmp.min[datInd$i.mm[[m]]]= obsDat[[Tag]]$mean[m]}
  Tag="mon_max_dyAll"
  tmp.max=rep(0,datInd$ndays);for(m in 1:12){tmp.max[datInd$i.mm[[m]]]= obsDat[[Tag]]$mean[m]}
  cord.y=c(tmp.min[1:datInd$ndays],tmp.max[datInd$ndays:1],tmp.min[1])
  cord.x=c(seq(1,datInd$ndays),seq(datInd$ndays,1),1)
  
  
  # windows(width=30,height=9)
  par(xpd=FALSE)
  # MAKE PLOTTING SPACE
  plot(1,type="n",xlim=c(1,datInd$ndays),ylim=c(min(simTS),max(simTS)*range.mult),ylab=label,xlab="days",xaxs="i")
  
  # ADD MONTHLY POLYGON
  polygon(x=cord.x,y=cord.y,col = "lemonchiffon1",border = NA)
  
  # ADD SIMULATED TIMESERIES
  lines(x=seq(1,datInd$ndays),y=simTS[1:datInd$ndays],col="black")  #add rain time series
  
  #ADD OBS MEAN AS OVERLAY  
  Tag="mon_mean_dyAll"
  dymean.mon=rep(0,datInd$ndays)
  for(m in 1:12){dymean.mon[datInd$i.mm[[m]]]= obsDat[[Tag]]$mean[m]}
  lines(x=seq(1,datInd$ndays),y=dymean.mon,col="red",lwd=2)              
  
  legend("topleft",legend = c("sim.","obs. mean (mon)","obs. range (mon)"),horiz = FALSE,seg.len = c(1,1,1),
         col=c("black","red",NA),fill = c(NA,NA,"lemonchiffon1"),border = c(NA,NA,"black"),lwd = c(1,1,1),bg="white")
}

#-------------------------------
#exposure space visuals 2d or gridded layout
expSpace2dViz<-function(x=NULL,    #vector of one attribute
                        y=NULL,    #vector of one attribute
                        x.lab=NULL,
                        y.lab=NULL
  
){
  #determine ranges
  y.range=c(min(y,na.rm=TRUE),max(y,na.rm=TRUE))
  x.range=c(min(x,na.rm=TRUE),max(x,na.rm=TRUE))
  
  #plot
  plot(x=x,y=y,type="p",pch=16,col="blue",xlab=x.lab,ylab=y.lab,xlim=x.range,ylim=y.range,frame.plot=FALSE)
}
#tester
# expSpace2dViz(x=c(1,2,3,4,1,2,3,4,1,2,3,4),y=c(1,1,1,1,2,2,2,2,3,3,3,3),x.lab="Ptot%",y.lab="Temp(+deg)")


#-------------------------------------------------
frontBoilerPlateInfo<-function(modelTag=NULL,
                               targetLocn=NULL,
                               spot=NULL,
                               nTarget=NULL,
                               attSel=NULL,
                               attPrim=NULL,
                               optimArgs=NULL,
                               sim=NULL,
                               simVar=NULL
                              ){
  #text col
  t.col="dodgerblue3"

  # MAKE BLANK UN-BORDERED PLOTTING SPACE
  par(mar=c(1,1,1,1),oma=c(1,1,1,1))
  plot(1,type="n",xlim=c(1,100),ylim=c(1,100),ylab="",xlab="",xaxs="i",xaxt="n",yaxt="n",frame.plot=FALSE,xpd=FALSE)
  
  #THINGS TO PLOT IN ALL CASES
  polygon(c(0,100,100,0,0),
          c(93,93,118,118,93),
          border=FALSE,
          col=adjustcolor("green3",alpha.f=0.1))
  
  line.no=0
  #TARGET INFORMATION 
  text(x = 50,y=(98-line.no*10),labels=paste("Target", spot, "of",nTarget,sep=" " ),cex=2.5,col=t.col)
  nacross=4
  if(length(attSel)>nacross){
    nlot=ceiling(length(attSel)/4)   #split into lots of 3
    nrem=length(attSel)%%nacross           #get remainder
    line.no=line.no+0.5
    for(i in 1:nlot){
      line.no=line.no+0.5
      nstart=(i-1)*nacross+1
      if((i == nlot)&(nrem>0)){nfin=nstart+(nrem-1) }else{ nfin=nstart+(nacross-1)}
      chunk=seq(nstart,nfin)
      if(i == 1){
        text(x=50,y=(100-line.no*10),labels=paste("Target location:",paste(attSel[chunk],": ",targetLocn[chunk],sep="",collapse = ",    "),sep=" "),font = 2,col=t.col)
      }else{
        text(x=50,y=(100-line.no*10),labels=paste(attSel[chunk],": ",targetLocn[chunk],sep="",collapse = ",    "),font = 2,col=t.col)  #cap at 3 across
      }
    }
  }else{
    line.no=line.no+1.0
    text(x=50,y=(100-line.no*10),labels=paste("Target location:",paste(attSel,": ",targetLocn,sep="",collapse = ",    "),sep=" "),font = 2,col=t.col)
  }
  
  #PRIMARY ATTRIBUTES (IF ANY)
  if(!is.null(attPrim)){
    line.no=line.no+0.75
    text(x = 50,y=(100-line.no*10),labels=paste("Primary attributes:",paste(attPrim,collapse=",  ")),cex=1.0,font=2,col=t.col)
  }
  
  #RUN INFORMATION
  line.no=line.no+1
  text(x = 50,y=(100-line.no*10),labels=paste("Simulation run with the following properties"),cex=2.0,col=t.col)
  
  line.no=line.no+0.75
  text(x = 50,y=(100-line.no*10),labels=paste("Models Used:",paste(modelTag,collapse=",  ")),cex=1.0,font=2,col=t.col)
  
  line.no=line.no+0.5
  text(x = 50,y=(100-line.no*10),labels=paste("Variables perturbed:",paste(simVar,collapse=",  ")),cex=1.0,font=2,col=t.col)
  
  #THINGS TO PLOT IF STOCHASTIC SIMULATION USED
  if(modelTag[1] != "Simple-ann"){
    line.no=line.no+0.5
    text(x=50,y=(100-line.no*10),labels="Optimisation used: GA",font = 2,col=t.col) 
    line.no=line.no+0.5
    text(x=50,y=(100-line.no*10),labels=paste("Max no. iterations:",optimArgs$maxiter,sep=" "),font = 2,col=t.col)
    line.no=line.no+0.5
    text(x=50,y=(100-line.no*10),labels=paste("Crossover:",optimArgs$pcrossover,sep=" "),font = 2,col=t.col)
    line.no=line.no+0.5
    text(x=50,y=(100-line.no*10),labels=paste("Mutation:",optimArgs$pmutation,sep=" "),font = 2,col=t.col)
    line.no=line.no+0.5
    text(x=50,y=(100-line.no*10),labels=paste("Population size:",optimArgs$popSize,sep=" "),font = 2,col=t.col)
    line.no=line.no+0.5
    text(x=50,y=(100-line.no*10),labels=paste0("Lambda(",attPrim,"): ",optimArgs$lambda.mult,collapse = ", "),font = 2,col=t.col)
  }else{
    line.no=line.no+0.5
    text(x=50,y=(100-line.no*10),labels="Simple scaling used",font = 2,col=t.col) 
  }
  
  #BOTTOM BORDER POLYGON
  polygon(c(0,100,100,0,0),
          c(0,0,5,5,0),
          border=FALSE,
          col=adjustcolor("green3",alpha.f=0.1))
  
}


#-----------------------------------------------------------------------------
exposureSlices<-function(targetMat=NULL,
                         attSel=NULL
  
){
  
  par(mfrow=c(2,3),mar=c(4,4,2,2),oma=c(1,1,1,1))  # try and keep as square as possible
  for(i in 1:ncol(targetMat)){                        # do all combinations
    for(j in 1:(ncol(targetMat)-1)){
      k=j+1
      for(jj in k:ncol(targetMat)){
        if(i!=jj){                                       #don't plot attributes v the same attribute
          expSpace2dViz(x=targetMat[,i],                #vector of attribute_1
                        y=targetMat[,jj],                #vector of one attribute_2
                        x.lab=attSel[i],   #Name of attribute_1
                        y.lab=attSel[jj]    #Name of attribute_2
          )
        }
      }

    }
  }
  

}






