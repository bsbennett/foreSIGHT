#################################
##   SUMMARY PLOTTING WRAPPER  ##
#################################


#CONTAINS
  # plotSummary()
  # exposureSummary()
  # collateDat()

#NOT ALL VARS WILL NEED ALL PLOTS
# SOME JOINT PLOTTING WLL BE REQUIRED
#START WITH TRAFFIC LIGHT PLOTTING
#MORE DETAILED PLOTTING
#SOME TABLES?


#DAILY AMOUNTS TAKEN IN BRACKET
#metricTagList=c("dyAll","tot","totCorrel","dyCorrel","dyWet","nWet")
#plus - SOMETHING FOR EXTREMES OTHER THAN MAXIMA
#something for w/d-spells
  
plotSummary<-function(obs=NULL,
                      sim=NULL,
                      simVar=NULL,
                      datInd=NULL,
                      attSel=NULL,
                      attPrim=NULL,
                      simTarget=NULL,
                      target=NULL,
                      targetType=NULL,
                      modelTag=NULL,
                      optimArgs=NULL
){
  

  nTarget=nrow(target)  
    
  # DO EVERYTHING FOR OBSERVED SERIES ONCE
  obsDat=list()
  for(i in 1:length(simVar)){
    plotVar=simVar[i]            #select variable to evaluate
    obsDat[[plotVar]]=collateDat(TS=obs[[plotVar]],datInd=datInd[["obs"]],plotVar=plotVar)
  }
  
  #LOOP OVER TARGETS
  for(i in 1:nTarget){
    simDat=list()
    
    #PLOT STUFF TO A PDF
    #LABEL A PDF
    fnam=nameMaker(attSel=attSel,target=target[i,])  #make filename
    fnam=paste(fnam,".pdf",sep="")
    pdf(file=fnam,height=8.27,width=11.69)   #landscape a4 page
    par(mar=c(3,5,3,3),oma=c(2,2,2,2))
    
    #FRONT BOILERPLATE INFO
    frontBoilerPlateInfo(modelTag=modelTag,
                         targetLocn=target[i,],
                         spot=i,
                         nTarget=nTarget,
                         attSel=attSel,
                         attPrim=attPrim,
                         optimArgs=optimArgs,
                         sim=sim,
                         simVar=simVar
    )
    
    #TRAFFIC LIGHT PLOT HERE
    if(modelTag[1] != "Simple-ann"){
      trafficAttPlot(attSel=attSel,attPrim=attPrim,simPt=simTarget[i,],target=target[i,],targetType=targetType)
    }
    
    #SET LAYOUT - 2 ROWS, 1 COLUMN
    par(mfrow=c(2,1),xaxs="i")
    par(mar=c(3,5,3,3),oma=c(3,5,3,3),xpd=FALSE)
    for(mod in 1:length(simVar)){
      plotVar=simVar[mod] 
      
      switch(modelTag[1],
        "Simple-ann" = {simTest=as.vector(unlist(sim[[i]][plotVar]))},
                       {simTest=sim[[i]][[plotVar]]$sim}
      )
    
      if(plotVar == "P"){mult=0.5}else{mult=1.05}
      simTS.overlayMonthlyObsRange(obsDat=obsDat[[plotVar]],simTS=simTest,datInd=datInd[["obs"]],label=plotVar,range.mult=mult)
    }
  
    for(mod in 1:length(simVar)){
      
      plotVar=simVar[mod]            #select variable to evaluate
      switch(modelTag[1],
             "Simple-ann" = {simTest=as.vector(unlist(sim[[i]][plotVar]))},
                            {simTest=sim[[i]][[plotVar]]$sim}
      )
      
      simDat[[plotVar]]=collateDat(TS=simTest,datInd=datInd[[modelTag[mod]]],plotVar=plotVar)
      
      #print(obsDat[[plotVar]][["ann_count_nWet"]])
     # print(simDat[[plotVar]][["ann_count_nWet"]])
      
      #SET LAYOUT - 2 ROWS, 1 COLUMN
      par(mfrow=c(2,1),xaxs="i",xpd=FALSE)
      par(mar=c(3,5,3,3),oma=c(3,5,3,3))
      #REGULAR PLOTS
      #monthwise batch
      runTag="mon_mean_dyAll"; lab=paste(plotVar,": daily mean", sep="")
      monthwise.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)
      
      runTag="mon_sd_dyAll"; lab=paste(plotVar,": daily sd", sep="")
      monthwise.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab) 
      
      runTag="mon_sum_dyAll"; lab=paste(plotVar,": total", sep="")
      monthwise.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab) 
      
      if(plotVar == "P"){
        runTag="mon_mean_dyWet"; lab=paste(plotVar,": daily wet mean", sep="")
        monthwise.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab) 
        
        runTag="mon_sd_dyWet"; lab=paste(plotVar,": daily wet sd", sep="")
        monthwise.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab) 
        
        # runTag="mon_mean_nWet"; lab=paste(plotVar,": no. wet days mean", sep="")
        # monthwise.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)

        runTag="mon_count_nWet"; lab=paste(plotVar,": no. wet days", sep="")
        monthwise.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)
      }
  
      
      #seasonal batch
      runTag="seas_mean_dyAll"; lab=paste(plotVar,": daily mean", sep="")
      seasonal.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)
      
      runTag="seas_sd_dyAll"; lab=paste(plotVar,": daily sd", sep="")
      seasonal.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)
      
      runTag="seas_sum_dyAll"; lab=paste(plotVar,": total", sep="")
      seasonal.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)
      
      # # 
      # if(plotVar == "P"){
      # #   runTag="seas_mean_dyWet"; lab=paste(plotVar,": daily wet mean", sep="")
      # #   monthwise.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab) 
      # #   
      # #   runTag="seas_sd_dyWet"; lab=paste(plotVar,": daily wet sd", sep="")
      # #   monthwise.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab) 
      # #   
      #   runTag="seas_mean_nWet"; lab=paste(plotVar,": no. wet days mean", sep="")
      #   monthwise.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)
      # 
      #   runTag="seas_sd_nWet"; lab=paste(plotVar,": no. wet days sd", sep="")
      #   monthwise.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)
      # }
      
      #annual batch
      #change 
      par(mfrow=c(2,3),xaxs="i",xpd=FALSE)  #assuming a4 landscape layout
      par(mar=c(3,5,3,3),oma=c(3,5,3,3))
      
      runTag="ann_mean_dyAll"; lab=paste(plotVar,": daily mean", sep="")
      annual.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab) 
      
      runTag="ann_sd_dyAll"; lab=paste(plotVar,": daily sd", sep="")
      annual.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab) 
      
      runTag="ann_sum_dyAll"; lab=paste(plotVar,": total", sep="")
      annual.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab) 
      
      if(plotVar == "P"){
        runTag="ann_mean_dyWet"; lab=paste(plotVar,": daily wet mean", sep="")
        annual.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab) 
        runTag="ann_sd_dyWet"; lab=paste(plotVar,": daily wet sd", sep="")
        annual.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab) 
        # runTag="ann_mean_nWet"; lab=paste(plotVar,": no. wet days mean", sep="")
        # annual.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)
        runTag="ann_count_nWet"; lab=paste(plotVar,": no. wet days", sep="")
        annual.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)
        
      }
      
    }
    dev.off()  #STOP PLOTTING TO PDF
  }

}

collateDat<-function(TS=NULL,
                     datInd=NULL,
                     plotVar=NULL
                     ){
  statTag=c("mean","sd","max","min","sum","count")
  aggTag=c("mon","ann","seas")
  metricTag=c("dyAll","dyWet","nWet")  
  
  outDat=list()
  
  #GENERAL STATISTICS
  k=1
  for(i in 1:3){
    for(j in 1:5){
      runTag=paste(aggTag[i],statTag[j],metricTag[k],sep="_")  #will need amending
      evalFunc=eval(parse(text = statTag[j]))
      switch(aggTag[i],
             "mon" = {outDat[[runTag]]=monthwise.extract.func(f=evalFunc,dat=TS,datInd=datInd,stat.func=stat.func,funcSel=funcSel[1:5],na.rm=TRUE)},
             "ann" = {outDat[[runTag]]=annual.extract.func(f=evalFunc,dat=TS,datInd=datInd,stat.func=stat.func,funcSel=funcSel[1:5],na.rm=TRUE)},
             "seas" = {outDat[[runTag]]=seasonal.extract.func(f=evalFunc,dat=TS,datInd=datInd,stat.func=stat.func,funcSel=funcSel[1:5],na.rm=TRUE)},
             -999.00 )
    }  #stat loop
  } #agg loop
  
  #RAINFALL SPECIFIC STATISTICS
  if(plotVar == "P"){   #only if rainfall
    k=2 
    for(i in 1:3){
      for(j in 1:2){
        runTag=paste(aggTag[i],statTag[j],metricTag[k],sep="_")  #will need amending
        switch(statTag[j],
               "mean" = {evalFunc=get.wet.average},
               "sd"  = {evalFunc=get.wet.sd},
               {evalFunc=eval(parse(text = statTag[j]))}
        )
        switch(aggTag[i],
               "mon" = {outDat[[runTag]]=monthwise.extract.func(f=evalFunc,dat=TS,datInd=datInd,stat.func=stat.func,funcSel=funcSel[1:5],threshold=0)},
               "ann" = {outDat[[runTag]]=annual.extract.func(f=evalFunc,dat=TS,datInd=datInd,stat.func=stat.func,funcSel=funcSel[1:5],threshold=0)},
               "seas" = {outDat[[runTag]]=seasonal.extract.func(f=evalFunc,dat=TS,datInd=datInd,stat.func=stat.func,funcSel=funcSel[1:5],threshold=0)},
               -999.00 )
      }  #stat loop
    } #agg loop
    
    k=3; j=6
    for(i in 1:3){
        runTag=paste(aggTag[i],statTag[j],metricTag[k],sep="_")  #will need amending
        evalFunc=get.nwet
        switch(aggTag[i],
               "mon" = {outDat[[runTag]]=monthwise.extract.func(f=evalFunc,dat=TS,datInd=datInd,stat.func=stat.func,funcSel=funcSel[1:5],threshold=0)},
               "ann" = {outDat[[runTag]]=annual.extract.func(f=evalFunc,dat=TS,datInd=datInd,stat.func=stat.func,funcSel=funcSel[1:5],threshold=0)},
               "seas" = {outDat[[runTag]]=seasonal.extract.func(f=evalFunc,dat=TS,datInd=datInd,stat.func=stat.func,funcSel=funcSel[1:5],threshold=0)},
               -999.00 )
    }

  } #end rain loop
  return(outDat)
}

#--------------------------------------------------------
exposureSummary<-function(targetMat=NULL,
                          attSel=NULL,
                          paths=paths
                          ){
  fnam=paste0(paths$Plots,"/exposureSpace.pdf")
  pdf(file=fnam,height=8.27,width=11.69)   #landscape a4 page
  par(mar=c(3,5,3,3),oma=c(2,2,2,2))
  exposureSlices(targetMat=targetMat,attSel=attSel)
  dev.off()
}


devPlotSummary<-function(obs=NULL,
                      sim=NULL,
                      simVar=NULL,
                      datInd=NULL,
                      attSel=NULL,
                      attPrim=NULL,
                      simTarget=NULL,
                      target=NULL,
                      targetType=NULL,
                      modelTag=NULL,
                      optimArgs=NULL,
                      id=NULL,
                      nTarget=NULL,
                      IOmode=NULL,
                      paths=paths
){
 
  # DO EVERYTHING FOR OBSERVED SERIES ONCE
  obsDat=list()
  for(i in 1:length(simVar)){
    plotVar=simVar[i]            #select variable to evaluate
    obsDat[[plotVar]]=collateDat(TS=obs[[plotVar]],datInd=datInd[["obs"]],plotVar=plotVar)
  }
  
  #LOOP OVER TARGETS
  
    simDat=list()
    
    
    #LABEL A PDF
    switch(IOmode,
           "dev"={ fnam=paste("summary",id,".pdf",sep="")},
                 { fnam=nameMaker(attSel=attSel,target=target)  #make filename
                   fnam=paste(paths$Diagnostics,"/",fnam,".pdf",sep="")}
           )
    #PLOT STUFF TO A PDF
    pdf(file=fnam,height=8.27,width=11.69)   #landscape a4 page
    par(mar=c(3,5,3,3),oma=c(2,2,2,2))
    
    #FRONT BOILERPLATE INFO
    frontBoilerPlateInfo(modelTag=modelTag,
                         targetLocn=target,
                         spot=id,
                         nTarget=nTarget,
                         attSel=attSel,
                         attPrim=attPrim,
                         optimArgs=optimArgs,
                         sim=sim,
                         simVar=simVar
    )
    
    #TRAFFIC LIGHT PLOT HERE
    if(modelTag[1] != "Simple-ann"){
      trafficAttPlot(attSel=attSel,attPrim=attPrim,simPt=simTarget,target=target,targetType=targetType)
    }
    
    #SET LAYOUT - 2 ROWS, 1 COLUMN
    par(mfrow=c(2,1),xaxs="i")
    par(mar=c(3,5,3,3),oma=c(3,5,3,3),xpd=FALSE)
    for(mod in 1:length(simVar)){
      plotVar=simVar[mod]

      switch(modelTag[1],
             "Simple-ann" = {simTest=as.vector(unlist(sim[plotVar]))},
             {simTest=sim[[plotVar]]$sim}
      )

      if(plotVar == "P"){mult=0.5}else{mult=1.05}
      simVobsTS(simTS=simTest[1:1460],obsTS=obs[[plotVar]][1:1460],datInd=datInd[["obs"]],varName=plotVar,asRollAv=TRUE) #datInd not used
      
      simTS.overlayMonthlyObsRange(obsDat=obsDat[[plotVar]],simTS=simTest,datInd=datInd[["obs"]],label=plotVar,range.mult=mult)
    }

    for(mod in 1:length(simVar)){

      plotVar=simVar[mod]            #select variable to evaluate
      switch(modelTag[1],
             "Simple-ann" = {simTest=as.vector(unlist(sim[plotVar]))},
             {simTest=sim[[plotVar]]$sim}
      )

      simDat[[plotVar]]=collateDat(TS=simTest,datInd=datInd[[modelTag[mod]]],plotVar=plotVar)

      #print(obsDat[[plotVar]][["ann_count_nWet"]])
      # print(simDat[[plotVar]][["ann_count_nWet"]])

      #SET LAYOUT - 2 ROWS, 1 COLUMN
      par(mfrow=c(2,1),xaxs="i",xpd=FALSE)
      par(mar=c(3,5,3,3),oma=c(3,5,3,3))
      #REGULAR PLOTS
      #monthwise batch
      runTag="mon_mean_dyAll"; lab=paste(plotVar,": daily mean", sep="")
      monthwise.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)

      runTag="mon_sd_dyAll"; lab=paste(plotVar,": daily sd", sep="")
      monthwise.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)

      runTag="mon_sum_dyAll"; lab=paste(plotVar,": total", sep="")
      monthwise.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)

      if(plotVar == "P"){
        runTag="mon_mean_dyWet"; lab=paste(plotVar,": daily wet mean", sep="")
        monthwise.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)

        runTag="mon_sd_dyWet"; lab=paste(plotVar,": daily wet sd", sep="")
        monthwise.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)

        # runTag="mon_mean_nWet"; lab=paste(plotVar,": no. wet days mean", sep="")
        # monthwise.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)

        runTag="mon_count_nWet"; lab=paste(plotVar,": no. wet days", sep="")
        monthwise.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)
      }


      #seasonal batch
      runTag="seas_mean_dyAll"; lab=paste(plotVar,": daily mean", sep="")
      seasonal.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)

      runTag="seas_sd_dyAll"; lab=paste(plotVar,": daily sd", sep="")
      seasonal.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)

      runTag="seas_sum_dyAll"; lab=paste(plotVar,": total", sep="")
      seasonal.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)

      #annual batch
      #change
      par(mfrow=c(2,3),xaxs="i",xpd=FALSE)  #assuming a4 landscape layout
      par(mar=c(3,5,3,3),oma=c(3,5,3,3))

      runTag="ann_mean_dyAll"; lab=paste(plotVar,": daily mean", sep="")
      annual.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)

      runTag="ann_sd_dyAll"; lab=paste(plotVar,": daily sd", sep="")
      annual.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)

      runTag="ann_sum_dyAll"; lab=paste(plotVar,": total", sep="")
      annual.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)

      if(plotVar == "P"){
        runTag="ann_mean_dyWet"; lab=paste(plotVar,": daily wet mean", sep="")
        annual.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)
        runTag="ann_sd_dyWet"; lab=paste(plotVar,": daily wet sd", sep="")
        annual.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)
        # runTag="ann_mean_nWet"; lab=paste(plotVar,": no. wet days mean", sep="")
        # annual.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)
        runTag="ann_count_nWet"; lab=paste(plotVar,": no. wet days", sep="")
        annual.boxplots(simDat=simDat[[plotVar]][[runTag]],obsDat=obsDat[[plotVar]][[runTag]],compObs=TRUE, metricTag=lab)

      }

    }
    dev.off()  #STOP PLOTTING TO PDF
  
  
}


