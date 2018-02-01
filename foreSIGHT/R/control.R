#---------------------------------------------------------------------------------------------------------------------------------------
scenarioGenerator<-function(obs=NULL,                           # data frame of observed data with column names compulsary [$year, $month, $day, $P,] additional [$Temp, $RH, $PET, $uz, $Rs] (or a subset of these)
                              modelTag=NULL,                    # scalar or vector of models to use
                              attPerturb=NULL,                  # vector of perturbed attributes
                              attHold=NULL,                     # vector of held attributes
                              attPenalty=NULL,                  # vector of primary attributes (a penalty function is used to prioritise this subset of selected attributes)
                              optimArgs=list(pcrossover= 0.8,   # list of a parameters used by the ga optimiser (if used)
                                             pmutation=0.1,
                                             maxiter=10,
                                             maxFitness=-0.001,
                                             popSize = 100,
                                             run=20,
                                             seed = 1234,
                                             parallel = FALSE,
                                             keepBest=TRUE,
                                             lambda.mult=0.0,
                                             suggestions=NULL
                                             ),
                              exSpArgs=list(),
                              simLengthNyrs=NULL,
                              IOmode="verbose",     # "dev","suppress","verbose"
                              arrayID=NULL,         # only used in "dev" mode
                              nSeed=NULL,           # No. of seeds
                              seedID=1234,          #'fixed' for 1-10, 'arrayID' for array id or a number to be held at that.
                              simDirectory="Simulation1"
  ){
    if(IOmode=="verbose"){
      path<-file.path(".",simDirectory)
      # if(dir.exists(path)==TRUE){
      #   query<-readline(prompt="Warning: This simulation directory already exists. Previous scenarios will be overridden. Proceed? (Y/N)")
      #   if(query=="Y"){
      #     
      #   }else{
      #     stop("User stopped due to specified directory")
      #   }
      # }
      if(!isTRUE(dir.exists(path))){
        dir.create(path)
      }
      pathScenario<-file.path(path,"Scenarios")
      pathDiagnostics<-file.path(path,"Diagnostics")
      pathPlots<-file.path(path,"Plots")
      pathLogfile<-file.path(path,"Logfile")
      pathMetadata<-file.path(path,"ScenarioMetadata")
      pathCSV<-file.path(pathScenario,"CSV")
      pathRData<-file.path(pathScenario,"RData")
      
      paths<-list(Scenario=pathScenario,
                  Diagnostics=pathDiagnostics,
                  CSV=pathCSV,
                  RData=pathRData,
                  Logfile=pathLogfile,
                  Plots=pathPlots,
                  Metadata=pathMetadata)
      
      if(!isTRUE(dir.exists(pathLogfile))){
        dir.create(pathLogfile)
      }
    
    }
  
   #IOmode == "verbose"
    if(IOmode=="verbose"){
      file<-filename(IOmode=IOmode,arrayID=arrayID)
      file<-paste0(paths$Logfile,"/",file)
    } else{
      file<-filename(IOmode=IOmode,arrayID=arrayID)
    }
    
    #Form Arguments
    arguments<-getArguments(attPerturb=attPerturb,attHold=attHold,optimArgs=optimArgs,exSpArgs=exSpArgs)
    attSel=arguments$attSel
    exSpArgs=arguments$exSpArgs
    optimArgs=arguments$optimArgs
    attPrim=attPenalty
    
    #CHECK FOR ARGUMENTS
    banner("CHECK FOR ARGUMENT INPUTS",file)
    progress("Checking argument inputs...",file)

    arg_check<-argument_check(names=names(obs),
                              obs=obs,
                              attSel=attSel,
                              attPrim=attPrim,
                              attHold=attHold,
                              attPerturb=attPerturb,
                              modelTag=modelTag,
                              optimArgs=optimArgs,
                              exSpArgs=exSpArgs,
                              file=file)


    progress("Argument input format OK",file)
    
    #CHECK FOR LOGIC IN ATTRIBUTE MODEL COMBOS
    banner("CHECK FOR MODEL AND ATTRIBUTE COMBINATIONS",file)
    progress("Checking model and attribute combinations...",file)
    
    argument_logic_check(names=names(obs),
                         attSel=attSel,
                         attPrim=attPrim,
                         modelTag=modelTag,
                         file=file)
    
    progress("Model and attribute combinations OK",file)
    
    #CHECK FOR INPUTS
    banner("CHECK FOR DATAFRAME INPUT",file)
    progress("Checking dataframe input...",file)
    inputcheck<-input_check(obs,file,simLengthNyrs)
    progress("Dataframe input OK",file)

    #GET ADDITIONAL MODEL INFO, ATT INFO & SORT (make into separate script/functions)
    nMod=length(modelTag)   
    if(nMod==1){
      modelInfo=list()
      modelInfo[[modelTag[1]]]=get.model.info(modelTag[1])                     #even if 1 model still stored in list format
    }else{
      modelInfo=sapply(X = modelTag,FUN=get.model.info,USE.NAMES=TRUE)
    }
    
    simPriority=sort(sapply(X=modelInfo,FUN=return.simPriority,USE.NAMES=TRUE)) #get simulation priority of each model
    modelTag=names(simPriority)                                                 # Force simVar="P" to come first via sorting by $simPrority
    simVar=sapply(X=modelInfo[modelTag],FUN=return.simVar,USE.NAMES=TRUE)       #???CREATE MODEL MASTER INFO - HIGHER LEVEL?
    attInfo=attribute.info.check(attSel=attSel)                                 # vector of selected attributes (strings)
    if(modelTag[1] == "Simple-ann"){simVar=attInfo$varType}
    attInd=get.att.ind(attInfo=attInfo,simVar=simVar)
    attInfo=update.att.Info(attInfo=attInfo,attInd=attInd,modelTag=modelTag,simVar=simVar) #add extra level for easier model mangmt
    if(modelTag[1] != "Simple-ann"){nParTot=0;for(i in 1:nMod){nParTot=nParTot+modelInfo[[i]]$npars}}                      #total number of pars
    
    #GET DATES DATA (and indexes for harmonic periods)
    banner("INDEXING DATES",file)
    progress("Indexing dates...",file)
    obs=inputcheck$data                                      # USE NEW APPENDED/CHECKED DATA
    yy=obs$year;mm=obs$month;dd=obs$day                      # STORE DATE VECTORS
    
    datInd=list()
    datInd[["obs"]]=get.date.ind(dd=dd,mm=mm,yy=yy,nperiod=12,southHemi=TRUE)              #make obs based datInd
    
    #EXTEND DATES IF NEEDED
    if(!is.null(simLengthNyrs)){
      if(modelTag[[1]] != "Simple-ann"){
        dateExtnd=extendDates(simLengthNyrs=simLengthNyrs,dd=dd,mm=mm,yy=yy)
      }else{
        dateExtnd=obs[,c("year","month","day")]                                              # make the same as observed
        progress("Length of time series cannot be increased using simple scaling",file)
      }
    }else{
      dateExtnd=obs[,c("year","month","day")]                                               # make the same as observed
    }
    
    for(i in 1:nMod){
      datInd[[modelTag[i]]]=get.date.ind(dd=dateExtnd$day,mm=dateExtnd$month,yy=dateExtnd$year,nperiod=modelInfo[[modelTag[i]]]$nperiod,southHemi=TRUE)          # FROM dateManager.R
    }
    
    #add on warmup period of 1 year if stochastic (fix this up!!)
    if(modelTag[[1]] != "Simple-ann"){
      datePlusWarm=extendDates(simLengthNyrs=(datInd[[modelTag[i]]]$nyr+1),dd=dateExtnd$day,mm=dateExtnd$month,yy=dateExtnd$year)
      datWarm=list()  #used for rainfall generation
      for(i in 1:nMod){
        datWarm[[modelTag[i]]]=get.date.ind(dd=datePlusWarm$day,mm=datePlusWarm$month,yy=datePlusWarm$year,nperiod=modelInfo[[modelTag[i]]]$nperiod,southHemi=TRUE)          # FROM dateManager.R
        datInd[[modelTag[i]]]$i.mod=datWarm[[modelTag[i]]]$i.pp
      }
      rm(datWarm)
    }

    
     progress("Dates indexed OK",file)
    
    #----------------------------------------
     #EXPOSURE SPACE GENERATION 
     banner("SAMPLING EXPOSURE SPACE",file)
     progress("Sampling exposure space targets...",file)
     
     spaceInfo=expSpaceSampManager(exSpArgs=exSpArgs,attInfo=attInfo,attSel=attSel,file=file,IOmode=IOmode,nSeed=nSeed,seedID=seedID,arrayID=arrayID)  #allows targetMat to be provided as CSV
     targetMat=spaceInfo$targetMat
     attRot=spaceInfo$attRot      #attRot - used if needed in "OAT" operations
     seedCatalogue=spaceInfo$seedCatalogue   #create list of seeds
     nTarget=dim(targetMat)[1]
 
     progress("Exposure space sampled OK",file)
     progress(p(nTarget," targets sampled across ", length(attInfo$varType)," attributes"),file) 
     
     #-------------------------------------
     #SIMPLE SCALING
     if(modelTag[[1]] == "Simple-ann"){
           banner("SIMPLE SCALE FOR OBSERVED DATA",file)
           progress("Scaling data...",file)
           sim=list()
           fnam=vector(length=nTarget)
           tarSeq=1:nTarget
           for (i in tarSeq) {
             sim[[i]]=simple.scaling(target=unlist(targetMat[i,]),
                                     targetType=attInfo$targetType,
                                     data=obs[simVar],
                                     varType=attInfo$varType,
                                     period=modelInfo[[modelTag[1]]]$nperiod,
                                     i.pp=datInd[[modelTag[1]]]$i.pp)

            #WRITE SIMPLE SCALING OUTPUT TO CSV & RDATA FILE
            if(IOmode=="verbose"){
              if(!isTRUE(dir.exists(pathScenario))){
                dir.create(pathScenario)
              }
              if(!isTRUE(dir.exists(pathCSV))){
                dir.create(pathCSV)
              }
              if(!isTRUE(dir.exists(pathRData))){
                dir.create(pathRData)
              }
              
              fnam[i]<-simpleSaveTarget(data=sim[[i]],
                                dates=obs[,c("year","month","day")],
                                simVar=simVar,
                                attSel=attSel,
                                target=targetMat[i,],
                                modelTag=modelTag[1],
                                modelInfo=modelInfo[1],
                                paths=paths)
               }
             }
           progress("Data scaled OK",file)

     }else{

          #----------------------------------------
          #STOCHASTIC SIMULATION

          #GET ATTRIBUTES OF OBSERVED DATA (testing attribute calc function)
          banner("OBSERVED BASELINE ATTRIBUTE CALCULATION",file)
          progress("Calculating attributes...",file)

          attObs=list()                            #make this into its own function (also inserted into model sequencer)
          for(i in 1:nMod){
            attObs[[i]]=attribute.calculator(attSel=attSel[attInd[[i]]],
                                             data=obs[[simVar[i]]],
                                             datInd=datInd[["obs"]],
                                             attribute.funcs=attribute.funcs)
          }
          attObs=unlist(attObs)

          progress(paste("Attributes of observed series - ",paste(attSel,": ",signif(attObs,digits=5),collapse = ", ",sep=""),sep=""),file)
          progress("Attributes calculated OK",file)   #NEED SOME ACTUAL CHECKING HERE BEFORE PRONOUNCING OK

          #PART 1: INITIAL CALIBRATION OF EACH MODEL
          #-INITIAL CALIBRATION OF SELECTED MODEL(S) - wrapper for alternate function
         # initCalibPars=init.calib(data=obs[[simVar[i]]],modelTag=modelTag[i],modelInfo=modelInfo[[modelTag[i]]],datInd=datInd[[modelTag[i]]]) #for rain model only

          #PART 2: OPTIMISING TO DETERMINE PARS
          #LOOP OVER EXPOSURE SPACE POINTS TO DETERMINE PARS
          banner("DETERMINING STOCHASTIC MODEL PARAMETERS FOR TARGETS",file)
          progress("Determining stochastic model parameters at each target location...",file)
          progress("Simulating stochastic time series...",file)
          progress("Starting cluster...",file)
      
          #DETERMINE WHICH PARS ATTACH TO WHICH MODEL (make this a function in stochParManager.R)
          parLoc=whichPars(simVar=simVar,modelInfo=modelInfo)
          
          #SCREEN INAPPROPRIATE SUGGESTIONS IF ANY
          if(!is.null(optimArgs$suggestions)){
            optimArgs$suggestions=screenSuggest(suggest=optimArgs$suggestions,modelInfo=modelInfo,modelTag=modelTag,parLoc=parLoc)
          }
          
          #MAKE SPACE FOR SIMULATION INFO
          simTarget=matrix(NA,nrow=nTarget,ncol=length(attSel))
          attValue=matrix(NA,nrow=nTarget,ncol=length(attSel))
          parSim=matrix(NA,nrow=nTarget,ncol=nParTot)
          fnam=rep(NA,nTarget)
          sim=list()                                   #STORE TIMESERIES, AND SIMULATED ATTRIBUTES
          
          #DETERMINE WHICH TARGETS TO SIMULATE
          switch(IOmode,
                 "dev" = {tarSeq=(arrayID:arrayID)},       # run through arrayID
                         {tarSeq=(1:nTarget)})             # loop over targets
          
          for(i in tarSeq){
            a<-Sys.time()
            progress(p("Working on scenario ",i," of ",nTarget),file)
            
            #IF "OAT" ROTATE attPrim
            if(is.character(exSpArgs)){
              attApp=attPrim
            } else {
              switch(exSpArgs$type,                  
                   "OAT" = {attApp=attRot[i]},  # rotate through attPrim
                           {attApp=attPrim}     # maintain attPrim
            )
            }
            
            sim[[i]]=simulateTarget(optimArgs=optimArgs,         #sim[[i]]$P, $Temp $attSim $targetSim
                                    simVar=simVar,
                                    modelTag=modelTag,
                                    modelInfo=modelInfo,
                                    attSel=attSel,
                                    attPrim=attApp,              #controlled via switch
                                    attInfo=attInfo,
                                    attInd=attInd,
                                    datInd=datInd,
                                    initCalibPars=NULL,
                                    targetLoc=targetMat[i,],     #is  a vector  (just 1 target here)
                                    attObs=attObs,
                                    parLoc=parLoc,
                                    parSim=NULL,
                                    setSeed=seedCatalogue[i],                   #seed based on loop counter
                                    file=file)




            
            if(IOmode == "dev"){
              #**DEV MODE TOWN**
              #MATRICES BECOME VECTORS (1 TARGET AT A TIME)
              simTarget=sim[[i]]$targetSim
              parSim=sim[[i]]$parS
              attValue=sim[[i]]$attSim  #NEW
              req=targetMat[i,]
              
              simTarget[length(attSel)+1]=parSim[nParTot+1]=attValue[length(attSel)+1]=req[length(attSel)+1]=arrayID
              simTarget[length(attSel)+2]=parSim[nParTot+2]=attValue[length(attSel)+2]=req[length(attSel)+2]=seedCatalogue[i]
              
              targ=t(matrix(simTarget))
              value=t(matrix(attValue))
              param=t(matrix(parSim))
              request=t(matrix(req))
              
              #WRITE SIMS TO CSV FILE
              write.table(targ,paste0("target",i,".csv"),append=FALSE,col.names=FALSE,row.names=FALSE,sep=",")
              write.table(value,paste0("value",i,".csv"),append=FALSE,col.names=FALSE,row.names=FALSE,sep=",")
              write.table(param,paste0("pars",i,".csv"),append=FALSE,col.names=FALSE,row.names=FALSE,sep=",")
              write.table(request,paste0("request",i,".csv"),append=FALSE,col.names=FALSE,row.names=FALSE,sep=",")
              
              simDat=makeOutputDataframe(data=sim[[i]],dates=dateExtnd,simVar=simVar,modelTag=modelTag[1])
              write.table(simDat,file=paste0("scenario",i,".csv"),row.names=FALSE,quote = FALSE,sep=",")
        
              devPlotSummary(obs=obs,
                             sim=sim[[i]],
                             simVar=simVar,
                             datInd=datInd,
                             attSel=attSel,
                             attPrim=attPrim,
                             simTarget=simTarget[c(1:length(attSel))],  #will be different for ... non arrayID
                             target=targetMat[i,],  
                             targetType=attInfo$targetType,
                             modelTag=modelTag,
                             optimArgs=optimArgs,
                             id=i,
                             nTarget=nTarget,
                             IOmode=IOmode)
            }else if (IOmode=="verbose"){
              
              if(!isTRUE(dir.exists(pathScenario))){
                dir.create(pathScenario)
              }
              if(!isTRUE(dir.exists(pathCSV))){
                dir.create(pathCSV)
              }
              if(!isTRUE(dir.exists(pathRData))){
                dir.create(pathRData)
              }
              #SAVE EVERYTHING
              simTarget[i,]=sim[[i]]$targetSim
              parSim[i,]=sim[[i]]$parS
              attValue[i,]=sim[[i]]$attSim  #NEW
              
              #STORE IN RDATA &  WRITE DATA FRAME TO CSV
              progress(p("Writing scenario ",i," to .RData and .csv"),file)
              fnam[i]=saveTarget(data=sim[[i]],
                                 dates=dateExtnd,
                                 modelTag=modelTag,
                                 modelInfo=modelInfo,
                                 simVar=simVar,
                                 target=targetMat[i,],
                                 attSel=attSel,
                                 attPrim=attPrim,
                                 paths=paths)
              
              if(!isTRUE(dir.exists(pathDiagnostics))){
                dir.create(pathDiagnostics)
              }#OUTPUT PLOT SUMMARY   #need alt filename here
              
              devPlotSummary(obs=obs,
                             sim=sim[[i]],
                             simVar=simVar,
                             datInd=datInd,
                             attSel=attSel,
                             attPrim=attPrim,
                             simTarget=simTarget[i,c(1:length(attSel))],
                             target=targetMat[i,],  
                             targetType=attInfo$targetType,
                             modelTag=modelTag,
                             optimArgs=optimArgs,
                             id=i,
                             nTarget=nTarget,
                             IOmode=IOmode,
                             paths=paths)
              
            } else {
              progress("No output generated in suppress mode",file)
            }

            b<-Sys.time()
            runt <- b-a
            progress(p("Time taken for scenario ",i,": ",signif(runt,digits=3)),file)
            logfile(signif(runt,digits=3),file)
          }
          progress("Stochastic model parameters and time series obtained at each target location",file)
     } #END STOCHASTIC SEGMENT
    #---------------------------------------------------------
    #SIMULATE AUXILLIARY TS
    #PET and any other auxilliary vars
    banner("SUMMARISING SIMULATION DIAGNOSTICS",file)
    #--------------------------------------------------------
    #SIMULATION INFORMATION SAVING
     if(IOmode == "verbose"){  #IF RUN IN VERBOSE MODE
       if(!isTRUE(dir.exists(pathMetadata))){
          dir.create(pathMetadata)
       }
       progress("Saving simulation information to file - requested targets, simulated targets, optimised parameters",file)
       if(modelTag[1] != "Simple-ann"){
         write.table(simTarget,file=paste0(paths$Metadata,"/simulatedTargets.csv"),row.names=FALSE,quote = FALSE,sep=",",col.names=attSel)     #WRITE SIMULATED TARGETS TO CSV
         parNames=NULL; for(i in 1:length(simVar)){parNames=c(parNames,modelInfo[[i]]$parNam)}
         write.table(parSim,file=paste0(paths$Metadata,"/simulatedTargetParameters.csv"),row.names=FALSE,quote = FALSE,sep=",",col.names=parNames)                #WRITE OPTIMISED PARS TO CSV
       }
       write.table(targetMat,file=paste0(paths$Metadata,"/requestedTargets.csv"),row.names=FALSE,quote = FALSE,sep=",")                      #WRITE REQUESTED TARGETS TO CSV
       write.table(fnam,file = paste0(paths$Scenario,"/filenames.txt"),row.names=FALSE,quote = FALSE,col.names=c("Filename"))                #OUTPUT FILENAME LIST
     }
    #----------------------------------------------------------
    
    #PLOT OF TARGETS TO SUMMARISE
    if(IOmode == "verbose"){  #If verbose mode
      #INSERT GENERAL FULL EXPSOURE SPACE PDF
      # if(!isTRUE(dir.exists(pathPlots))){
      #   dir.create(pathPlots)
      # }
      # if(modelTag[1] != "Simple-ann"){
      #   exposureSummary(targetMat=simTarget,attSel=attSel,paths=paths)
      # }else{
      #   exposureSummary(targetMat=targetMat,attSel=attSel,paths=paths)
      # }
    }
    
    #DIAGNOSTICS GENERATION
    if(modelTag[1]=="Simple-ann"){ progress("No diagnostics for simple scaling yet",file)}
    
    #MAKE OUTPUT DATAFRAME IF NOT DEV MODE
    if(IOmode !="dev"){
      simDat=list()
      for(i in 1:nTarget){ # loop over targets
        simDat[[i]]=makeOutputDataframe(data=sim[[i]],dates=dateExtnd,simVar=simVar,modelTag=modelTag[1])
      }
    }

    progress("Creating exposure space diagnostics",file)
    progress(paste0("Simulations completed. Please see outputs located in ",getwd()),file)
    

     #OUTPUT SIMULATION & ASSOCIATED INFORMATION
    if(IOmode != "dev"){
        if(modelTag[[1]] == "Simple-ann"){
          out=list(sim=sim,
                   modelTag=modelTag,
                   target=targetMat,
                   attPerturb=attPerturb,
                   attHold=attHold,
                   attSel=attSel,
                   attPrim=attPrim,
                   data=simDat,
                   nTarget=nTarget,
                   exSpArgs=exSpArgs)
        } else {
          out=list(sim=sim,
                   modelTag=modelTag,
                   target=targetMat,
                   attPerturb=attPerturb,
                   attHold=attHold,
                   attSel=attSel,
                   attPrim=attPrim,
                   attObs=attObs,
                   data=simDat,
                   nTarget=nTarget,
                   exSpArgs=exSpArgs)
        }
      
    }else{
      out=NULL  #no output in dev mode
    }

     return(out)
}

#----------------------------------------------------------------------------------------------------------------------------------------
performanceSpaces<-function(data=NULL,
                            plotTag=NULL,                   # can be OAT, Binary, Contours, Heat, more to come
                            plotArgs=NULL,                  # some plots will need levels specified, also titles, axis labels, etc.
                            systemModel=NULL,
                            systemArgs=NULL,
                            simDirectory="Simulation1",
                            performance=NULL,
                            IOmode="verbose"
                            ){
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
  
  #Copying for in function - this is a cludge I know
  simDat=data$data
  nTarget=data$nTarget
  attPerturb=data$attPerturb
  attSel=data$attSel
  targetMat=data$target
  exSpArgs=data$exSpArgs
  
  plotArgsDefault<-list()
   plotArgsDefault$title="Scenario Neutral Space"
   plotArgsDefault$xlim=c(-2,2)
   plotArgsDefault$ylim=c(0.7,1.3)
   plotArgsDefault$xtitle=NULL
   plotArgsDefault$ytitle=NULL
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
   
  if(is.null(performance)){
    #Needs data set format
    performance=rep(NA,nTarget)
    for(i in 1:nTarget){
      performance[i]=systemModel(data=simDat[[i]],systemArgs=systemArgs,seed=i)
    }
  }

  ###Create baseline plot

  if(plotTag=="Heat"){
      p1<-heatPlot(plotArgs=plotArgs,targetMat=targetMat, attSel=attSel,attPerturb=attPerturb,performance=performance)
      
      if(IOmode=="verbose"){
        cowplot::save_plot(paste0(path,"/HeatmapSpace.pdf"),p1$plot,base_aspect_ratio = 1,base_height = 7)
      }
  } else if (plotTag=="OAT") {
      # colnames(simTarget)<-attSel
      # p1<-oatplots(targetMat=targetMat,performance=performance,samp=data$exSpArgs$samp)
      p1<-oatplots(targetMat=targetMat,performance=performance,attPerturb=attPerturb,exSpArgs=exSpArgs)
      if(IOmode=="verbose"){
        cowplot::save_plot(paste0(path,"/OATPlots.pdf"),p1,base_width = 14,base_height = 7)
      }
  } else if (plotTag=="Binary"){
    #Not currently available
  } else if (plotTag=="Contours"){
    p1<-contourPlots(plotArgs=plotArgs,targetMat=targetMat,attPerturb=attPerturb,performance=performance)
    if(IOmode=="verbose"){
      cowplot::save_plot(paste0(path,"/ContourSpace.pdf"),p1$plot,base_aspect_ratio = 1,base_height = 7)
    }
  }

  #p1 is a list of an editable plot "plotEdit" and a final plot_grid cowlplot "plot"
  return(p1)

}

#----------------------------------------------------------------------------------------------------------------------------
plotLayers<-function(plot=NULL,
                     plotArgs=NULL,
                     climdata=NULL,
                     climArgs=list(),
                     simDirectory="Simulation1",
                     IOmode="verbose"){
  
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
    
    

    p1<-p1+geom_point(data=climdata,aes(x = climdata[,x], y =climdata[,y]),colour=climArgs$colour,size=2.5,shape=16)
  
    
    if(!is.null(climArgs$fill)){
      if(climArgs$fill=="performance"){
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


#---------------------------------------------------------------------------------------------------------------------------
#quickSpace(obs=obs,attPerturb = c("P_ann_tot_m","Temp_ann_avg_m"),exSpArgs=exSpArgs,systemModel = tankWrapper,systemArgs = systemArgs,climdata = climdata)

quickSpace<-function(obs=NULL,                           # data frame of observed data with column names compulsary [$year, $month, $day, $P,] additional [$Temp, $RH, $PET, $uz, $Rs] (or a subset of these)
                       modelTag="Simple-ann",                    # scalar or vector of models to use
                       attPerturb=NULL,                  # vector of perturbed attributes
                       attHold=NULL,                     # vector of held attributes
                       attPenalty=NULL,                  # vector of primary attributes (a penalty function is used to prioritise this subset of selected attributes)
                       optimArgs=list(pcrossover= 0.8,   # list of a parameters used by the ga optimiser (if used)
                                      pmutation=0.1,
                                      maxiter=10,
                                      maxFitness=-0.001,
                                      popSize = 100,
                                      run=20,
                                      seed = NULL,
                                      parallel = FALSE,
                                      keepBest=TRUE,
                                      lambda.mult=0.0
                       ),
                       exSpArgs=list(),
                       simLengthNyrs=NULL,
                       systemModel=NULL,
                       systemArgs=NULL,
                       plotTag="Heat",
                       plotArgs=NULL,
                       IOmode="suppress",    # "dev","suppress","verbose"
                       arrayID=NULL,        # only used in "dev" mode
                       nSeed=NULL,
                       climdata=NULL,
                       climArgs=list(),
                       simDirectory="Simulation1"){
  
  #Stage one - make the space
  
  out<-scenarioGenerator(obs=obs,attHold=attHold,attPerturb = attPerturb,modelTag=modelTag,exSpArgs=exSpArgs,IOmode=IOmode,simDirectory = simDirectory)
  
  #Stage two - simulate performance
  
  plot<-performanceSpaces(data=out,plotTag=plotTag,plotArgs=plotArgs,systemModel=systemModel,systemArgs=systemArgs,simDirectory = simDirectory,IOmode=IOmode)
  p1<-list()
  p1<-plot$plotEdit
  #Stage three - add layers of information
  
  plotclim<-plotLayers(plot=p1,plotArgs=plotArgs,climdata = climdata,climArgs=climArgs,simDirectory = simDirectory,IOmode=IOmode)
  
  cowplot::save_plot("quickSpace.pdf",plotclim,base_aspect_ratio = 1,base_height = 7)
  
  return(plotclim)
}

#-----------------------------------------------------------------------------------------------------------------------------
exSpArgsVisual<-function(exSpArgs=NULL){
  
  modelTag="Simple-ann"
  attSel=names(exSpArgs$bounds)
  
  modelInfo=list()
  modelInfo[[modelTag[1]]]=get.model.info(modelTag[1])
  attInfo=attribute.info.check(attSel=attSel)                                 # vector of selected attributes (strings)
  if(modelTag[1] == "Simple-ann"){simVar=attInfo$varType}
  attInd=get.att.ind(attInfo=attInfo,simVar=simVar)
  attInfo=update.att.Info(attInfo=attInfo,attInd=attInd,modelTag=modelTag,simVar=simVar)
  
  targetMat=expSpaceSampler(type=exSpArgs$type,
                            samp=exSpArgs$samp,
                            bounds=exSpArgs$bounds,
                            varType=attInfo$varType,
                            targetType=attInfo$targetType,
                            attSel=attSel,
                            file)
  
  out<-expSpace2dViz(targetMat[,2],targetMat[,1],x.lab=names(exSpArgs$bounds)[2],y.lab=names(exSpArgs$bounds)[1])
  
}

#-----------------------------------------------------------------------------------------------------------------------------
attributeCalculator<-function(obs=NULL,                    # scalar or vector of models to use
                            attSel=NULL,                   # vector of selected attributes
                            slice=NULL,                    #
                            window=10
                                             
){
  IOmode="verbose"
  arrayID=NULL
  simLengthNyrs=NULL
    file<-filename(IOmode=IOmode,arrayID=arrayID)

  input<-input_check(obs,file,simLengthNyrs) #Checks for missing values/full years of data
  obs<-input$data
  
  if(!is.null(slice)){                       #For non historical records
    start=slice-window
    text<-paste("Note: Window is set ",window," years before slice",sep="")
#    progress(text,file)
    obs<-obs[which(obs$year>=start&obs$year<=slice),]
  }
  
  #Get necessary variables for historical atts
  attInfo=attribute.info.check(attSel=attSel)              # vector of selected attributes (strings)
  simVar<-attInfo$varType
  simVar<-unique(simVar)
  nvar<-length(simVar)
  
#  banner("INDEXING DATES",file)
#  progress("Indexing dates...",file)                       # USE NEW APPENDED/CHECKED DATA
  yy=obs$year;mm=obs$month;dd=obs$day  
  # STORE DATE VECTORS
  
  datInd=list()
  datInd[["obs"]]=get.date.ind(dd=dd,mm=mm,yy=yy,nperiod=12,southHemi=TRUE)
  attInd=get.att.ind(attInfo=attInfo,simVar=simVar)
  
#  progress("Dates indexed OK",file)
  
#  banner("OBSERVED BASELINE ATTRIBUTE CALCULATION",file)
#  progress("Calculating attributes...",file)
  
  attObs=list()                            #make this into its own function (also inserted into model sequencer)
  for(i in 1:nvar){
    attObs[[i]]=attribute.calculator(attSel=attSel[attInd[[i]]],
                                     data=obs[[simVar[i]]],
                                     datInd=datInd[["obs"]],
                                     attribute.funcs=attribute.funcs) 
  }
  
#  progress("Attributes calculated OK",file)   #NEED SOME ACTUAL CHECKING HERE BEFORE PRONOUNCING OK
  attObs=unlist(attObs)
  names(attObs)=attSel
#  progress(attObs)
  
  return(attObs)
}

#---------------------------------------------------------------------------------------------------------------------------
# devGenerator<-function(obs=NULL,                           # data frame of observed data with column names compulsary [$year, $month, $day, $P,] additional [$Temp, $RH, $PET, $uz, $Rs] (or a subset of these)
#                             modelTag=NULL,                    # scalar or vector of models to use
#                             attSel=NULL,                      # vector of selected attributes
#                             attPrim=NULL,                     # vector of primary attributes (a penalty function is used to prioritise this subset of selected attributes)
#                             optimArgs=list(pcrossover= 0.8,   # list of a parameters used by the ga optimiser (if used)
#                                            pmutation=0.1,
#                                            maxiter=50,
#                                            maxFitness=-0.001,
#                                            popSize = 500,
#                                            run=20,
#                                            seed = NULL,
#                                            parallel = FALSE,
#                                            keepBest=TRUE,
#                                            lambda.mult=0.0
#                             ),
#                             exSpArgs=NULL,
#                             simLengthNyrs=NULL,  # length of simulation in year (if longer simulation selected)
#                             arrayID=NULL
# ){
#   
#   file<-paste0("logfile",arrayID,".txt")
#     
#   #CHECK FOR ARGUMENTS
#   banner("CHECK FOR ARGUMENT INPUTS",file)
#   progress("Checking argument inputs...",file)
#   
#   arg_check<-argument_check(names=names(obs),
#                             obs=obs,
#                             attSel=attSel,        
#                             attPrim=attPrim,
#                             modelTag=modelTag,
#                             optimArgs=optimArgs,
#                             exSpArgs=exSpArgs,
#                             file=file)
#   
#   obs=arg_check$obs
#   optimArgs=arg_check$optimArgs
#   exSpArgs=arg_check$exSpArgs
#   progress("Argument input format OK",file)
#   
#   #CHECK FOR LOGIC IN ATTRIBUTE MODEL COMBOS
#   banner("CHECK FOR MODEL AND ATTRIBUTE COMBINATIONS",file)
#   progress("Checking model and attribute combinations...",file)
#   
#   argument_logic_check(names=names(obs),
#                        attSel=attSel,
#                        attPrim=attPrim,
#                        modelTag=modelTag,
#                        file=file)
#   
#   progress("Model and attribute combinations OK",file)
#   
#   #CHECK FOR INPUTS
#   banner("CHECK FOR DATAFRAME INPUT",file)
#   progress("Checking dataframe input...",file)
#   inputcheck<-input_check(obs,file,simLengthNyrs)
#   progress("Dataframe input OK",file)
#   
#   #GET ADDITIONAL MODEL INFO, ATT INFO & SORT
#   nMod=length(modelTag)   
#   if(nMod==1){
#     modelInfo=list()
#     modelInfo[[modelTag[1]]]=get.model.info(modelTag[1])                     #even if 1 model still store in list format
#   }else{
#     modelInfo=sapply(X = modelTag,FUN=get.model.info,USE.NAMES=TRUE)
#   }
#   
#   simPriority=sort(sapply(X=modelInfo,FUN=return.simPriority,USE.NAMES=TRUE)) #get simulation priority of each model
#   modelTag=names(simPriority)                                                 # Force simVar="P" to come first via sorting by $simPrority
#   simVar=sapply(X=modelInfo[modelTag],FUN=return.simVar,USE.NAMES=TRUE)       #???CREATE MODEL MASTER INFO - HIGHER LEVEL?
#   attInfo=attribute.info.check(attSel=attSel)                                 # vector of selected attributes (strings)
#   if(modelTag[1] == "Simple-ann"){simVar=attInfo$varType}
#   attInd=get.att.ind(attInfo=attInfo,simVar=simVar)
#   attInfo=update.att.Info(attInfo=attInfo,attInd=attInd,modelTag=modelTag,simVar=simVar) #add extra level for easier model mangmt
#   if(modelTag[1] != "Simple-ann"){nParTot=0;for(i in 1:nMod){nParTot=nParTot+modelInfo[[i]]$npars}}                      #total number of pars
#   
#   #GET DATES DATA (and indexes for harmonic periods)
#   banner("INDEXING DATES",file)
#   progress("Indexing dates...",file)
#   obs=inputcheck$data                                      # USE NEW APPENDED/CHECKED DATA
#   yy=obs$year;mm=obs$month;dd=obs$day                      # STORE DATE VECTORS
#   
#   datInd=list()
#   datInd[["obs"]]=get.date.ind(dd=dd,mm=mm,yy=yy,nperiod=12,southHemi=TRUE)              #make obs based datInd
#   
#   #EXTEND DATES IF NEEDED
#   if(!is.null(simLengthNyrs)){
#     if(modelTag[[1]] != "Simple-ann"){
#       dateExtnd=extendDates(simLengthNyrs=simLengthNyrs,dd=dd,mm=mm,yy=yy)
#     }else{
#       dateExtnd=obs[,c("year","month","day")]                                              # make the same as observed
#       progress("Length of time series cannot be increased using simple scaling",file)
#     }
#   }else{
#     dateExtnd=obs[,c("year","month","day")]                                              # make the same as observed
#   }
#   
#   for(i in 1:nMod){
#     datInd[[modelTag[i]]]=get.date.ind(dd=dateExtnd$day,mm=dateExtnd$month,yy=dateExtnd$year,nperiod=modelInfo[[modelTag[i]]]$nperiod,southHemi=TRUE)          # FROM dateManager.R
#   }
#   progress("Dates indexed OK",file)
#   
#   #EXPOSURE SPACE GENERATION 
#   banner("SAMPLING EXPOSURE SPACE",file)
#   progress("Sampling exposure space targets...",file)
#   
#   if(is.character(exSpArgs)==TRUE) {
#     targetMat=read.table(file=exSpArgs,sep=",",header=TRUE)
#     attRot=NULL
#   } else {
#     spaceInfo=expSpaceSampManager(exSpArgs=exSpArgs,attInfo=attInfo,attSel=attSel,file=file)
#     targetMat=spaceInfo$targetMat
#     attRot=spaceInfo$attRot      #attRot - used if needed in "OAT" operations
#   }
#   
#   nTarget=dim(targetMat)[1]
#   progress("Exposure space sampled OK",file)
#   progress(p(nTarget," targets sampled across ", length(attInfo$varType)," attributes"),file) 
#   
#   
#   # windows()
#   # expSpace2dViz(x=targetMat[,6],    #vector of one attribute
#   #               y=targetMat[,1],    #vector of one attribute
#   #               x.lab=colnames(targetMat)[6],
#   #               y.lab=colnames(targetMat)[1]
#   #               
#   # )
#   
#   #SIMPLE SCALING
#   if(modelTag[[1]] == "Simple-ann"){
#     banner("SIMPLE SCALE FOR OBSERVED DATA",file)  
#     progress("Scaling data...",file)  
#     sim=list()
#     fnam=vector(length=nTarget)
#     for (i in 1:nTarget) {
#       sim[[i]]=simple.scaling(target=unlist(targetMat[i,]),
#                               targetType=attInfo$targetType,
#                               data=obs[simVar],
#                               varType=attInfo$varType,
#                               period=modelInfo[[modelTag[1]]]$nperiod,
#                               i.pp=datInd[[modelTag[1]]]$i.pp) 
#       
#       #WRITE SIMPLE SCALING OUTPUT TO CSV & RDATA FILE
#       fnam[i]<-simpleSaveTarget(data=sim[[i]],  
#                                 dates=obs[,c("year","month","day")],
#                                 simVar=simVar,   
#                                 attSel=attSel,
#                                 target=targetMat[i,],
#                                 modelTag=modelTag[1],
#                                 modelInfo=modelInfo[1])
#     }  
#     progress("Data scaled OK",file)
#     
#   }else{
#     
#     
#     #STOCHASTIC SIMULATION
#     
#     #GET ATTRIBUTES OF OBSERVED DATA (testing attribute calc function)
#     banner("OBSERVED BASELINE ATTRIBUTE CALCULATION",file)
#     progress("Calculating attributes...",file)
#     
#     attObs=list()                            #make this into its own function (also inserted into model sequencer)
#     for(i in 1:nMod){
#       attObs[[i]]=attribute.calculator(attSel=attSel[attInd[[i]]],
#                                        data=obs[[simVar[i]]],
#                                        datInd=datInd[["obs"]],
#                                        attribute.funcs=attribute.funcs) 
#     }
#     attObs=unlist(attObs)
#     
#     progress(paste("Attributes of observed series - ",paste(attSel,": ",signif(attObs,digits=4),collapse = ", ",sep=""),sep=""),file)
#     progress("Attributes calculated OK",file)   #NEED SOME ACTUAL CHECKING HERE BEFORE PRONOUNCING OK
#     
#     #PART 1: INITIAL CALIBRATION OF EACH MODEL
#     #-INITIAL CALIBRATION OF SELECTED MODEL(S) - wrapper for alternate function
#     # initCalibPars=init.calib(data=obs[[simVar[i]]],modelTag=modelTag[i],modelInfo=modelInfo[[modelTag[i]]],datInd=datInd[[modelTag[i]]]) #for rain model only
#     
#     #PART 2: OPTIMISING TO DETERMINE PARS
#     #LOOP OVER EXPOSURE SPACE POINTS TO DETERMINE PARS
#     banner("DETERMINING STOCHASTIC MODEL PARAMETERS FOR TARGETS",file)
#     progress("Determining stochastic model parameters at each target location...",file)  
#     progress("Simulating stochastic time series...",file)  
#     progress("Starting cluster...",file)  
#     
#     #DETERMINE WHICH PARS ATTACH TO WHICH MODEL (make this a function in stochParManager.R)
#     parLoc=whichPars(simVar=simVar,modelInfo=modelInfo)
#     
#     #SCREEN INAPPROPRIATE SUGGESTIONS IF ANY
#     optimArgs$suggestions=screenSuggest(suggest=optimArgs$suggestions,modelInfo=modelInfo,modelTag=modelTag,parLoc=parLoc)
#    
#     fnam=rep(NA,nTarget)
#     sim=list()                                   #STORE TIMESERIES, AND SIMULATED ATTRIBUTES
#     for(i in arrayID:arrayID){     
#       a<-Sys.time()
#       progress(p("Working on scenario ",i," of ",nTarget),file)
#       
#       #IF "OAT" ROTATE attPrim
#       switch(exSpArgs$type,                  
#              "OAT" = {attApp=attRot[i]},  # rotate through attPrim
#              {attApp=attPrim}             # maintain attPrim
#       )
#       
#       sim[[i]]=simulateTarget(optimArgs=optimArgs,         #sim[[i]]$P, $Temp $attSim $targetSim
#                               simVar=simVar,
#                               modelTag=modelTag,      
#                               modelInfo=modelInfo,
#                               attSel=attSel,
#                               attPrim=attApp,              #controlled via switch above
#                               attInfo=attInfo,
#                               attInd=attInd,
#                               datInd=datInd,
#                               initCalibPars=NULL,
#                               targetLoc=targetMat[i,],     #is  a vector  (just 1 target here)    
#                               attObs=attObs,   
#                               parLoc=parLoc,
#                               parSim=parSim,
#                               setSeed=arrayID,
#                               file=file)
#       
#       
#       
#       simTarget=sim[[i]]$targetSim
#       parSim=sim[[i]]$parS
#       attValue=sim[[i]]$attSim
#       
#       simTarget[length(attSel)+1]=parSim[nParTot+1]=attValue[length(attSel)+1]=arrayID
#       
#       targ<-t(matrix(simTarget));value<-t(matrix(attValue));param<-t(matrix(parSim))
#       
#       
#       
#       b<-Sys.time()
#       runt <- b-a
#       progress(p("Time taken for scenario ",i,": ",signif(runt,digits=3)),file)
#       logfile(signif(runt,digits=3),file)
#       
#       #STORE IN RDATA &  WRITE DATA FRAME TO CSV
#       progress(p("Writing scenario ",i," to .RData and .csv"),file)
#       # fnam[i]=saveTarget(data=sim[[i]],       
#       #                    dates=dateExtnd,      
#       #                    modelTag=modelTag,  
#       #                    modelInfo=modelInfo,  
#       #                    simVar=simVar,    
#       #                    target=targetMat[i,],     
#       #                    attSel=attSel,     
#       #                    attPrim=attPrim)
#       
#       write.table(targ,paste0("target",i,".csv"),append=FALSE,col.names=FALSE,row.names=FALSE,sep=",")
#       write.table(value,paste0("value",i,".csv"),append=FALSE,col.names=FALSE,row.names=FALSE,sep=",")
#       write.table(param,paste0("pars",i,".csv"),append=FALSE,col.names=FALSE,row.names=FALSE,sep=",")
#       
#       simDat=makeOutputDataframe(data=sim[[i]],dates=dateExtnd,simVar=simVar,modelTag=modelTag[1])
#       write.table(simDat,file=paste0("scenario",i,".csv"),row.names=FALSE,quote = FALSE,sep=",")
#       
#       devPlotSummary(obs=obs,
#                     sim=sim[[i]],
#                     simVar=simVar,
#                     datInd=datInd,
#                     attSel=attSel,
#                     attPrim=attPrim,
#                     simTarget=simTarget[c(1:length(attSel))],
#                     target=targetMat[i,],
#                     targetType=attInfo$targetType,
#                     modelTag=modelTag,
#                     optimArgs=optimArgs,
#                     id=i)
#       
#     }  
#     progress("Stochastic model parameters and time series obtained at each target location",file)  
#   }
#  
#   
#   #OUTPUT SIMULATION & ASSOCIATED INFORMATION
#   if(modelTag[[1]] == "Simple-ann"){
#     out=list(sim=sim,
#              modelTag=modelTag,
#              target=targetMat, 
#              attSel=attSel,
#              attPrim=attPrim)
#   } else {
#     out=list(sim=sim,
#              modelTag=modelTag,
#              target=targetMat, 
#              attSel=attSel,
#              attPrim=attPrim,
#              attObs=attObs)  
#   }
#   return(out)
# }
# 
