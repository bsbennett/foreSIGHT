#---------------------------------------------------------------------------------------------------------------------------------------
scenarioGenerator<-function(obs=NULL,                           # data frame of observed data with column names compulsary [$year, $month, $day, $P,] additional [$Temp, $RH, $PET, $uz, $Rs] (or a subset of these)
                              modelTag=NULL,                    # scalar or vector of models to use
                              modelInfoMod=list(),              # list of options for the stochastic models
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
                              IOmode="suppress",    # "dev","suppress","verbose"
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
      
      if(!isTRUE(dir.exists(paths$Logfile))){ #paths$Logfile
        dir.create(pathLogfile)
      }
    
    }
  
    #SET LOGFILE PATH AND NAME
    if(IOmode=="verbose"){                          #send logfile to wd subfolder if IOmode="dev"
      file<-filename(IOmode=IOmode,arrayID=arrayID)
      file<-paste0(paths$Logfile,"/",file)
    }else if (IOmode=="suppress"){
      file<-filename(IOmode=IOmode,arrayID=arrayID) #send logfile to tempdir if IOmode="suppress"
      file<-paste0(tempdir(),"/",file)
    }else{  
      file<-filename(IOmode=IOmode,arrayID=arrayID) #send logfile to wd if IOmode="dev"
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
    obs=inputcheck$data                                      # USE NEW APPENDED/CHECKED DATA
    progress("Dataframe input OK",file)

    #GET ADDITIONAL MODEL INFO, ATT INFO & SORT (make into separate script/functions)
    nMod=length(modelTag)   
    modelInfo=get.multi.model.info(modelTag=modelTag)

    #UPDATE MODELINFO IF NEEDED
    for(mod in 1:nMod){
      if(!is.null(modelInfoMod[[modelTag[mod]]])){
        #modifyList
        if(mod==1) progress("Updating model info...",file)
        defaultMods=list(minBound=NULL,maxBound=NULL,fixedPars=NULL)
        modPars=modifyList(defaultMods,modelInfoMod[[modelTag[mod]]])
        modelInfo[[modelTag[mod]]]=update.model.info(modelTag=modelTag[mod],
                                                    modelInfo=modelInfo[[modelTag[mod]]],
                                                    fixedPars=modPars$fixedPars,
                                                    minUserBound=modPars$minBound,
                                                    maxUserBound=modPars$maxBound,
                                                    file=file)  #need to build in checks for this
        # if(!is.na(modelInfo[[modelTag[mod]]]$fixedPars)
      }
    }

    modelTag=update.simPriority(modelInfo=modelInfo)
    simVar=sapply(X=modelInfo[modelTag],FUN=return.simVar,USE.NAMES=TRUE)       #?CREATE MODEL MASTER INFO - HIGHER LEVEL?
    attInfo=attribute.info.check(attSel=attSel,attPrim=attPrim)                                 # vector of selected attributes (strings)
    if(modelTag[1] == "Simple-ann"){simVar=attInfo$varType}
    attInd=get.att.ind(attInfo=attInfo,simVar=simVar)
    attInfo=update.att.Info(attInfo=attInfo,attInd=attInd,modelTag=modelTag,simVar=simVar) #add extra level for easier model mangmt
    if(modelTag[1] != "Simple-ann"){nParTot=0;for(i in 1:nMod){nParTot=nParTot+modelInfo[[i]]$npars}}                      #total number of pars

    
    #GET DATES DATA (and indexes for harmonic periods)
    banner("INDEXING DATES",file)
    progress("Indexing dates...",file)
    dateExtnd=dateExtender(obs=obs,simLengthNyrs=simLengthNyrs,file=file,modelTag=modelTag)  #Extend dates if needed
    datInd=mod.get.date.ind.extnd(obs=obs,dateExtnd=dateExtnd,modelTag=modelTag,modelInfo=modelInfo,simLengthNyrs=simLengthNyrs,file=file,southHemi=TRUE)
    
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
          attObs=unlist(attObs); attObs=attObs[attSel]   #unlist attObs and make sure order is correct
        
          progress(paste("Attributes of observed series - ",paste(attSel,": ",signif(attObs,digits=5),collapse = ", ",sep=""),sep=""),file)
          progress("Attributes calculated OK",file)   #NEED SOME ACTUAL CHECKING HERE BEFORE PRONOUNCING OK

          #PART 1: INITIAL CALIBRATION OF EACH MODEL
          #-INITIAL CALIBRATION OF SELECTED MODEL(S) - wrapper for alternate function
          #initCalibPars=init.calib(data=obs[[simVar[i]]],modelTag=modelTag[i],modelInfo=modelInfo[[modelTag[i]]],datInd=datInd[[modelTag[i]]]) #for rain model only

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
              objScore=sim[[i]]$score
              
              simTarget[length(attSel)+1]=parSim[nParTot+1]=attValue[length(attSel)+1]=req[length(attSel)+1]=arrayID
              simTarget[length(attSel)+2]=parSim[nParTot+2]=attValue[length(attSel)+2]=req[length(attSel)+2]=seedCatalogue[i]
              
              targ=t(matrix(simTarget))
              value=t(matrix(attValue))
              param=t(matrix(parSim))
              request=t(matrix(req))
              fitnesses=t(matrix(objScore))
              
              #WRITE SIMS TO CSV FILE
              write.table(targ,paste0("target",i,".csv"),append=FALSE,col.names=FALSE,row.names=FALSE,sep=",")
              write.table(value,paste0("value",i,".csv"),append=FALSE,col.names=FALSE,row.names=FALSE,sep=",")
              write.table(param,paste0("pars",i,".csv"),append=FALSE,col.names=FALSE,row.names=FALSE,sep=",")
              write.table(request,paste0("request",i,".csv"),append=FALSE,col.names=FALSE,row.names=FALSE,sep=",")
              write.table(fitnesses,paste0("fitness",i,".csv"),append=FALSE,col.names=FALSE,row.names=FALSE,sep=",")
              
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
                            IOmode="suppress"
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
   plotArgsDefault$contourlevels=NULL

  if(!is.null(plotArgs)){
   plotArgs=modifyList(plotArgsDefault,plotArgs)
  } else {
    plotArgs=plotArgsDefault
  }
   
  #Running system model 
  if(is.null(performance)){
    #Needs data set format
    performance=rep(NA,nTarget) # need to update to have more than one performance metric
    for(i in 1:nTarget){
      performance[i]=systemModel(data=simDat[[i]],systemArgs=systemArgs,repID=i)
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
      p1<-oatplots(targetMat=targetMat,performance=performance,attPerturb=attPerturb,exSpArgs=exSpArgs,plotArgs=plotArgs)
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

  perfMap=cbind(targetMat,performance)
  p1$perfDat=perfMap
  
  #p1 is a list of an editable plot "plotEdit" and a final plot_grid cowlplot "plot"
  return(p1)

}









