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
  
  if(IOmode=="verbose"){
    cowplot::save_plot("quickSpace.pdf",plotclim,base_aspect_ratio = 1,base_height = 7)
  }
  
  return(plotclim)
}

#-----------------------------------------------------------------------------------------------------------------------------