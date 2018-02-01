###############################
##      MODEL SEQUENCER      ##
###############################

#CONTAINS
  # simulateTarget() - simulates all requested timseries for an individual target location (also outputs attributes and targetSim)

#------------------------------------------------
#FUNCTIONS
simulateTarget<-function(
                    optimArgs=NULL,
                    simVar=NULL,
                    modelTag=NULL,      
                    modelInfo=NULL,
                    attSel=NULL,
                    attPrim=NULL,
                    attInfo=NULL,
                    attInd=NULL,
                    datInd=NULL,
                    initCalibPars=NULL,
                    targetLoc=NULL,     #is  a vector  (just 1 target here)    
                    attObs=NULL,   
                    parLoc=NULL,        #which pars belong to which model parLoc[[mod]]=c(start,end)
                    parSim=NULL,        #pars used to simulate targets so far
                    setSeed=1234,
                    file=NULL
                   # resid_ts=NULL    - for other models not currently in play
                    ){
  
  #LOOP OVER EACH STOCHASTIC MODEL NOMINATED
  nMod=length(modelTag)
  out=list()
  
  parV=NULL
  
  #TRIM TO RETAIN NON-NAs
  #Which target inloop (done by eval 1st column)
#  if(is.na(parSim[1,1])){nLoop=1}else{nLoop=which(is.na(parSim[,1]))[1]} 
#  if(nLoop>1){parSim=parSim[1:(nLoop-1),]}else{parSim=NULL}
  
  #MERGE WITH ANY SUGGESTIONS SUPPLIED IN OPTIMARGS
  if(!is.null(optimArgs$suggestions)){
    parSugg=rbind(parSim,optimArgs$suggestions)
    if(dim(parSugg)[1] > optimArgs$popSize){
      parSugg=parSugg[(1:optimArgs$popSize),]  #make sure it doesn't exceed popSize
    }
  }else{ 
    parSugg=NULL
  }
  
  for(mod in 1:nMod){
    
     #IF CONDITIONED ON DRY-WET STATUS, populate wdStatus
      switch(simVar[mod], #
             "P" = {wdStatus=NULL},
             "Temp" = {if(modelInfo[[modelTag[mod]]]$WDcondition==TRUE){
                           wdStatus=out[["P"]]$sim
                         }else{
                           wdStatus=NULL   
                         }
                       },
                    {wdStatus=NULL}  #default
             )
      
      progress(p("    Working on variable ",simVar[mod]),file)
      progress(p("    Commencing optimisation..."),file)
      
      #GRAB PAR SUGGESTIONS RELATED TO modelTag
      if(!is.null(parSugg)){
        parSel=parSugg[,(parLoc[[mod]][1]:parLoc[[mod]][2])] #grab par suggestions related to modelTag running
      }else{
        parSel=NULL      #no suggestions to be had
      }
      
      optTest=gaWrapper(gaArgs=optimArgs,          
                        modelTag=modelTag[mod],      
                        modelInfo=modelInfo[[modelTag[mod]]],
                        attSel=attSel[attInd[[mod]]],
                        attPrim=attPrim,
                        attInfo=attInfo[[modelTag[mod]]],
                        datInd=datInd[[modelTag[mod]]],
                        initCalibPars=NULL,
                        parSuggest=parSel,
                        target=targetLoc[attInd[[mod]]],        
                        attObs=attObs[attInd[[mod]]],        
                        lambda.mult=optimArgs$lambda.mult,  
                        simSeed=setSeed,           
                        wdSeries=wdStatus,   #selecting rainfall  if needed 
                        resid_ts=NULL)
      
      progress(p("    Best fitness: ",signif(optTest$fitness,digits=5), ". Optimisation stopped at iter ",optTest$opt@iter),file)
      #progress(p("    Note:",signif(summary(optTest$opt)$fitness,digits=5)),file)
      #plot(optTest$opt)
      
      out[[simVar[mod]]]=switch_simulator(type=modelInfo[[modelTag[mod]]]$simVar,
                                          parS=optTest$par,
                                          modelTag=modelTag[mod],
                                          modelInfo=modelInfo[[modelTag[mod]]],
                                          datInd=datInd[[modelTag[mod]]],
                                          initCalibPars=NULL,
                                          wdSeries=wdStatus,
                                          resid_ts=NULL,
                                          seed=optTest$seed)
      
      
      
      #CALCULATE SELECTED ATTRIBUTE VALUES
      sim.att=attribute.calculator(attSel=attSel[attInd[[mod]]],data=out[[simVar[mod]]]$sim,datInd=datInd[[modelTag[mod]]],attribute.funcs=attribute.funcs)
      
      #RELATING TO BASELINE SERIES 
      simPt=unlist(Map(function(type, val,baseVal) simPt.converter.func(type,val,baseVal), attInfo$targetType[attInd[[mod]]], as.vector(sim.att),as.vector(attObs[attInd[[mod]]])),use.names = FALSE)   
      
      score=objFuncMC(attSel= attSel[attInd[[mod]]],     # vector of selected attributes 
                      attPrim=attPrim,      # any primary attributes
                      simPt=simPt,
                      target=targetLoc[attInd[[mod]]],
                      penalty.func=penaltyFunc_basic,   #make this changeable (auto calc lambda)
                      lambda=optimArgs$lambda.mult)
      
      #CONFIRMING SCORE FOR SIM SERIES
      progress(paste0("    Variable ",simVar[mod]," final sim series fitness: ",signif(score,4)),file)
      
      parV=c(parV,optTest$par)
      

  }  #end model loop
  
  #CALCULATE SIM ATTRIBUTES HERE-----
  attSim=list()
  for(mod in 1:nMod){
    attSim[[mod]]=attribute.calculator(attSel=attSel[attInd[[mod]]],data=out[[simVar[mod]]]$sim,datInd=datInd[[modelTag[mod]]],attribute.funcs=attribute.funcs)
  }
  out$attSim=unlist(attSim)  #store in out list
  progress(paste("    Attributes Simulated - ",paste(attSel,": ",signif(out$attSim,digits=4),collapse = ", ",sep=""),sep=''),file)
  
  targetSim=unlist(Map(function(type, val,baseVal) simPt.converter.func(type,val,baseVal), attInfo$targetType, as.vector(out$attSim),as.vector(attObs)),use.names = FALSE)
 
  names(targetSim)=attSel
  out$targetSim=targetSim
  progress(paste("    Target Simulated - ",paste(attSel,": ",signif(out$targetSim,digits=4),collapse = ", ",sep=''),sep=""),file)
  # progress(paste("    Final sim series fitness (no penalty):", signif(eucDist(simPt=out$targetSim,target=targetLoc),digits=4)),file)

  #penalty.func(target=target[primInd],simPt=simPt[primInd],lambda=lambda)
  
  # get.ind=function(x,y){which(x == y)}
  # 
  # # PENALTY FUNCTION
  # if(length(attPrim)>0){
  #   #IDENTIFY PRIMARY ATTRIBUTES 
  #   primInd=vapply(attPrim,FUN=get.ind,FUN.VALUE=numeric(1),x=attSel,USE.NAMES = FALSE)  #Indices of primary attributes
  
  out$parS=parV
  
  #WRITE SIMULATED TIMESERIES TO CSV FILE
  
  
  return(out)
}


#TESTER
# tmp=simulateTarget(optimArgs=optimArgs,
#                    simVar=simVar,
#                    modelTag=modelTag,      
#                    modelInfo=modelInfo,
#                    attSel=attSel,
#                    attPrim=attPrim,
#                    attInfo=attInfo,
#                    attInd=attInd,
#                    datInd=datInd,
#                    initCalibPars=NULL,
#                    targetLoc=targetMat[1,],     #is  a vector  (just 1 target here)    
#                    attObs=attObs,        
#                    lambda.mult=1.0,  
#                    setSeed=1234)
#
# tmp[[simVar[mod]]]
# tmp$attSim
