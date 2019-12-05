#Function to repackage scenarios created using dev mode to look more like "verbose" or "suppress"

devScenarioClean<-function(obs=NULL,
                           optimArgs=NULL,
                           attPerturb=NULL,
                           attHold=NULL,
                           attPenalty=NULL,
                           modelTag=NULL,
                           modelInfoMod=list(),              # list of options for the stochastic models
                           exSpArgs=NULL, 
                           simDirectory=NULL,
                           nSeed=NULL,
                           nRep=NULL,
                           repack=FALSE
                           ){
  
  path<-file.path(simDirectory)
  
  #GET ADDITIONAL MODEL INFO, ATT INFO & SORT (make into separate script/functions)
  nMod=length(modelTag)   
  modelInfo=get.multi.model.info(modelTag=modelTag)
  
  #UPDATE MODELINFO IF NEEDED
  for(mod in 1:nMod){
    if(!is.null(modelInfoMod[[modelTag[mod]]])){
      #modifyList
      # if(mod==1) progress("Updating model info...",file)
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
  
  #Run some checks
  #get Arguments
  arguments<-getArguments(attPerturb=attPerturb,attHold=attHold,optimArgs=optimArgs,exSpArgs=exSpArgs)
  attSel=arguments$attSel
  exSpArgs=arguments$exSpArgs
  optimArgs=arguments$optimArgs
  attPrim=attPenalty
  
  #1. Read fitness files
  fitness=NULL
  for(i in 1:nRep){
    dat=matrix(as.numeric(utils::read.csv(file=paste0(path,"/fitness",i,".csv"),header=F)),nrow=1)
    fitness=rbind(fitness,dat)
  }
  dummy=paste0(simVar,"-fit")
  colnames(fitness)=dummy
  
  #2. Read target files
  target=NULL
  for(i in 1:nRep){
    dat=matrix(as.numeric(utils::read.csv(file=paste0(path,"/target",i,".csv"),header=F)),nrow=1)
    target=rbind(target,dat)
  }
  colnames(target)=matrix(c(attPerturb,attHold,"rep","seed"),nrow=1)
  
  #3. Read value files
  value=NULL
  for(i in 1:nRep){
    dat=matrix(as.numeric(utils::read.csv(file=paste0(path,"/value",i,".csv"),header=F)),nrow=1)
    value=rbind(value,dat)
  }
  colnames(value)=matrix(c(attPerturb,attHold,"rep","seed"),nrow=1)
  
  #4. Read request files
  request=NULL
  for(i in 1:nRep){
    dat=matrix(as.numeric(utils::read.csv(file=paste0(path,"/request",i,".csv"),header=F)),nrow=1)
    request=rbind(request,dat)
  }
  colnames(request)=matrix(c(attPerturb,attHold,"rep","seed"),nrow=1)
  
  #5. Read pars files
  pars=NULL
  for(i in 1:nRep){
    dat=matrix(as.numeric(utils::read.csv(file=paste0(path,"/pars",i,".csv"),header=F)),nrow=1)
    pars=rbind(pars,dat)
  }
  #colnames(pars) 
  
  #6. Read Scenario files
  sim=list()
  for(i in 1:nRep){
    dat=utils::read.csv(file=paste0(path,"/scenario",i,".csv"),header=T)
    sim[[i]]=dat
  }
 
  #write collated files is repack set to true
  if(repack==TRUE){
    #write combined files to directory
    write.csv(x=fitness,file=paste0(path,"/fitness.csv"),row.names = FALSE)
    write.csv(x=target,file=paste0(path,"/simulatedTarget.csv"),row.names = FALSE)
    write.csv(x=value,file=paste0(path,"/value.csv"),row.names = FALSE)
    write.csv(x=request,file=paste0(path,"/requestedTargets.csv"),row.names = FALSE)
    write.csv(x=pars,file=paste0(path,"/simulatedTargetParameters.csv"),row.names = FALSE)
  }

  #Also grab these bits of information separately
  reps=target[,'rep']
  seedIDs=target[,'seed']
  nTarget=determineNoTargets(exSpArgs=exSpArgs)
  locNo=rep(seq(1,nTarget),nSeed)   #vector of location numbers (i.e. all 1's represent the same requested target)
  

  #create list to be passed out
  out=list(data=sim,
           modelTag=modelTag,
           target=target,
           request=request,
           attPerturb=attPerturb,
           attHold=attHold,
           attSel=attSel, 
           attPrim=attPrim,
           # attObs=attObs, # put in next time
           nTarget=nTarget,    #
           nRep=nRep,
           seedIDs=seedIDs,
           locNo=locNo,
           fitness=fitness,
           nSeed=nSeed,
           exSpArgs=exSpArgs,
           simVar=as.vector(simVar))
  
 return(out) 
}

determineNoTargets<-function(exSpArgs=NULL){
  if(exSpArgs$type=="OAT"){
    nTarget=sum(exSpArgs$samp)
  }else{
    nTarget=prod(exSpArgs$samp)
  }    
  return(nTarget)
}
