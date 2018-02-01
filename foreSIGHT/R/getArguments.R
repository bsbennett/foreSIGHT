##################################
##  PREPARES ARGUMENTS FOR USE  ##
##################################

# CONTAINS
# get.varType() - grabs first part of string, sep specifiable  

getArguments<-function(attPerturb=attPerturb,attHold=attHold,optimArgs=optimArgs,exSpArgs=exSpArgs){
  
  #make attSel 

  attSel=c(attPerturb,attHold)
  
  #CHECKS FOR OPTIM-ARGS
  optimArgsdefault=list(pcrossover= 0.8,   # list of a parameters used by the ga optimiser (if used)
                        pmutation=0.1,
                        maxiter=50,
                        maxFitness=-0.001,
                        popSize = 500,
                        run=20,
                        seed = NULL,
                        parallel = FALSE,
                        keepBest=TRUE,
                        lambda.mult=0.0,
                        suggestions=NULL
  )
  
  optimArgs=modifyList(optimArgsdefault,optimArgs)
  
  
  if(is.character(exSpArgs)==TRUE){
    # READING FROM FILE (ASSUMED SELF-CHECKED)
    # AMEND THIS
  }else {
    # CHECKS FOR BOUNDS
    boundVars<-sapply(names(exSpArgs$bounds),get.varType,USE.NAMES=FALSE,sep="_")
    attVars<-sapply(attSel,get.varType,USE.NAMES = FALSE)
    boundNames<-names(exSpArgs$bounds)
    
    if(!is.null(exSpArgs$samp)){
      if(length(exSpArgs$samp)!=length(attPerturb)){
        stop("Samp needs to be specified for each attPerturb")
      }
    }
    
    n=length(attVars)                       ###Code for creating a default set of historical bounds
    boundsdefault=vector(length = n)
    for (i in 1:n) {
      if(attVars[i]=="Temp"){
        boundsdefault[i]=0
      } else {
        boundsdefault[i]=1
      }
    }
    tmp=as.list(boundsdefault)
    names(tmp)=attSel
    
    #this fills in samp/bounds
    exSpArgsdefault=list(type="regGrid",
                         samp=rep(1,n),
                         bounds=tmp)
    
    exSpArgs=modifyList(exSpArgsdefault,exSpArgs)
    exSpArgs$samp=c(exSpArgs$samp,rep(1,length(attHold)))
    
    
  
  }
  out=list(attSel=attSel,optimArgs=optimArgs,exSpArgs=exSpArgs)
  return(out)
}
