################################
##  CHECKS FOR ARGUMENT TAGS  ##
################################

# CONTAINS
  # get.varType() - grabs first part of string, sep specifiable  
  # argument_check()- check duplicate attribute/model tags + check requests for two model types of one variable + checking other master control arguments
  
#---------------------------------------------------------------------------
#FUNCTIONS

#Function to split string and extract first component
get.varType<-function(attrib=NULL, # attribute name
                      sep="_"){
  varType=strsplit(x = attrib,split=sep)[[1]][1]
  return(varType)
}

#Function to check supplied arguments
argument_check<-function(names=NULL,
                         obs=NULL,
                         attSel=NULL,        
                         attPrim=NULL,
                         attHold=NULL,
                         attPerturb=NULL,
                         modelTag=NULL,
                         exSpArgs=NULL,
                         optimArgs=NULL,
                         file
                         ){
 
  names<-names[names!="year"];names<-names[names!="month"];names<-names[names!="day"]
  
  #CHECKS FOR ATTRIBUTES & MODELTAGS
  if(is.null(attPerturb)){
    logfile("Error: No attributes nominated for perturbation",file)
    logfile("Program terminated",file)
    stop("No attributes nominated for perturbation")
  }
  
  if (modelTag[1]=="Simple-ann") {
    if(length(attHold)!=0) {
      logfile("Error: Invalid - Simple scaling cannot hold attributes constant",file)
      logfile("Program terminated",file)
      stop("Simple scaling cannot hold attributes constant")
    }
  } else{
    if(is.null(attHold)){
      warn("Warning: no attributes held at historical levels",file)
    }
  }
  
  
  #CHECKs part 2 (check validity of these for new set up)
  if (modelTag[1]=="Simple-ann") {
    
    for (i in 1:length(attPerturb)){
    if(sum(attPerturb[i] %in% attributelist)==0){
      logfile("Error: attPerturb unrecognised",file)
      logfile("Program terminated",file)
      stop(paste0("attPerturb ",i," unrecognised"))
    }
    }
    
    if(length(attPrim)!=0) {
      logfile("Error: Simple scaling uses no primary attributes",file)
      logfile("Program terminated",file)
      stop("Simple scaling uses no primary attributes")
    }
    
    if(length(attPerturb)!=length(names)){
      logfile("Error: There is a mismatch between number of variables and number of attributes. These should be the same for simple scaling, which only has multiplicative or additive changes",file)
      logfile("Program terminated",file)
      stop("There is a mismatch between number of variables and number of attributes. These should be the same for simple scaling, which only has multiplicative or additive changes")
    }
    
    if (anyDuplicated(modelTag)!=0) {
      logfile("Error: There are multiple entries of the same model tag",file)
      logfile("Program terminated",file)
      stop("There are multiple entries of the same model tag")
    }
    
    
    
  } else {
    
    #CHECK FOR DUPLICATE TAGS
    if (anyDuplicated(attSel)!=0) {
      logfile("Error:There are multiple entries of the same attribute",file)
      logfile("Program terminated",file)
      stop("There are multiple entries of the same attribute")
    }
    
    if (anyDuplicated(attPrim)!=0) {
      logfile("Error: There are multiple entries of the same primary attribute",file)
      logfile("Program terminated",file)
      stop("There are multiple entries of the same primary attribute")
    }
    
    if (anyDuplicated(modelTag)!=0) {
      logfile("Error: There are multiple entries of the same model tag",file)
      logfile("Program terminated",file)
      stop("There are multiple entries of the same model tag")
    }
    
    #ENSURE PRIMARY ATTRIBUTES ARE ALSO LISTED IN ATTSEL
    # if(sum(attPrim %in% attSel)!=length(attPrim)){
    #   logfile("Error: Ensure that primary attributes are also listed in selected attributes",file)
    #   logfile("Program terminated",file)
    #   stop("Ensure that primary attributes are also listed in selected attributes")
    # }
    for(i in 1:length(modelTag)){
    if(sum(modelTag[i] %in% modelTaglist)==0){
      logfile("Error: modelTag unrecognised",file)
      logfile("Program terminated",file)
      stop(paste0("modelTag ",i," unrecognised"))
    }
    }
    
    for (i in 1:length(attPerturb)){
    if(sum(attPerturb[i] %in% attributelist)==0){
      logfile("Error: attPerturb unrecognised",file)
      logfile("Program terminated",file)
      stop(paste0("attPerturb ",i," unrecognised"))
    }
    }
    
    
    if(!is.null(attHold)){
    for (i in 1:length(attHold)){
    if(sum(attHold[i] %in% attributelist)==0){
      logfile("Error: attHold unrecognised",file)
      logfile("Program terminated",file)
      stop(paste0("attHold ",i," unrecognised"))
    }
    }
    }
    
    #CHECKS FOR TWO REQUESTED MODEL TYPES
    modelVars<-sapply(modelTag,get.varType,USE.NAMES=FALSE,sep="-")
    
    if (anyDuplicated(modelVars)!=0) {
      logfile("Error: There are multiple entries of a model type for one variable",file)
      logfile("Program terminated",file)
      stop("There are multiple entries of a model type for one variable")
    }
    
    ### Checks for columns of data without model tags.
    if (length(which((names %in% modelVars)==FALSE))>0) {
      warn("There is a mismatch between provided model types and supplied variables. Stochastic series will only be produced for supplied model tags",file)
      array<-c("year","month","day",modelVars)
      obs=obs[array]
    }
    
  }
  
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
  
  #CHECKS FOR LAMBDA VALUES
  if((length(attPrim!=0)) & (length(which(optimArgs$lambda.mult==0))>0)) {
    warn("There are specified Primary attributes with a lambda value of zero",file)
  }
  
  if(length(attPrim)>length(optimArgs$lambda.mult)){        # NO. OF ATTPRIM IS GREATER THAN LAMBDA VECTOR
    warn("There are more specified Primary attributes than lambda values",file)
    logfile("Error: check number of supplied lambda values",file)
    logfile("Program terminated",file)
    stop("Ensure a lambda value is entered for each Primary attribute")
  }else{
    note=paste0("Lambda(",attPrim,"): ",optimArgs$lambda.mult,collapse = ", ")
    progress(note,file)
    logfile(note,file)
  }
  
  ###Add more checks for ExSpArgs
  
  
  
  
  if(is.character(exSpArgs)==TRUE){
    # READING FROM FILE (ASSUMED SELF-CHECKED)
    # AMEND THIS
  }else {
    # CHECKS FOR BOUNDS
    boundVars<-sapply(names(exSpArgs$bounds),get.varType,USE.NAMES=FALSE,sep="_")
    attVars<-sapply(attSel,get.varType,USE.NAMES = FALSE)
    boundNames<-names(exSpArgs$bounds)
    
    if (modelTag[1]=="Simple-ann") {
      
      if (!isTRUE(all(boundVars==attVars))) {
        logfile("Error: Ensure bounds are entered for each variable in provided data",file)
        logfile("Program terminated",file)
        stop("Ensure bounds are entered for each variable in provided data")
      }
      
    } else {
      
      #switch to attPerturb
      if (!isTRUE(all(boundNames==attSel))) { 
        logfile("Error: Enter bounds for each attribute in attSel",file)
        logfile("Program terminated",file)
        stop("Enter bounds for each attribute in attSel")
      }
    } 
    
    
}
  
  
}


#TESTER
# exSpArgs=list(type="regGrid",
#               samp=5,
#               bounds=list(P_ann_tot_m=c(0.7,1.3),
#                           P_ann_P99_m=0,
#                           Temp_ann_avg_m=c(-6,6)))
# attSel=c("P_ann_tot_m", "P_ann_P99_m", "Temp_ann_avg_m")
# attPrim=c("P_ann_tot_m", "Temp_ann_avg_m")
# modelTag=c("P-ann-wgen", "Temp-har26-wgen")
# lambda.mult=0