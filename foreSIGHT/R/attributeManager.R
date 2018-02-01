#######################################
##      ATTRIBUTE MANAGER            ##
#######################################

#CONTAINS
  #attribute.funcs - LIST (STORED in...)
  #attribute.calculator() - calculate values of attributes
  #attribute.info.check() - get targetType, varType and identify any invalid model selections
    #check.attribute.model.combo() - check if any attribute-model combos are invalid
    #get.attribute.info() - Identify invalid models
    #get.target.type() - treated as fractions, percent or abs value
    #get.attribute.varType() - "P", "Temp" OR ...


#---------------------------------------------------------------------------------------
#ATTRIBUTE FUNCTION LIST - CAN CALL SPECIFIC ATTRIBUTE CALCS FROM THE FUNCTION LIST
#list of attribute calculation functions - NB: all must have similar/same arguments
#used via lappply (attribute.funcs[attSel], function(f) f(data,datInd)) - return labelled list of outputs
#NEED TO THINK ABOUT WHAT ENVIRONMENT THIS IS STORED IN. #
attribute.funcs=list(
  P_ann_tot_m=function(data,datInd) extractor.summaryMean(func=sum,data=data,indx=datInd$i.yy,nperiod=datInd$nyr),                       # function labelled "Ptot_m" in list #get.tot(data)/datInd$nyr
  P_ann_dyWet_m=function(data,datInd) get.wet.average(data,threshold=0),            # function labelled "dyWet_m" in list 
  P_ann_dyAll_m=function(data,datInd) get.wet.average(data,threshold=-999), 
  P_ann_nWet_m=function(data,datInd) extractor.summaryMean(func=get.nwet,data=data,indx=datInd$i.yy,nperiod=datInd$nyr,threshold=0),  
  
  P_ann_DSD_m=function(data,datInd) get.cdd(data,datInd$i.yy,datInd$nyr),            # function labelled "annDSD_m" in list  
  P_ann_P99_m=function(data,datInd) extractor.summaryMean(func=get.quantile,data=data,indx=datInd$i.yy,nperiod=datInd$nyr,quant=0.99),
  P_ann_dyWet99p_m=function(data,datInd) extractor.summaryMean(func=get.quantile.wet,data=data,indx=datInd$i.yy,nperiod=datInd$nyr,quant=0.99),
  
  P_ann_avgWSD_m=function(data,datInd) mean(get.spell.lengths(data=data,thresh=0,type="wet"),na.rm=TRUE),
  P_ann_avgDSD_m=function(data,datInd) mean(get.spell.lengths(data=data,thresh=0,type="dry"),na.rm=TRUE),
  
  P_JJA_avgWSD_m=function(data,datInd) mean(get.spell.lengths(data=data[datInd$i.ss[[4]]],thresh=0,type="wet"),na.rm=TRUE),
  P_MAM_avgWSD_m=function(data,datInd) mean(get.spell.lengths(data=data[datInd$i.ss[[3]]],thresh=0,type="wet"),na.rm=TRUE),
  P_DJF_avgWSD_m=function(data,datInd) mean(get.spell.lengths(data=data[datInd$i.ss[[2]]],thresh=0,type="wet"),na.rm=TRUE),
  P_SON_avgWSD_m=function(data,datInd) mean(get.spell.lengths(data=data[datInd$i.ss[[1]]],thresh=0,type="wet"),na.rm=TRUE),
  
  P_JJA_avgDSD_m=function(data,datInd) mean(get.spell.lengths(data=data[datInd$i.ss[[4]]],thresh=0,type="dry"),na.rm=TRUE),
  P_MAM_avgDSD_m=function(data,datInd) mean(get.spell.lengths(data=data[datInd$i.ss[[3]]],thresh=0,type="dry"),na.rm=TRUE),
  P_DJF_avgDSD_m=function(data,datInd) mean(get.spell.lengths(data=data[datInd$i.ss[[2]]],thresh=0,type="dry"),na.rm=TRUE),
  P_SON_avgDSD_m=function(data,datInd) mean(get.spell.lengths(data=data[datInd$i.ss[[1]]],thresh=0,type="dry"),na.rm=TRUE),
  
  P_JJA_dyWet_m=function(data,datInd) extractor(func=get.wet.average,data=data,indx=datInd$i.ss[[4]],threshold=0),
  P_MAM_dyWet_m=function(data,datInd) extractor(func=get.wet.average,data=data,indx=datInd$i.ss[[3]],threshold=0),
  P_DJF_dyWet_m=function(data,datInd) extractor(func=get.wet.average,data=data,indx=datInd$i.ss[[2]],threshold=0),
  P_SON_dyWet_m=function(data,datInd) extractor(func=get.wet.average,data=data,indx=datInd$i.ss[[1]],threshold=0),
  
  P_JJA_dyAll_m=function(data,datInd) extractor(func=get.wet.average,data=data,indx=datInd$i.ss[[4]],threshold=-999),
  P_MAM_dyAll_m=function(data,datInd) extractor(func=get.wet.average,data=data,indx=datInd$i.ss[[3]],threshold=-999),
  P_DJF_dyAll_m=function(data,datInd) extractor(func=get.wet.average,data=data,indx=datInd$i.ss[[2]],threshold=-999),
  P_SON_dyAll_m=function(data,datInd) extractor(func=get.wet.average,data=data,indx=datInd$i.ss[[1]],threshold=-999),
  
  P_JJA_tot_m=function(data,datInd) extractor(func=get.avg.tot,data=data,indx=datInd$i.ss[[4]],nblock=datInd$nyr),
  P_MAM_tot_m=function(data,datInd) extractor(func=get.avg.tot,data=data,indx=datInd$i.ss[[3]],nblock=datInd$nyr),
  P_DJF_tot_m=function(data,datInd) extractor(func=get.avg.tot,data=data,indx=datInd$i.ss[[2]],nblock=datInd$nyr),
  P_SON_tot_m=function(data,datInd) extractor(func=get.avg.tot,data=data,indx=datInd$i.ss[[1]],nblock=datInd$nyr),
  
  P_ann_seasRatio_m=function(data,datInd) (extractor(func=get.avg.tot,data=data,indx=c(datInd$i.ss[[3]],datInd$i.ss[[4]]),nblock=datInd$nyr)/extractor(func=get.avg.tot,data=data,indx=c(datInd$i.ss[[1]],datInd$i.ss[[2]]),nblock=datInd$nyr)),
  
  P_seas_tot_cv=function(data,datInd) extractor.cv(func=get.avg.tot,data=data,indx=datInd$i.ss,nperiod=4,nblocks=datInd$nyr),
  P_mon_tot_cv=function(data,datInd) extractor.cv(func=get.avg.tot,data=data,indx=datInd$i.mm,nperiod=12,nblocks=datInd$nyr),
  
  # P_JJA_nWet_m=function(data,datInd) extractor.summaryMean(func=get.nwet,data=data,indx=datInd$i.ss[[4]],nperiod=datInd$nyr,threshold=0), 
  # P_MAM_nWet_m=function(data,datInd) extractor(func=get.avg.tot,data=data,indx=datInd$i.ss[[3]],nblock=datInd$nyr),
  # P_DJF_nWet_m=function(data,datInd) extractor(func=get.avg.tot,data=data,indx=datInd$i.ss[[2]],nblock=datInd$nyr),
  # P_SON_nWet_m=function(data,datInd) extractor(func=get.avg.tot,data=data,indx=datInd$i.ss[[1]],nblock=datInd$nyr),
  
  P_ann_maxWSD_m=function(data,datInd) extractor.summaryMean(func=CDWcalc,data=data,indx=datInd$i.yy,nperiod=datInd$nyr), #add fortran .so
  P_ann_maxDSD_m=function(data,datInd) extractor.summaryMean(func=CDDcalc,data=data,indx=datInd$i.yy,nperiod=datInd$nyr), #add fortran .so
  P_ann_R10_m=function(data,datInd) extractor.summaryMean(func=R10calc,data=data,indx=datInd$i.yy,nperiod=datInd$nyr),    #add fortran .so
  P_ann_90X_m=function(data,datInd) extractor.summaryMean(func=get.wet.tot,data=data,indx=datInd$i.yy,nperiod=datInd$nyr,threshold=9.6),
  
  Temp_ann_GSL_m=function(data,datInd) extractor.summaryMean(func=GSLcalc,data=data,indx=datInd$i.yy,nperiod=datInd$nyr),    #add fortran .so
  Temp_ann_CSL_m=function(data,datInd) extractor.summaryMean(func=CSLcalc,data=data,indx=datInd$i.yy,nperiod=datInd$nyr),    #add fortran .so
  
  Temp_ann_avg_m=function(data,datInd) extractor.summaryMean(func=mean,data=data,indx=datInd$i.yy,nperiod=datInd$nyr,na.rm=TRUE),
  Temp_ann_P5_m=function(data,datInd) extractor.summaryMean(func=get.quantile,data=data,indx=datInd$i.yy,nperiod=datInd$nyr,quant=0.05),
  Temp_ann_P95_m=function(data,datInd) extractor.summaryMean(func=get.quantile,data=data,indx=datInd$i.yy,nperiod=datInd$nyr,quant=0.95),
  Temp_ann_F0_m=function(data,datInd) extractor.summaryMean(func=F0calc,data=data,indx=datInd$i.yy,nperiod=datInd$nyr), #add fortran .so
  Temp_ann_rng_m=function(data,datInd) extractor.summaryMean(func=get.quantile.rng,data=data,indx=datInd$i.yy,nperiod=datInd$nyr),
  
  PET_ann_avg_m=function(data,datInd) extractor.summaryMean(func=mean,data=data,indx=datInd$i.yy,nperiod=datInd$nyr,na.rm=TRUE),
  PET_ann_tot_m=function(data,datInd) extractor.summaryMean(func=sum,data=data,indx=datInd$i.yy,nperiod=datInd$nyr),  
  PET_ann_rng_m=function(data,datInd) extractor.summaryMean(func=get.quantile.rng,data=data,indx=datInd$i.yy,nperiod=datInd$nyr),
  
  PET_ann_90pX_m=function(data,datInd) extractor.summaryMean(func=get.perc.above.thresh,data=data,indx=datInd$i.yy,nperiod=datInd$nyr,threshold=6.7)
  
  #insert the rest of the attributes 
  #-------------
)

#ATTRIBUTE CALCULATOR FUNCTION
attribute.calculator<-function(attSel=NULL,         #list of evaluated attribute names
                               data=NULL,           #timeseries data
                               datInd=NULL,         #dat indices and properties (e.g. datInd$nyr, datInd$i.yy)
                               attribute.funcs=NULL #list of attribute calculating functions
                               ){
  
  out=lapply(attribute.funcs[attSel], function(f) f(data,datInd)) #returns labelled list of outputs
  return(out)
 
}

#ATTRIBUTE AUX INFO (determine attribute type and if approved combo with model used)
attribute.info.check<-function(attSel=NULL  # vector of selected attributes (strings)
                              #simVar=NULL    # vector of variables simulated using models e.g. c("P","Temp")
                              # modelTag=NULL # model selected
){
  nAtt=length(attSel) # no. of attributes nominated
  attInfo=list()      #create blank list for storage
  
  #attribute name chopper function 
  attInfo$varType=vapply(attSel,FUN = get.attribute.varType,FUN.VALUE=character(1),USE.NAMES = FALSE) #drop use of names as comes ordered anyway
  
  #ASSIGN TARGET TYPE (IF P USE "FRAC", IF T USE "DIFF")
  attInfo$targetType=vapply(attInfo$varType,FUN=get.target.type,FUN.VALUE=character(1),USE.NAMES=FALSE)
  
  #CHECK FOR INVALID MODEL CHOICE - returns a logical for each attribute
  # attInfo$modelInvalid=vapply(attSel,FUN=check.attribute.model.combo,FUN.VALUE=logical(1),USE.NAMES=FALSE,modelTag=modelTag)
  
  return(attInfo)
}

get.att.ind<-function(attInfo=NULL,
                      simVar=NULL
){
  #DETERMINE WHICH ATTRIBUTE RELATES TO WHICH SIMULATOR
  attInd=list()
  if(simVar[1] != "All"){                    # ONLY DO IF STOCHASTIC GENERATION IS SELECTED (not simple scaling)
    for(i in 1:length(simVar)){
      attInd[[simVar[i]]]= which(attInfo$varType==simVar[i])
    }
  }
  return(attInd)
}

update.att.Info<-function(attInfo=NULL,
                          attInd=NULL,
                          modelTag=NULL,
                          simVar=NULL
){
  #divide up attInfo to different models
    for(i in 1:length(modelTag)){
      attInfo[[modelTag[i]]]$varType=attInfo$varType[attInd[[simVar[i]]]]
      attInfo[[modelTag[i]]]$targetType=attInfo$targetType[attInd[[simVar[i]]]]
    }
  return(attInfo)
}

#GETS VARTYPE BY READING FIRST ELEMENT OF ATTRIBUTE STRING
get.attribute.varType<-function(attrib=NULL, # attribute name
                                 sep="_"){
  varType=strsplit(x = attrib,split=sep)[[1]][1]
  return(varType)
}

#get.attribute.varType(attrib=attSel[1], sep="_")

#TARGET TYPE CLASSIFIER
get.target.type<-function(varType=NULL){
  if(varType == "P"){
    targetType="frac"
  }else{
      if(varType == "Temp"){
        targetType="diff"
      }else{
        targetType="frac"
      }
  }
  return(targetType)
}

tagBlender<-function(attLab=NULL
){
  
  chopped=strsplit(x = attLab,split="_")[[1]]
  
  #variable type
  if(chopped[1]== "P"){
    vtype="rainfall (fraction)"
  }else if(chopped[1]== "Temp"){
    vtype="temperature (additive change)"
  }else if(chopped[1]== "PET"){
    vtype="PET (fraction)"
  }
  
  #aggregation type
  if(chopped[2]== "ann"){
    atype="annual"
  }else if(chopped[2]== "JJA"){
    atype="JJA"
  }else if(chopped[2]== "MAM"){
    atype="MAM"
  }else if(chopped[2]== "DJF"){
    atype="DJF"
  }else if(chopped[2]== "SON"){
    atype="SON"
  }
  
  #metricType
  if(chopped[3]== "nWet"){
    mtype="no. wet days"
  }else if(chopped[3]== "dyWet"){
    mtype="wet day amount"
  }else if(chopped[3]== "DSD"){
    mtype="dryspell duration"
  }else if(chopped[3]== "P99"){
    mtype="99th percentile day amount"
  }else if(chopped[3]== "dyWet99p"){
    mtype="99th percentile wet day amount"
  }else if(chopped[3]== "avgWSD"){
    mtype="average wetspell duratio"
  }else if(chopped[3]== "avgDSD"){
    mtype="average dryspell duration"
  }else if(chopped[3]== "maxDSD"){
    mtype="max dryspell duration"
  }else if(chopped[3]== "maxWSD"){
    mtype="max wetspell duration"
  }else if(chopped[3]== "tot"){
    mtype="total"
  }else if(chopped[3]== "R10"){
    mtype="no. days above 10mm"
  }else if(chopped[3]== "GSL"){
    mtype="growing season length"
  }else if(chopped[3]== "CSL"){
    mtype="cold season length"
  }else if(chopped[3]== "avg"){
    mtype="average"
  }else if(chopped[3]== "P5"){
    mtype="5th percentile"
  }else if(chopped[3]== "P95"){
    mtype="95th percentile"
  }else if(chopped[3]== "F0"){
    mtype="frost days"
  }else if(chopped[3]== "rng"){
    mtype="range"
  }else if(chopped[3]== "90pX"){
    mtype="percent above historical 90th percentile"
  }else if(chopped[3]== "90X"){
    mtype="volume above historical 90th percentile"
  }else if(chopped[3]== "seasRatio"){
    mtype="ratio of wet to dry seasonal volume"
  }
  
  #statType
  if(chopped[4]== "m"){
    stype="Mean"  #as yet un-used
  }
  
  #stitch togther
  phrase=paste(stype,atype,mtype,vtype)
  phrase  
}


