 ################################
#### STOCHASTIC PAR MANAGER ####
################################

#CONTAINS
  #get.model.info() - based on model tag gets general model information (e.g. nperiods in a year, no. harmonic cycles fitted)
  #par.manager() - based on model tag converts input vector of pars into pdd, pwd, alpha & beta vectors of nperiod length
  #init.calib() - initial calibration function. Uses obs data to fit model to baseline climate.
    #uses model tag to determine what should be fitted (e.g. how many periods, using harmonic, etc.)
    #gammaParsMLE2() - args(dat, wetThresh)
    #pdd.pwd.estimator() -args (dat,ind,threshold)
  #simHarTS.parmanager() - based on model tag converts input vector of pars into cor0, cor1, Hmean, Hsd vectors of period length
  #whichPars
#-------------------------------------------------------------------------------------------------------------
get.model.info<-function(modelTag=NULL #string used to specify model for stochastic generation
  ){
  
  modelInfo=list()
  #SET UP MODEL RELATED PARAMETERS
  switch(modelTag,
         "Simple-ann"  = {modelInfo$simVar="All"
                          modelInfo$simPriority=1 
                          modelInfo$nperiod=1
                          },
         
         "P-seas-wgen" = {modelInfo$simVar="P"
                          modelInfo$simPriority=1
                          modelInfo$nperiod=4       # 4 periods in a year
                          modelInfo$fixedPars=NA    # No fixed pars
                          modelInfo$ncycle=NA       # No harmonic fit
                          modelInfo$npars=modelInfo$nperiod*4 #par vector is of length 16
                          modelInfo$parNam=c("pdd_1","pdd_2","pdd_3","pdd_4",
                                             "pwd_1","pwd_2","pwd_3","pwd_4",
                                             "alpha_1","alpha_2","alpha_3","alpha_4",
                                             "beta_1","beta_2","beta_3","beta_4")
                          modelInfo$minBound=c(0.389, 0.334, 0.375, 0.277, 0.078, 0.079, 0.084, 0.036, 
                                               0.295,	0.303, 0.309,	0.257, 0.043,	0.046, 0.048, 0.034) #Aus 3stdev hard bounds
                          modelInfo$maxBound=c(0.997, 0.989, 0.994, 0.998, 0.85, 0.714, 0.714, 0.808, 
                                               0.998, 0.998, 0.998, 0.998, 15.716, 30.08, 27.877, 21.193)
                          #bounds here?????????????
                          #npar.optim???? - then split into max, min bounds
                          },
         "P-ann-wgen" = {modelInfo$simVar="P"
                         modelInfo$simPriority=1
                         modelInfo$nperiod=1
                         modelInfo$fixedPars=NA
                         modelInfo$ncycle=NA
                         modelInfo$npars=modelInfo$nperiod*4       #par vector is of length 4
                         modelInfo$parNam=c("pdd","pwd","alpha","beta")
                         modelInfo$minBound=c(0.427, 0.088, 0.313, 0.043) #Aus 3stdev hard bounds
                         modelInfo$maxBound=c(0.998, 0.824, 0.998, 25.46)
                        },
         "P-har12-wgen" = {modelInfo$simVar="P"
                           modelInfo$simPriority=1
                           modelInfo$nperiod=12
                           modelInfo$fixedPars=NA
                           modelInfo$ncycle=1 
                           modelInfo$npars=4*(1+modelInfo$ncycle*2)  #par vector is of length 12 
                           modelInfo$parNam=c("pdd_m","pdd_amp","pdd_ang",
                                              "pwd_m","pwd_amp","pwd_ang",
                                              "alpha_m","alpha_amp","alpha_ang",
                                              "beta_m","beta_amp","beta_ang")
                           # modelInfo$minBound=c(0.376, 0.006, 3,
                           #                      0.093, 0.004, 3, 
                           #                      0.33, 0.002, -1.559, 
                           #                      0.085, 0.028, -0.223) #Aus 3stdev hard bounds
                           # modelInfo$maxBound=c(0.986, 0.357, 3.5, 
                           #                      0.728, 0.319, 3.5, 
                           #                      0.979, 0.248, 3.13, 
                           #                      19.723, 13.568, 2.743)
                           # modelInfo$minBound=c(0.771, 0.055, -0.916,
                           #                      0.413, 0.052, -0.864,
                           #                      0.561, 0.071, 1.161,
                           #                      6.174, 1.853, -0.193) #Aus 3stdev hard bounds
                           # modelInfo$maxBound=c(0.773, 0.057, -0.918,
                           #                      0.415, 0.054, -0.866,
                           #                      0.563, 0.073, 1.163,
                           #                      6.176, 1.855, -0.195)
                           modelInfo$minBound=c(0.476, 0.006, 0.730,
                                                0.093, 0.004, 0.543,
                                                0.33, 0.002, 4.108,
                                                0.085, 0.028, 1.348) #Aus 3stdev hard bounds
                           modelInfo$maxBound=c(0.950, 0.257, 0.733,
                                                0.728, 0.319, 0.545,
                                                0.950, 0.200, 4.110,
                                                15.00, 6.50, 1.350)
                         },
         "P-har6-wgen" = {modelInfo$simVar="P"
                           modelInfo$simPriority=1
                           modelInfo$nperiod=6
                           modelInfo$fixedPars=NA
                           modelInfo$ncycle=1 
                           modelInfo$npars=4*(1+modelInfo$ncycle*2)  #par vector is of length 12 
                           modelInfo$parNam=c("pdd_m","pdd_amp","pdd_ang",
                                              "pwd_m","pwd_amp","pwd_ang",
                                              "alpha_m","alpha_amp","alpha_ang",
                                              "beta_m","beta_amp","beta_ang")
                           modelInfo$minBound=c(0.376, 0.006, 3,
                                                0.093, 0.004, 3, 
                                                0.33, 0.002, -1.559, 
                                                0.085, 0.028, -0.223) #Aus 3stdev hard bounds
                           modelInfo$maxBound=c(0.986, 0.357, 3.5, 
                                                0.728, 0.319, 3.5, 
                                                0.979, 0.248, 3.13, 
                                                19.723, 13.568, 2.743)
                        },
         
         "P-har26-wgen" = {modelInfo$simVar="P"
                           modelInfo$simPriority=1
                           modelInfo$nperiod=26
                           modelInfo$fixedPars=NA
                           modelInfo$ncycle=1 
                           modelInfo$npars=4*(1+modelInfo$ncycle*2)  #par vector is of length 12 
                           modelInfo$parNam=c("pdd_m","pdd_amp","pdd_ang",
                                              "pwd_m","pwd_amp","pwd_ang",
                                              "alpha_m","alpha_amp","alpha_ang",
                                              "beta_m","beta_amp","beta_ang")
                           modelInfo$minBound=c(0.376, 0.006, 0.580, 
                                                0.093, 0.004, 0.461, 
                                                0.33, 0.002, 3.940, 
                                                0.085, 0.028, 1.200) #Aus 3stdev hard bounds
                           modelInfo$maxBound=c(0.986, 0.357, 0.584, 
                                                0.728, 0.319, 0.464, 
                                                0.979, 0.248, 3.942, 
                                                19.723, 13.568, 1.202)
                        },


         "P-2har26-wgen" = {modelInfo$simVar="P"
                            modelInfo$simPriority=1
                            modelInfo$nperiod=26
                            modelInfo$fixedPars=NA
                            modelInfo$ncycle=2 
                            modelInfo$npars=4*(1+modelInfo$ncycle*2)  #par vector is of length 20
                            modelInfo$parNam=c("pdd_m","pdd_amp1","pdd_amp2","pdd_ang1","pdd_ang2",
                                               "pwd_m","pwd_amp1","pwd_amp2","pwd_ang1","pwd_ang2",
                                               "alpha_m","alpha_amp1","alpha_amp2","alpha_ang1","alpha_ang2",
                                               "beta_m","beta_amp1","beta_amp2","beta_ang1","beta_ang2")
                            modelInfo$minBound=c(0.376, 0.006, 0.002, -1.462, -1.184, 0.093, 0.004, 
                                                 0.001, -1.55, -1.338, 0.33, 0.002, 0.001, -1.559, 
                                                 -1.539, 0.085, 0.028, 0.011, -0.223, -1.424) #Aus 3stdev hard bounds
                            modelInfo$maxBound=c(0.986, 0.357, 0.087, 2.931, 2.941, 0.728, 0.319, 
                                                 0.093, 3.118, 3.015, 0.979, 0.248, 0.129, 3.13, 
                                                 3.131, 19.723, 13.568, 6.366, 2.743, 2.948)
         },
         "Temp-har26-wgen-wd" = {modelInfo$simVar="Temp"
                                 modelInfo$simPriority=2
                                 modelInfo$nAssocSeries=0
                                 modelInfo$WDcondition=TRUE  #conditioned on wet/dry status
                                 modelInfo$wdCycle="All"
                                 modelInfo$nperiod=26
                                 modelInfo$fixedPars=NA
                                 modelInfo$ncycle=1 
                                 modelInfo$npars=4*(1+modelInfo$ncycle*2)+1             #par vector is of length  13
                                 modelInfo$parNam=c("cor0",
                                                    "W-mCycle-m","W-mCycle-amp","W-mCycle-ang",
                                                    "W-sCycle-m","W-sCycle-amp","W-sCycle-ang",
                                                    "D-mCycle-m","D-mCycle-amp","D-mCycle-ang",
                                                    "D-sCycle-m","D-sCycle-amp","D-sCycle-ang")
                                 
                                 modelInfo$minBound=c(0.45,7.0,1.0,-0.05,0.9,0.1,-1.6,7.0,1.0,-0.05,0.9,0.1,-1.6) #Placeholder bounds
                                 modelInfo$maxBound=c(0.90,28.0,9.0,0.81,4.9,1.4,3.15,28.0,9.0,0.81,4.9,1.4,3.15)
         },
         "Temp-har26-wgen" = {modelInfo$simVar="Temp"
                              modelInfo$simPriority=2
                              modelInfo$nAssocSeries=0
                              modelInfo$WDcondition=FALSE  #conditioned on wet/dry status
                              modelInfo$wdCycle=FALSE
                              modelInfo$nperiod=26
                              modelInfo$fixedPars=NA
                              modelInfo$ncycle=1 
                              modelInfo$npars=2*(1+modelInfo$ncycle*2)+1             #par vector is of length  7
                              modelInfo$parNam=c("cor0",
                                                 "WD-mCycle-m","WD-mCycle-amp","WD-mCycle-ang",
                                                 "WD-sCycle-m","WD-sCycle-amp","WD-sCycle-ang")
                              modelInfo$minBound=c(0.45,7.0,1.0,-0.05,0.9,0.1,-1.6) #Placeholder bounds
                              modelInfo$maxBound=c(0.9,28.0,9.0,0.81,4.9,1.4,3.15)
                              },
         "Temp-har26-wgen-wdsd" = {modelInfo$simVar="Temp"
                                   modelInfo$simPriority=2
                                   modelInfo$nAssocSeries=0
                                   modelInfo$WDcondition=TRUE  #conditioned on wet/dry status
                                   modelInfo$wdCycle="sCycle"
                                   modelInfo$nperiod=26
                                   modelInfo$fixedPars=NA
                                   modelInfo$ncycle=1 
                                   modelInfo$npars=3*(1+modelInfo$ncycle*2)+1             #par vector is of length  10
                                   modelInfo$parNam=c("cor0",
                                                      "WD-mCycle-m","WD-mCycle-amp","WD-mCycle-ang",
                                                      "W-sCycle-m","W-sCycle-amp","W-sCycle-ang",
                                                      "D-sCycle-m","D-sCycle-amp","D-sCycle-ang")
                                   modelInfo$minBound=c(0.45,7.0,1.0,-0.05,0.9,0.1,-1.6,0.9,0.1,-1.6) #aus bounds
                                   modelInfo$maxBound=c(0.90,28.0,9.0,0.81,4.9,1.4,3.15,4.9,1.4,3.15)
                                    },
         "PET-har12-wgen" = {modelInfo$simVar="PET"
                             modelInfo$simPriority=2
                             modelInfo$nAssocSeries=0
                             modelInfo$WDcondition=FALSE  #conditioned on wet/dry status
                             modelInfo$wdCycle=FALSE
                             modelInfo$nperiod=12
                             modelInfo$fixedPars=NA
                             modelInfo$ncycle=1 
                             modelInfo$npars=2*(1+modelInfo$ncycle*2)+1             #par vector is of length  7
                             modelInfo$parNam=c("cor0",
                                                "WD-mCycle-m","WD-mCycle-amp","WD-mCycle-ang",
                                                "WD-sCycle-m","WD-sCycle-amp","WD-sCycle-ang")
                             modelInfo$minBound=c(0.0, 
                                                  0,0.01,0.2,
                                                  0.01,0.01,0.2)  #NB: Placeholder bounds
                             modelInfo$maxBound=c(0.9,
                                                  6,5,0.3,
                                                  3,3,0.3)
                            },
         "PET-har26-wgen" = {modelInfo$simVar="PET"
                              modelInfo$simPriority=2
                              modelInfo$nAssocSeries=0
                              modelInfo$WDcondition=FALSE  #conditioned on wet/dry status
                              modelInfo$wdCycle=FALSE
                              modelInfo$nperiod=26
                              modelInfo$fixedPars=NA
                              modelInfo$ncycle=1 
                              modelInfo$npars=2*(1+modelInfo$ncycle*2)+1             #par vector is of length  7
                              modelInfo$parNam=c("cor0",
                                                "WD-mCycle-m","WD-mCycle-amp","WD-mCycle-ang",
                                                "WD-sCycle-m","WD-sCycle-amp","WD-sCycle-ang")
                              modelInfo$minBound=c(0.0  ,0  ,0.01 ,0.2 ,1 ,0.4 ,0.2)  #NB: Placeholder bounds
                              modelInfo$maxBound=c(0.9  ,6  ,5    ,0.3 ,3    ,3    ,0.3)
                               },
         "PET-har26-wgen-wd" = {modelInfo$simVar="PET"
                                 modelInfo$simPriority=2
                                 modelInfo$nAssocSeries=0
                                 modelInfo$WDcondition=TRUE  #conditioned on wet/dry status
                                 modelInfo$wdCycle="All"
                                 modelInfo$nperiod=26
                                 modelInfo$fixedPars=NA
                                 modelInfo$ncycle=1 
                                 modelInfo$npars=4*(1+modelInfo$ncycle*2)+1             #par vector is of length  13
                                 modelInfo$parNam=c("cor0",
                                                    "W-mCycle-m","W-mCycle-amp","W-mCycle-ang",
                                                    "W-sCycle-m","W-sCycle-amp","W-sCycle-ang",
                                                    "D-mCycle-m","D-mCycle-amp","D-mCycle-ang",
                                                    "D-sCycle-m","D-sCycle-amp","D-sCycle-ang")
                                 
                                 modelInfo$minBound=c(0.001,
                                                      0.01,0.01,0.95,
                                                      0.01,0.01,0.9,
                                                      0.01,0.01,0.95,
                                                      0.01,0.01,0.9) #Placeholder bounds
                                 modelInfo$maxBound=c(0.95,
                                                      30.0,10.0,1.1,
                                                      10.0,10.0,1.05,
                                                      30.0,9.0,1.1,
                                                      10.0,10.0,1.05)
         },
         #--- MORE VERSIONS COMING ---
         # "P-har26-wgen-FS" = {modelInfo$simVar="P"
         # modelInfo$nperiod=26
         #                     modelInfo$fixedPars="phase.angle"
         #                     modelInfo$ncycle=1
         #                     modelInfo$npars=4*(1+modelInfo$ncycle*1)  #par vector is of length 8
         #                     },
         # "P-2har26-wgen-FS" = {modelInfo$simVar="P"
         # modelInfo$nperiod=26
         #                       modelInfo$fixedPars="phase.angle"
         #                       modelInfo$ncycle=2
         #                       modelInfo$npars=4*(1+modelInfo$ncycle*1)  #par vector is of length 12
         # },
         # versions where occurence w/d is kept the same as current
         
         -999
         )
  return(modelInfo)
  
}

#RETRUN VARIOUS PARS
return.simPriority<-function(modelInfo=NULL){
  return(modelInfo$simPriority)
}
return.simVar<-function(modelInfo=NULL){
  return(modelInfo$simVar)
}

#PARAMETER MANAGER FOR WGEN STYLE RAIN SIMULATOR
  #NPERIOD, I.PP , DATIND INFO
  #IF NEEDED (E.G. HARMONIC) 
par.manager<-function(parS=NULL,         # pars to split
                      modelInfo=NULL,    # assoicated model info
                      modelTag=NULL,     # model label
                      initCalibPars=NULL # par series from intial calib to obs
  ){
  
  #IF NO HARMONIC OR PARAMETER FIXING APPLIED IN MODEL VERSION
  if(is.na(modelInfo$ncycle) & is.na(modelInfo$fixedPars)){
    #check length(pars == modelInfo$npars)   # if it fails put in a warning
    pdd=parS[1:modelInfo$nperiod]                                 # extract first set of pars (pars evenly split across vector)
    pwd=parS[(modelInfo$nperiod+1):(2*modelInfo$nperiod)]
    alpha=parS[(2*modelInfo$nperiod+1):(3*modelInfo$nperiod)]
    beta=parS[(3*modelInfo$nperiod+1):(4*modelInfo$nperiod)]
  }
  
  
  #if harmonics are required and no pars fixed - fit them
   if(!is.na(modelInfo$ncycle) & is.na(modelInfo$fixedPars)){
     npos=1
     st.parset=(npos-1)*(2*modelInfo$ncycle+1)+1       # start position in vector for this parameter set
     pdd=harmonicFunc(x=seq(1,modelInfo$nperiod),
                  mean=parS[st.parset],
                  amp=parS[(st.parset+1):(st.parset+modelInfo$ncycle)],
                  phase.ang=parS[(st.parset+modelInfo$ncycle+1):((st.parset+2*modelInfo$ncycle))],
                  k=modelInfo$ncycle,
                  nperiod=modelInfo$nperiod)
     
     npos=2
     st.parset=(npos-1)*(1+2*modelInfo$ncycle)+1       # start position in vector for this parameter set
     pwd=harmonicFunc(x=seq(1,modelInfo$nperiod),
                      mean=parS[st.parset],
                      amp=parS[(st.parset+1):(st.parset+modelInfo$ncycle)],
                      phase.ang=parS[(st.parset+modelInfo$ncycle+1):((st.parset+2*modelInfo$ncycle))],
                      k=modelInfo$ncycle,
                      nperiod=modelInfo$nperiod)
     
     npos=3
     st.parset=(npos-1)*(1+2*modelInfo$ncycle)+1       # start position in vector for this parameter set
     alpha=harmonicFunc(x=seq(1,modelInfo$nperiod),
                      mean=parS[st.parset],
                      amp=parS[(st.parset+1):(st.parset+modelInfo$ncycle)],
                      phase.ang=parS[(st.parset+modelInfo$ncycle+1):((st.parset+2*modelInfo$ncycle))],
                      k=modelInfo$ncycle,
                      nperiod=modelInfo$nperiod)
     
     npos=4
     st.parset=(npos-1)*(1+2*modelInfo$ncycle)+1       # start position in vector for this parameter set
     beta=harmonicFunc(x=seq(1,modelInfo$nperiod),
                      mean=parS[st.parset],
                      amp=parS[(st.parset+1):(st.parset+modelInfo$ncycle)],
                      phase.ang=parS[(st.parset+modelInfo$ncycle+1):((st.parset+2*modelInfo$ncycle))],
                      k=modelInfo$ncycle,
                      nperiod=modelInfo$nperiod)
    
   }
  

  #CONDITIONS WHERE SOME PARS ARE FIXED
    #FIXED SEASONALITY OR OCCURANCE OR ...
    #INITICALIBPARS
    #need sytem for each type of fixing
    # if modelInfo$fixedPars="phase.angle"...
    #if(!is.na(modelInfo$ncycle) & (modelInfo$fixedPars == "phase.angle")){}
    # if modelInfo$fixedPars=
    
  
  
  
   #out is to - CALCULATE PAR VECTORS (PDD,PWD,ALPA,BETA)
  out=list(pdd=pdd,
           pwd=pwd,
           alpha=alpha,
           beta=beta)
   return(out)
}

init.calib<- function(modelTag=NULL,  #model identifier
                      modelInfo=NULL, #model information based on model identifier
                      data=NULL,      #observed data (data frame) to be used in calibration
                      datInd=NULL     #date index information
  ){

  #FOR ALL MODELS
    pdd=rep(0,modelInfo$nperiod); alpha=beta=pwd=pdd    #make space to store fitted pars
    for(p in 1:modelInfo$nperiod){
      #fit pwd and pdd
      tmp=pdd.pwd.estimator(dat=data,ind=datInd$i.pp[[p]],threshold=0.00)
      pdd[p]=tmp$pdd; pwd[p]=tmp$pwd

      #fit gamma pars (alpha & beta - using wgen manual labelling convention)
      tmp=gammaParsMLE2(dat=data[datInd$i.pp[[p]]],wetThresh=0.00)
      alpha[p]=tmp$shape; beta[p]=tmp$scale
    }
  
    
  
  # MODELS WHERE A HARMONIC IS USED
    #EACH PAR (PDD, PWD, ALPHA, BETA) IS FIT TO A HARMONIC (SOME EXCEPTIONS/SPECIAL CASES)
    if(!is.na(modelInfo$ncycle)){
      #fit pwd, pdd, alpha, beta
      pdd.fit=fit.harmonic.opts(v.stat=pdd,k=modelInfo$ncycle,nperiod=modelInfo$nperiod)  #from harmonicFit.R
      pwd.fit=fit.harmonic.opts(v.stat=pwd,k=modelInfo$ncycle,nperiod=modelInfo$nperiod)
      alpha.fit=fit.harmonic.opts(v.stat=alpha,k=modelInfo$ncycle,nperiod=modelInfo$nperiod)
      beta.fit=fit.harmonic.opts(v.stat=beta,k=modelInfo$ncycle,nperiod=modelInfo$nperiod)
      #PARS MUST BE ARANGED IN ORDER (MEAN, AMP, PHASE ANGLE) FOR EACH FITTED PAR
      initCalibPars=c(unlist(pdd.fit,use.names = FALSE),
                      unlist(pwd.fit,use.names = FALSE),
                      unlist(alpha.fit,use.names = FALSE),
                      unlist(beta.fit,use.names = FALSE))
    }else{
      #MAKE PAR VECTOR C(PAR1 X NPERIOD),(PAR2 X NPERIOD),...)
      initCalibPars=c(pdd,pwd,alpha,beta)
    }
    
    #PLOT UP
    # windows();par(mfrow=c(2,2))
    # plot(x=seq(1,modelInfo$nperiod),pdd,pch=16,xlab="period")
    # cycle.pdd=harmonicFunc(x=seq(1,modelInfo$nperiod),mean=pdd.fit$mean,amp=pdd.fit$amp,phase.ang=pdd.fit$phase.ang,k=modelInfo$ncycle,nperiod=modelInfo$nperiod)
    # lines(x=seq(1,modelInfo$nperiod),cycle.pdd,col="red")
    # title("pdd")
    # plot(x=seq(1,modelInfo$nperiod),pwd,pch=16,xlab="period")
    # cycle.pwd=harmonicFunc(x=seq(1,modelInfo$nperiod),mean=pwd.fit$mean,amp=pwd.fit$amp,phase.ang=pwd.fit$phase.ang,k=modelInfo$ncycle,nperiod=modelInfo$nperiod)
    # lines(x=seq(1,modelInfo$nperiod),cycle.pwd,col="red")
    # title("pwd")
    # plot(x=seq(1,modelInfo$nperiod),alpha,pch=16,xlab="period")
    # alpha.cycle=harmonicFunc(x=seq(1,modelInfo$nperiod),mean=alpha.fit$mean,amp=alpha.fit$amp,phase.ang=alpha.fit$phase.ang,k=modelInfo$ncycle,nperiod=modelInfo$nperiod)
    # lines(x=seq(1,modelInfo$nperiod),alpha.cycle,col="red")
    # title("alpha")
    # plot(x=seq(1,modelInfo$nperiod),beta,pch=16,xlab="period")
    # beta.cycle=harmonicFunc(x=seq(1,modelInfo$nperiod),mean=beta.fit$mean,amp=beta.fit$amp,phase.ang=beta.fit$phase.ang,k=modelInfo$ncycle,nperiod=modelInfo$nperiod)
    # lines(x=seq(1,modelInfo$nperiod),beta.cycle,col="red")
    # title("beta")
    
    
    #return pars from initial calibration
    return(initCalibPars)
}

#------------------------------------------------------------------------------------------
gammaParsMLE2<- function(dat=NULL, #vector timeseries of rainfall amounts
                         wetThresh=0.01, #threshold at which day is deemed wet
                         ...){
  
  x=dat[which(dat>wetThresh)]    #get wet day amounts
  nw=length(x)                   #no. wet days
  x.mean=mean(x)                 #arithmetic mean of wet days
 
  s=log(x.mean)-sum(log(x))/nw
  #est.shape - note in wgen manual shape is denoted using alpha
  # ML method to estimate the 2 parameters of the gamma distribution from
  # Wiki - https://en.wikipedia.org/wiki/Gamma_distribution#Characterization_using_shape_.CE.B1_and_rate_.CE.B2
 # k.est=(3-s+sqrt((s-3)^2+24*s))/(12*s)
  #estimator from wgen manual
  Anum=8.898919+9.05995*s+0.9775373*s^2.0
  Adom=s*(17.79728+11.968477*s+s^2.0)
  k.est=Anum/Adom
  if(k.est >=1.0){k.est=0.998}
  
  #est.scale - Note in wgen manual scale is denoted by beta
  theta.est=x.mean/k.est
  
  out=list(scale=theta.est,shape=k.est)
  return(out)
  
}

#---------------------------------------------------------------------------
#Estimate pdd and pwd
pdd.pwd.estimator<-function(dat=NULL,       # vector of rainfall values
                            ind=NULL,       # indexes of days to assess
                            threshold=0.01  # wet threshold
){
  n=length(ind)
  nw=length(which(dat[ind]>threshold))
  nd=n-nw
  #pDry=nd/n
  
  ind.prior=ind[-n]; ind.now=ind[-n]+1  #for clarity spell out which is the prior day series and current day series (DROP LAST VALUE TO AVOID ARRAY OVERFLOW)
  
  ind.wd.p=ind[which((dat[ind.prior])>threshold & (dat[ind.now])<=threshold)]  # GET INDICES OF WET(i-1) - DRY(i) PAIRS
  n.wd=length(ind.wd.p)
  
  ind.dw.p=ind[which((dat[ind.prior])<=threshold & (dat[ind.now])>threshold)]  # GET INDICES OF DRY(i-1) - WET(i) PAIRS
  n.dw=length(ind.dw.p)
  
  probs=list(pwd=(n.wd/nw),pdd=(1-(n.dw/nd)))
  return(probs)
}


#parMangement for TS generation
simHarTS.parmanager<-function(parS=NULL,   #par vector to be divided up (cors, wet (mean par, sd pars), dry (mean pars,sd pars))
                              modelTag=NULL,
                              modelInfo=NULL,
                              initCalibPars=NULL
){
  
  #position calculator function for harmonic pars
  posCalc<-function(npos,nAssocSeries,ncycle){st.pos=(npos-1+nAssocSeries)*(2*ncycle+1)+2}
  
  Hmean=list(); Hsd=list()
  
  #if harmonics are required, no pars fixed & conditional on wd status - fit them
  if(!is.na(modelInfo$ncycle) & is.na(modelInfo$fixedPars)){
    if(modelInfo$nAssocSeries ==0){  #If series residual generated alone
      cor1=parS[1]
      cor0=1
    }else{
      #INSERT WARNING
      #put on to-do list
      
      stop("Missing capcity to generate correl residuals")
    }
    
    if(modelInfo$WDcondition == TRUE){
      if(modelInfo$wdCycle == "sCycle"){
        #MAKE H MEANS WET AND DRY THE SAME
        st.parset=posCalc(npos=1,nAssocSeries=modelInfo$nAssocSeries,ncycle=modelInfo$ncycle)     # start position in vector for this parameter set
        Hmean$W=harmonicFunc(x=seq(1,modelInfo$nperiod),
                             mean=parS[st.parset],
                             amp=parS[(st.parset+1):(st.parset+modelInfo$ncycle)],
                             phase.ang=parS[(st.parset+modelInfo$ncycle+1):((st.parset+2*modelInfo$ncycle))],
                             k=modelInfo$ncycle,
                             nperiod=modelInfo$nperiod)
        
        Hmean$D=harmonicFunc(x=seq(1,modelInfo$nperiod),
                             mean=parS[st.parset],
                             amp=parS[(st.parset+1):(st.parset+modelInfo$ncycle)],
                             phase.ang=parS[(st.parset+modelInfo$ncycle+1):((st.parset+2*modelInfo$ncycle))],
                             k=modelInfo$ncycle,
                             nperiod=modelInfo$nperiod)
        
        #MAKE H SD WET AND DRY CONDITIONAL
        st.parset=posCalc(npos=2,nAssocSeries=modelInfo$nAssocSeries,ncycle=modelInfo$ncycle)         # start position in vector for this parameter set
        Hsd$W=harmonicFunc(x=seq(1,modelInfo$nperiod),
                           mean=parS[st.parset],
                           amp=parS[(st.parset+1):(st.parset+modelInfo$ncycle)],
                           phase.ang=parS[(st.parset+modelInfo$ncycle+1):((st.parset+2*modelInfo$ncycle))],
                           k=modelInfo$ncycle,
                           nperiod=modelInfo$nperiod)
        
        st.parset=posCalc(npos=3,nAssocSeries=modelInfo$nAssocSeries,ncycle=modelInfo$ncycle)          # start position in vector for this parameter set
        Hsd$D=harmonicFunc(x=seq(1,modelInfo$nperiod),
                           mean=parS[st.parset],
                           amp=parS[(st.parset+1):(st.parset+modelInfo$ncycle)],
                           phase.ang=parS[(st.parset+modelInfo$ncycle+1):((st.parset+2*modelInfo$ncycle))],
                           k=modelInfo$ncycle,
                           nperiod=modelInfo$nperiod)
        
      }else{
        st.parset=posCalc(npos=1,nAssocSeries=modelInfo$nAssocSeries,ncycle=modelInfo$ncycle)     # start position in vector for this parameter set
        Hmean$W=harmonicFunc(x=seq(1,modelInfo$nperiod),
                             mean=parS[st.parset],
                             amp=parS[(st.parset+1):(st.parset+modelInfo$ncycle)],
                             phase.ang=parS[(st.parset+modelInfo$ncycle+1):((st.parset+2*modelInfo$ncycle))],
                             k=modelInfo$ncycle,
                             nperiod=modelInfo$nperiod)
        
        st.parset=posCalc(npos=2,nAssocSeries=modelInfo$nAssocSeries,ncycle=modelInfo$ncycle)         # start position in vector for this parameter set
        Hsd$W=harmonicFunc(x=seq(1,modelInfo$nperiod),
                           mean=parS[st.parset],
                           amp=parS[(st.parset+1):(st.parset+modelInfo$ncycle)],
                           phase.ang=parS[(st.parset+modelInfo$ncycle+1):((st.parset+2*modelInfo$ncycle))],
                           k=modelInfo$ncycle,
                           nperiod=modelInfo$nperiod)
        
        st.parset=posCalc(npos=3,nAssocSeries=modelInfo$nAssocSeries,ncycle=modelInfo$ncycle)         # start position in vector for this parameter set
        Hmean$D=harmonicFunc(x=seq(1,modelInfo$nperiod),
                             mean=parS[st.parset],
                             amp=parS[(st.parset+1):(st.parset+modelInfo$ncycle)],
                             phase.ang=parS[(st.parset+modelInfo$ncycle+1):((st.parset+2*modelInfo$ncycle))],
                             k=modelInfo$ncycle,
                             nperiod=modelInfo$nperiod)
        
        st.parset=posCalc(npos=4,nAssocSeries=modelInfo$nAssocSeries,ncycle=modelInfo$ncycle)          # start position in vector for this parameter set
        Hsd$D=harmonicFunc(x=seq(1,modelInfo$nperiod),
                           mean=parS[st.parset],
                           amp=parS[(st.parset+1):(st.parset+modelInfo$ncycle)],
                           phase.ang=parS[(st.parset+modelInfo$ncycle+1):((st.parset+2*modelInfo$ncycle))],
                           k=modelInfo$ncycle,
                           nperiod=modelInfo$nperiod)
      }
      
    }else{
      #NOT CONDITIONAL ON WET-DRY
      st.parset=posCalc(npos=1,nAssocSeries=modelInfo$nAssocSeries,ncycle=modelInfo$ncycle)     # start position in vector for this parameter set
      Hmean$WD=harmonicFunc(x=seq(1,modelInfo$nperiod),
                            mean=parS[st.parset],
                            amp=parS[(st.parset+1):(st.parset+modelInfo$ncycle)],
                            phase.ang=parS[(st.parset+modelInfo$ncycle+1):((st.parset+2*modelInfo$ncycle))],
                            k=modelInfo$ncycle,
                            nperiod=modelInfo$nperiod)
      
      st.parset=posCalc(npos=2,nAssocSeries=modelInfo$nAssocSeries,ncycle=modelInfo$ncycle)         # start position in vector for this parameter set
      Hsd$WD=harmonicFunc(x=seq(1,modelInfo$nperiod),
                          mean=parS[st.parset],
                          amp=parS[(st.parset+1):(st.parset+modelInfo$ncycle)],
                          phase.ang=parS[(st.parset+modelInfo$ncycle+1):((st.parset+2*modelInfo$ncycle))],
                          k=modelInfo$ncycle,
                          nperiod=modelInfo$nperiod)
    }
  }else{
    stop("Fixed parameter functionality yet to come")
  }
  
  
  #CONDITIONS WHERE SOME PARS ARE FIXED
  #FIXED SEASONALITY OR OCCURANCE OR ...
  #INITICALIBPARS
  #need sytem for each type of fixing
  # if modelInfo$fixedPars="phase.angle"...
  #if(!is.na(modelInfo$ncycle) & (modelInfo$fixedPars == "phase.angle")){}
  # if modelInfo$fixedPars=
  
  #out is to - CALCULATE PAR VECTORS (corr_1,corr_0,Hmean,Hsd)
  out=list(cor1=cor1,
           cor0=cor0,
           Hmean=Hmean,
           Hsd=Hsd
  )
  return(out)
  
}

#tester
#simHarTS.parmanager(parS=seq(1,13),modelTag="Temp-har26-wgen",modelInfo=modelInfo,initCalibPars=NULL)

#WHICHPARS
whichPars<-function(simVar=NULL,
                    modelInfo=NULL
){
  parLoc=list()
  pos.start=1
  for(i in 1:length(simVar)){
    dummyA=modelInfo[[i]]$npars
    pos.end=(dummyA-1) + pos.start
    tmp=c(pos.start,pos.end)
    parLoc[[i]]=tmp         # store in list
    pos.start=pos.end + 1     # update pos.start ready for next model
  }
  return(parLoc)
}




