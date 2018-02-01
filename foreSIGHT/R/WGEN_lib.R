#######################################
##     WGEN FUNCTION LIBRARY         ##
#######################################

# CONTAINS

  # switch_simulator()
  #----------------------------
  # P_WGEN_master()
    # P_WGEN_generic() - simulates rainfall using Richardson type model
        #Pstatus_WGEN_generic()
        #Pamount_WGEN_generic()
  #----------------------------
  # TS_WGEN_master()
    # TS_WGEN_generic() -
      # calcDaySeries()
        # calcDayFunc()
      # residualGenerator()
      #****NOT DONE YET Also other correlated series models (via residuals etc) ---

#FUNCTIONS
#-----------------------------------------------------------------------------------------------------------
switch_simulator<-function(type=NULL,          # what vartype is being simulated
                           parS=NULL,
                           modelTag=NULL,      
                           modelInfo=NULL,
                           datInd=NULL,        
                           initCalibPars=NULL,
                           wdSeries=NULL,      
                           resid_ts=NULL,
                           seed=NULL
                           ){
  
  
  switch(type,
         "P" = { P_WGEN_master(parS=parS,              #RAIN SELECTED
                               modelTag=modelTag,      
                               modelInfo=modelInfo,
                               datInd=datInd,        
                               initCalibPars=initCalibPars, 
                               Nw=200,   
                               #seeds
                               N=seed)
               },
         "Temp" = { TS_WGEN_master(parS=parS,         
                                   modelTag=modelTag,      
                                   modelInfo=modelInfo,
                                   datInd=datInd,        
                                   initCalibPars=initCalibPars, 
                                   wdSeries=wdSeries,      
                                   resid_ts=resid_ts,
                                   seed=seed)
           
                   },
         "PET" = { TS_WGEN_master(parS=parS,         
                                  modelTag=modelTag,      
                                  modelInfo=modelInfo,
                                  datInd=datInd,        
                                  initCalibPars=initCalibPars, 
                                  wdSeries=wdSeries,      
                                  resid_ts=resid_ts,
                                  seed=seed)
           
                 },
         
         # "RH" = { TS_WGEN_master(parS=parS,         
         #                         modelTag=modelTag,      
         #                         modelInfo=modelInfo,
         #                         datInd=datInd,        
         #                         initCalibPars=initCalibPars, 
         #                         wdSeries=wdSeries,      
         #                         resid_ts=resid_ts,
         #                         seed=seed)
         #   
         #         },
         -99.00
  )
}
#tester
# mod=2
# testpar=c(0.64,16.8,7.3,0.4,3.4,1.1,0.17)
# out=switch_simulator(type=modelInfo[[modelTag[mod]]]$simVar,parS=testpar,
#                     modelTag=modelTag[mod],modelInfo=modelInfo[[modelTag[mod]]],datInd=datInd[[modelTag[mod]]],
#                     initCalibPars=NULL,wdSeries=NULL,resid_ts=NULL,seed=1234)
# plot(out$sim)

P_WGEN_master<- function(parS,               # vector of pars (will change in optim)
                         modelTag=NULL,      # tag to link/identify model
                         modelInfo=NULL,
                         datInd=NULL,        # dat ind
                         initCalibPars=NULL, # vector of pars from initial baseline calibration
                         Nw=NULL,            # warmup period in days
                         N=NULL             # seeds
){
  #Converts supplied pars into required format (e.g. if harmonic applied)
  par=par.manager(parS=parS,modelInfo=modelInfo,modelTag=modelTag,initCalibPars=initCalibPars)
  
  #SIMULATE RAINFALL TIME SERIES
  sim=P_WGEN_generic(parPwd=par$pwd,
                     parPdd=par$pdd,
                     parAlpha=par$alpha,
                     parBeta=par$beta,
                     nperiod=modelInfo$nperiod,
                     i.pp=datInd$i.mod,   #
                     ndays=length(unlist(datInd$i.mod)), #
                     Nw=Nw,
                     N=N)
  
  #REMOVE 1 YEAR WARM-UP PERIOD supplied by i.mod
  nLast=length(sim$sim); sim$sim=sim$sim[(nLast-datInd$ndays+1):nLast]
  
  return(sim)  #return simulated rainfall
}

P_WGEN_generic<-function(parPwd=NULL,    # vector of pars for pwd (length = nperiod)
                         parPdd=NULL,    # vector of pars for pdd (length = nperiod)
                         parAlpha=NULL,  # vector of pars for alpha (length = nperiod)
                         parBeta=NULL,   # vector of pars for beta (length = nperiod)
                         
                         nperiod=NULL,   # no. of periods over the year
                         i.pp=NULL,      # indices for each period (given nperiod) - from get.period.ind() or i.ss etc
                         ndays=NULL,     # total no. sim days/length(obs)
                         
                         Nw=NULL,    #warm-up period
                         N=NULL     #seed
){
  #sim occurence
  simS=Pstatus_WGEN_generic(parPwd=parPwd,    # vector of pars for pwd (length = nperiod)
                            parPdd=parPdd,    # vector of pars for pdd (length = nperiod)
                            nperiod=nperiod,  # no. of periods over the year
                            i.pp=i.pp,        # indices for each period (given nperiod) - from get.period.ind() or i.ss etc
                            ndays=ndays,      # total no. sim days/length(obs)    
                            Nw=Nw,            # warmup period length in days
                            N=N               # another seed (POSSIBLE REDUNDANT)
  )
  
  #sim amounts
  simP=Pamount_WGEN_generic(parAlpha=parAlpha,         # vector of pars for alpha (length = nperiod)
                            parBeta=parBeta,           # vector of pars for beta (length = nperiod)
                            nperiod=nperiod,           # no. of periods over the year
                            i.pp=i.pp,                 # indices for each period (given nperiod) - from get.period.ind() or i.ss etc
                            ndays=ndays,               # total no. sim days/length(obs)   
                            status_ts=simS$status_ts,  # TS vector of wet/dry statuses-obtained from the output of 'wvar_gen_Pstatus'
                            N=N                        # random seeds
                            #seed3=seed3               # random seeds
  )
  
  
  syntP <- list(sim=simP$sim,
                seed=N)
  return(syntP)
}

Pstatus_WGEN_generic <- function(parPwd=NULL,    # vector of pars for pwd (length = nperiod)
                                 parPdd=NULL,    # vector of pars for pdd (length = nperiod)
                                 nperiod=NULL,   # no. of periods over the year
                                 i.pp=NULL,      # indices for each period (given nperiod) - from get.period.ind() or i.ss etc
                                 ndays=NULL,     # total no. sim days/length(obs)    
                                 Nw,             # warmup period length in days
                                 N              # another seed (POSSIBLE REDUNDANT)
                                 # seed1,          # seed to generate first day of each period
                                 # seed2           # seed to generate occurance series                                        
  ){
  
  set.seed(N)                                    # seed seed to fix input  
  drywet_period <- vector(mode="list", nperiod)  # used to save wet/dry status for all days in each period
  
  # Markov chain - rain occurance
 # .Random.seed <- seed1                 # set the 1st seed to fix input of start days
  fday=sample(0:1,(nperiod+Nw),replace=TRUE) # occurance status of first day for each period is sampled randomly (but controlled by seed1)
  fday=fday[-(1:Nw)]
  #.Random.seed <- seed2                 # set the second seed
  RN <- runif((ndays+nperiod*Nw),0,1)   # generate a vector of RN with the length of the entire time-series including warm up for each period
  # distribute the RN vector to each period
  for(i in 1:nperiod) {
    drywet_period[[i]] <- matrix(NA,nrow=(length(i.pp[[i]])+Nw),ncol=1)  # making matrix/vector for each period
    drywet_period[[i]][1]=fday[i]                                        # loading first day into series
    } 
  
  #FIRST ORDER MARKOV CHAIN FOR RAINFALL OCCURANCE
  counter=0                                 # initialise counter
  for (i in 1:nperiod) {                    # Loop over periods
    counter=counter+1                       # step to first day of period
    for (j in 2:(length(i.pp[[i]])+Nw)) {   # for number of days in the period + warmup 
      counter=counter+1                     # step through days
      if (drywet_period[[i]][j-1] == 0) {
        if (RN[counter-1] <= parPdd[i]) {   # examine prior day
          drywet_period[[i]][j] <- 0
        } else {
          drywet_period[[i]][j] <- 1
        }
      }  else if (drywet_period[[i]][j-1] == 1) {
        if (RN[counter-1] <= parPwd[i]) {
          drywet_period[[i]][j] <- 0
        } else {
          drywet_period[[i]][j] <- 1
        }
      }                                     # end else if
    }                                       # loop over days in period
    drywet_period[[i]] <- drywet_period[[i]][-(1:Nw)]  # chop off warmup period 
  }                                         # end period loop
  
  #STICK SERIES BACK TOGETHER AS A CONTINUOUS TIME SERIES USING INDICES
  drywet_TS=rep(NA,ndays)                      # create blank TS
  for(i in 1:nperiod){
    drywet_TS[i.pp[[i]]]=drywet_period[[i]]    # take drywet status for each period and slot into status TS
  }
  
  #LIST TO RETURN
  syntstatus <- list( #  status_s=drywet_period, # dry/wet status arranged by period
                     status_ts=drywet_TS,    # dry/wet TS                     
                     # seed1=seed1,            # record seed for sampling start days
                     # seed2=seed2,            # record seed to generate occurance series
                     seed=N                # seed used to start function (POSSIBLY REDUNDANT)
                     )
  return(syntstatus)
}

Pamount_WGEN_generic <- function(parAlpha=NULL,         # vector of pars for alpha (length = nperiod)
                                 parBeta=NULL,          # vector of pars for beta (length = nperiod)
                                 nperiod=NULL,          # no. of periods over the year
                                 i.pp=NULL,             # indices for each period (given nperiod) - from get.period.ind() or i.ss etc
                                 ndays=NULL,            # total no. sim days/length(obs)   
                                 status_ts,             # TS vector of wet/dry statuses-obtained from the output of 'wvar_gen_Pstatus'
                                 N                     # random seeds
                                 #seed3                  # random seeds
){ 
  
  #GENERATE RAIN ON A PERIOD BASIS
  Nw=50                                               #warmup
  rain=rep(0,ndays)                                   # make a rainfall TS of 0s
  for (i in 1:nperiod) {
    set.seed(N)                                       # set seeds
   # .Random.seed <- seed3                            # same seed is being used for each wet day generation call
    wet.days=which(status_ts[i.pp[[i]]]==1)           
    ind=i.pp[[i]][wet.days]                           # index of where to put the wet days into the TS vector
    nwet=length(ind)                                  # get no. of wet days to sim 
    options(warn=-1)
    allwet <- rgamma((nwet+Nw),
                     shape=parAlpha[i],
                     scale=parBeta[i])                # generate rainfall intensity for all wet days in each season 
    allwet=allwet[-(1:Nw)]
    options(warn=0)
    rain[ind]=allwet                                  # distribute wet-day amounts for season 's' into rainfall timeseries
  }                                                   # loop over seasons
  
  #this return list should be adjustable
  syntP <- list(sim=rain,  #
                # seed3=seed3,      # return seeds
                seed=N)       
  return(syntP)
}

# TEST CASE - seasonal with all pars the same
# pdd=0.8152509;pwd=0.3680295;alpha=0.6341648;beta=0.06911646
# nperiod=4; i.ss=datInd$i.ss ; nperiod=4
# 
# sim=P_WGEN_generic(parPwd=rep(pwd,nperiod),
#                parPdd=rep(pdd,nperiod),
#                parAlpha=rep(alpha,nperiod),
#                parBeta=rep(beta,nperiod),
#                nperiod=nperiod,
#                i.pp=i.ss,
#                ndays=datInd$ndays,
#                Nw=100,
#                N=122,  
#                seed1=122,
#                seed2=122, 
#                seed3=122                      
# )
# windows() 
# y.range=c(0,60)
# plot(x=seq(1,datInd$ndays),y=sim$rain_ts,type="l", xlab="days", ylab="Rain (mm)", main="Sim - test",ylim=y.range,xaxs="i")
#---------------------------------------------------------------------------------------------------------------


# sim=P_WGEN_generic(parPwd=pwd,
#                parPdd=pdd,
#                parAlpha=alpha,
#                parBeta=beta,
#                nperiod=nperiod,
#                i.pp=i.ss,
#                ndays=datInd$ndays,
#                Nw=100,
#                N=122,
#                seed1=122,
#                seed2=122,
#                seed3=122
# )
# windows()
# y.range=c(0,60)
# plot(x=seq(1,datInd$ndays),y=sim$rain_ts,type="l", xlab="days", ylab="Rain (mm)", main="Sim - test",ylim=y.range,xaxs="i")
# 
# max(data);max(sim$rain_ts);sum(sim$rain_ts[i.ss[[2]]]);sum(data[i.ss[[2]]])

TS_WGEN_master<- function(parS=NULL,         # vector of pars (will change in optim)
                          modelTag=NULL,      # tag to link/identify model
                          modelInfo=NULL,
                          datInd=NULL,        # dat ind
                          initCalibPars=NULL, # vector of pars from initial baseline calibration
                          wdSeries=NULL,      # rain  series
                          resid_ts=NULL,
                          seed=NULL
){
  #Converts supplied pars into required format (e.g. if harmonic applied)
  par=simHarTS.parmanager(parS=parS,modelTag=modelTag,modelInfo=modelInfo,initCalibPars=initCalibPars)
  
  #SIMULATE REQUIRED TIMESERIES
  sim=TS_WGEN_generic(parCor0=par$cor0,         # correl pars
                      parCor1=par$cor1, 
                      parHmean=par$Hmean,       # mean harmonic pars (for $WD or $W & $D)
                      parHsd=par$Hsd,           # sd harmonic pars (for $WD or $W & $D)
                      k=modelInfo$ncycle,
                      nperiod=modelInfo$nperiod,
                      ndays=datInd$ndays,
                      nAssocSeries=modelInfo$nAssocSeries,
                      i.pp=datInd$i.pp,
                      initCalibPars=initCalibPars,  # initial model calibration pars
                      WDcondition=modelInfo$WDcondition,   # generate ts conditional on wet-dry series
                      wdSeries=wdSeries,        # wet-dry series
                      resid_ts=resid_ts,        # leave capacity to generate or receive residuals
                      seed=seed                 # seed for residuals generation
  )
  
  return(sim)
}
#TESTER
# out=TS_WGEN_master(parS=seq(0.001,0.13,0.01),modelTag=modelTag,modelInfo=modelInfo, 
#                    datInd=datInd,initCalibPars=NULL, 
#                    wdSeries=obsP,resid_ts=NULL,seed=1234)

# testpar=c(0.64,16.8,7.3,0.4,3.4,1.1,0.17)
# out=TS_WGEN_master(parS=testpar,modelTag=modelTag[mod],modelInfo=modelInfo[[modelTag[mod]]],
#                    datInd=datInd[[modelTag[mod]]],initCalibPars=NULL,
#                    wdSeries=NULL,resid_ts=NULL,seed=1234)
#plot(out$sim)

TS_WGEN_generic<-function(parCor0=NULL,         # correl pars
                          parCor1=NULL,
                          parHmean=NULL,      # mean harmonic pars
                          parHsd=NULL,        # sd harmonic pars
                          k=NULL,
                          nperiod=NULL,
                          ndays=NULL,
                          nAssocSeries=NULL,
                          i.pp=NULL,
                          initCalibPars=NULL,  # initial model calibration pars
                          WDcondition=FALSE,   # generate ts conditional on wet-dry series
                          wdSeries=NULL,       # wet-dry series
                          resid_ts=NULL,       # leave capacity to generate or receive residuals
                          seed=NULL            # seed for residuals generation
){
  
  #generate residual series if none supplied
  if(is.null(resid_ts)){
    resid_ts=residualGenerator(parCor0=parCor0,
                               parCor1=parCor1,
                               ndays=ndays,
                               nAssocSeries=nAssocSeries,
                               seed=seed
    )
  }
  
  #divy up parameters and simulate
  sim=calcDaySeries(Hpar_m=parHmean,       #
                    Hpar_sd=parHsd,     
                    k=k,           
                    nperiod=nperiod,     
                    ndays=ndays,  
                    resid_ts=resid_ts,
                    i.pp=i.pp,         
                    WDcondition=WDcondition, 
                    wdSeries=wdSeries     
  )
  out=list(sim=sim,seed=seed)
  return(out)
}
#TESTER
# tmp=TS_WGEN_generic(parCor0=par$cor0, parCor1=par$cor1,parHmean=par$Hmean,parHsd=par$Hsd,k=modelInfo$ncycle,
#                   nperiod=modelInfo$nperiod,ndays=datInd$ndays, nAssocSeries=0,i.pp=datInd$i.pp,
#                   initCalibPars=NULL,WDcondition=TRUE,wdSeries=obsP,resid_ts=NULL,seed=1234)

#Generate the daily series
calcDaySeries<-function(Hpar_m=NULL,       #harmonic pars for means of length nperiod 
                        Hpar_sd=NULL,      #harmonic pars for std dev 
                        k=NULL,            #No. cycles in model specified
                        nperiod=NULL,      #No. period require by model specified
                        ndays=NULL,        # No. of days simulated
                        resid_ts=NULL,     #residual error timeseries
                        i.pp=NULL,         #period  indices
                        WDcondition=FALSE, #generate ts conditional on wet-dry series
                        wdSeries=NULL      #wetdry series
){
  
  genTS=rep(NA,ndays)           #MAKE BLANK VECTOR FOR TS
  if(WDcondition==FALSE){       #IF NO WET-DRY CONDITION
    #CALCULATE VALUE BASED ON PERIOD PARS & STUFF BACK INTO TS AT CORRECT POINT
    for(p in 1:nperiod){
      genTS[i.pp[[p]]]=calcDayFunc(mean=Hpar_m$WD[p],sd=Hpar_sd$WD[p],err=resid_ts[i.pp[[p]]])
    }
  }else{                        #IF WET-DRY CONDITIONAL
    #DETERMINE WET DAY INDICIES
    indW=which(wdSeries>0.01)
    #CALCULATE VALUE BASED ON PERIOD PARS & STUFF BACK INTO TS AT CORRECT POINT
    for(p in 1:nperiod){
      indWP=intersect(i.pp[[p]],indW)
      genTS[indWP]=calcDayFunc(mean=Hpar_m$W[p],sd=Hpar_sd$W[p],err=resid_ts[indWP])  #Store values on wet days for period
      indDP=outersect(i.pp[[p]],indWP)
      genTS[indDP]=calcDayFunc(mean=Hpar_m$D[p],sd=Hpar_sd$D[p],err=resid_ts[indDP])  #Store values on dry days for period
    }
  }
  return(genTS)
}

#SIMULATE TEMPERATURE SERIES
calcDayFunc<-function(mean=NULL,
                      sd=NULL,
                      err=NULL
){
  val=mean+sd*err
}

#GENERATE RESIDUALS USING SUPLLIED PARAMETERS
residualGenerator<-function(parCor0=NULL,
                            parCor1=NULL,
                            ndays=NULL,
                            nAssocSeries=0,
                            seed=1234
){
  
  #GENERATE RANDOM UNIFORM NUMBERS
  set.seed(seed)
  
  RN_res<-matrix(runif(ndays*(1+nAssocSeries)),nrow=ndays,ncol=(1+nAssocSeries))
  res_gen<-matrix(NA,nrow=ndays,ncol=(1+nAssocSeries))
  
  if(nAssocSeries==0){  #if just one series
    A<- parCor1[1]
    B<-1
    res_gen[1,1]=B*RN_res[1,(1+nAssocSeries)]   #load day 1 (no residual error)
    
    for(i in 2:ndays){
      res_gen[i,1]<-A*res_gen[(i-1),(1+nAssocSeries)]+B*RN_res[i,(1+nAssocSeries)]
    }
    
  }else{
    #IMPLEMENT A/B MATRIX MATHS HERE
    #
    stop("functionality not yet implemented")
  }
  
  return(res_gen)
}

#TESTER
#residualGenerator(parCor0=1,parCor1=0.01,ndays=5113, nAssocSeries=0, seed=1234)
