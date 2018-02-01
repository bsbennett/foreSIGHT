#############################################
##  LOGIC CHECKS FOR ATTRIBUTE/MODEL TAGS  ##
#############################################

#CONTAINS
  # argument_logic_check()
    #checks for simple scaling attributes
    #checks for matching variable, attribute and model lists

argument_logic_check<-function(names=NULL,
                         attSel=NULL,        
                         attPrim=NULL,
                         modelTag=NULL,
                         file
){

  nam<-names[-c(1:3)]
  
  ###not temp wd if no rain
  modelVars<-sapply(modelTag,get.varType,USE.NAMES=FALSE,sep="-")
  
  if (sum("P" %in% modelVars)==0) {
    if(sum("Temp-har26-wgen-wd" %in% modelTag)==1) {  #add in same for PET
      logfile("Error: Cannot simulate stochastic wet/dry dependent temperature without a rainfall model",file)
      stop("Cannot simulate stochastic wet/dry dependent temperature without a rainfall model")
    } else if(sum("PET-har26-wgen-wd" %in% modelTag)==1) {
      logfile("Error: Cannot simulate stochastic wet/dry dependent PET without a rainfall model",file)
      stop("Cannot simulate stochastic wet/dry dependent PET without a rainfall model")
    }
    
  }

  if (modelTag[1]=="Simple-ann") {
    
  } else {
    
    validAtts=("temp")
    for(i in 1:length(modelVars)) {
      temp=get.attribute.info(modelTag=modelTag[i])
      validAtts=append(validAtts,temp)
    }
    validAtts=validAtts[-1]
    
    if(sum(attSel %in% validAtts)!=length(attSel)) {
      logfile("Error: Model combinations cannot perturb selected attributes",file)
      logfile("Program terminated",file)
      stop("Model combinations cannot perturb selected attributes")
    }
    
    progress("You have selected primary attributes:",file)
    cat("     ")
    cat(attPrim,sep=", ")
    cat("\n")
    cat("\n")
    logfile(attPrim,file)
    progress("These attributes will be perturbed with model types:",file)
    cat("     ")
    cat(modelTag,sep=", ")
    cat("\n")
    cat("\n")
    logfile(modelTag,file)
    progress("The scenarios will include the following attributes in the objective function:",file)
    cat("     ")
    cat(attSel,sep=", ")
    cat("\n")
    cat("\n")
    logfile(attSel,file)
  }
  
  #model assessor

  
}

get.attribute.info<-function(modelTag=NULL # attribute name
){
  #SET UP ATTRIBUTE RELATED PARAMETERS
  switch(modelTag,
         "Simple-ann" = {ValidAttributes=attributelist[c(1,8)]    # vector of inappropriate models to achieve attribute
         },
         "P-ann-wgen" = {ValidAttributes=attributelist[c(1:6,12:14,16:17,36,37,47)]    # vector of inappropriate models to achieve attribute
         },
         "P-seas-wgen" = {ValidAttributes=attributelist[c(1:7,12:14,16:25,28:41,47:48)]    # vector of inappropriate models to achieve attribute
         },
         "P-har26-wgen" =  {ValidAttributes=attributelist[c(1:7,12:25,28:41,47:48)]    # vector of inappropriate models to achieve attribute
         },
         "P-har12-wgen" =  {ValidAttributes=attributelist[c(1:7,12:25,28:41,47:48)]    # vector of inappropriate models to achieve attribute
         },
         "P-har6-wgen" =  {ValidAttributes=attributelist[c(1:7,12:25,28:41,47:48)]    # vector of inappropriate models to achieve attribute
         },
         "P-2har26-wgen" =  {ValidAttributes=attributelist[c(1:7,12:25,28:41,47:48)]    # vector of inappropriate models to achieve attribute
         },
         "Temp-har26-wgen" =  {ValidAttributes=attributelist[c(8:11,26:27,45)]    # vector of inappropriate models to achieve attribute
         },
         "Temp-har26-wgen-wd" =  {ValidAttributes=attributelist[c(8:11,26:27,45)]    # vector of inappropriate models to achieve attribute
         },
         "Temp-har26-wgen-wdsd" =  {ValidAttributes=attributelist[c(8:11,26:27,45)]    # vector of inappropriate models to achieve attribute
         },
        "PET-har26-wgen" ={ValidAttributes=attributelist[c(42:44,46)]    # vector of inappropriate models to achieve attribute
         },
        "PET-har12-wgen" ={ValidAttributes=attributelist[c(42:44,46)]    # vector of inappropriate models to achieve attribute
        },
        "PET-har26-wgen-wd" ={ValidAttributes=attributelist[c(42:44,46)]    # vector of inappropriate models to achieve attribute
          },
         #--- MORE COMING ---
         
         -999
  )
  return(ValidAttributes)
}

modelTaglist=c("Simple-ann",
               "P-ann-wgen",
               "P-seas-wgen",
               "P-har26-wgen",
               "P-har12-wgen",
               "P-har6-wgen",
               "P-2har26-wgen",
               "Temp-har26-wgen",
               "Temp-har26-wgen-wd",
               "Temp-har26-wgen-wdsd",
               "PET-har26-wgen",
               "PET-har12-wgen",
               "PET-har26-wgen-wd")

attributelist=c("P_ann_tot_m",      #...1                  
                "P_ann_R10_m",      #...2
                "P_ann_maxDSD_m",   #...3
                "P_ann_maxWSD_m",   #...4
                "P_ann_P99_m",      #...5
                "P_ann_dyWet99p_m", #...6
                "P_ann_ratioWS_m",  #...7
                "Temp_ann_avg_m",   #...8
                "Temp_ann_P5_m",    #...9
                "Temp_ann_P95_m",   #...10
                "Temp_ann_F0_m",    #...11
                "P_ann_dyWet_m",    #...12
                "P_ann_DSD_m",      #...13
                "P_seas_tot_cv",    #...14
                "P_mon_tot_cv",     #...15
                "P_ann_avgWSD_m",   #...16
                "P_ann_avgDSD_m",   #...17
                "P_JJA_avgWSD_m",   #...18
                "P_MAM_avgWSD_m",   #...19
                "P_DJF_avgWSD_m",   #...20
                "P_SON_avgWSD_m",   #...21
                "P_JJA_avgDSD_m",   #...22
                "P_MAM_avgDSD_m",   #...23
                "P_DJF_avgDSD_m",   #...24
                "P_SON_avgDSD_m",   #...25
                "Temp_ann_GSL_m",   #...26
                "Temp_ann_CSL_m",   #...27
                "P_JJA_dyWet_m",    #...28
                "P_MAM_dyWet_m",    #...29
                "P_DJF_dyWet_m",    #...30
                "P_SON_dyWet_m",    #...31
                "P_JJA_tot_m",      #...32
                "P_MAM_tot_m",      #...33
                "P_DJF_tot_m",      #...34
                "P_SON_tot_m",      #...35
                "P_ann_nWet_m",     #...36
                "P_ann_dyAll_m",    #...37
                "P_JJA_dyAll_m",    #...38
                "P_MAM_dyAll_m",    #...39
                "P_DJF_dyAll_m",    #...40
                "P_SON_dyAll_m",    #...41
                "PET_ann_avg_m",    #...42
                "PET_ann_tot_m",    #...43
                "PET_ann_rng_m",    #...44
                "Temp_ann_rng_m",   #...45
                "PET_ann_90pX_m",   #...46
                "P_ann_90X_m",      #...47
                "P_ann_seasRatio_m" #...48
                )
