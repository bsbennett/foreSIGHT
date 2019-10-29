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
      stop("Model combinations cannot perturb or hold selected attributes. Change attPerturb or attHold selection.")
    }
    
    progress("You have selected attributes for perturbation:",file)
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
         "Simple-ann" = {ValidAttributes=attributelist[c(1,8,37,42,43,112,113)]    # vector of inappropriate models to achieve attribute
         },
         "P-ann-wgen" = {ValidAttributes=attributelist[c(1:6,12:14,16:17,36,37,47)]    # vector of inappropriate models to achieve attribute
         },
         "P-seas-wgen" = {ValidAttributes=attributelist[c(1:7,12:14,16:25,28:41,47:48)]    # vector of inappropriate models to achieve attribute
         },
         "P-har26-wgen" =  {ValidAttributes=attributelist[c(1:7,12:25,28:41,47:48,51:62)]    # vector of inappropriate models to achieve attribute
         },
         "P-har12-wgen" =  {ValidAttributes=attributelist[c(1:7,12:25,28:41,47:48,51:62)]    # vector of inappropriate models to achieve attribute
         },
         "P-har12-wgen-FS" =  {ValidAttributes=attributelist[c(1:7,12:25,28:41,47:48,51:62)]    # vector of inappropriate models to achieve attribute
         },
         "P-har6-wgen" =  {ValidAttributes=attributelist[c(1:7,12:25,28:41,47:48,51:62)]    # vector of inappropriate models to achieve attribute
         },
         "P-2har26-wgen" =  {ValidAttributes=attributelist[c(1:7,12:25,28:41,47:48,51:62)]    # vector of inappropriate models to achieve attribute
         },
         "Temp-har26-wgen" =  {ValidAttributes=attributelist[c(8:11,26:27,45,63:78)]    # vector of inappropriate models to achieve attribute
         },
         "Temp-har26-wgen-wd" =  {ValidAttributes=attributelist[c(8:11,26:27,45,63:78)]    # vector of inappropriate models to achieve attribute
         },
         "Temp-har26-wgen-wdsd" =  {ValidAttributes=attributelist[c(8:11,26:27,45,63:78)]    # vector of inappropriate models to achieve attribute
         },
        "PET-har26-wgen" ={ValidAttributes=attributelist[c(42:44,46,49:50,79:111)]    # vector of inappropriate models to achieve attribute
         },
        "PET-har12-wgen" ={ValidAttributes=attributelist[c(42:44,46,49:50,79:111)]    # vector of inappropriate models to achieve attribute
        },
        "PET-har26-wgen-wd" ={ValidAttributes=attributelist[c(42:44,46,49:50,79:111)]    # vector of inappropriate models to achieve attribute
          },
        "Radn-har26-wgen"= {ValidAttributes=attributelist[c(112:149)] 
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
               "P-har12-wgen-FS",
               "P-har6-wgen",
               "P-2har26-wgen",
               "Temp-har26-wgen",
               "Temp-har26-wgen-wd",
               "Temp-har26-wgen-wdsd",
               "PET-har26-wgen",
               "PET-har12-wgen",
               "PET-har26-wgen-wd",
               "Radn-har26-wgen")

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
                "P_ann_seasRatio_m", #...48
                "PET_ann_P5_m",      #...49
                "PET_ann_P95_m",     #...50
                "P_Jan_tot_m",       #...51
                "P_Feb_tot_m",       #...52
                "P_Mar_tot_m",       #...53
                "P_Apr_tot_m",       #...54
                "P_May_tot_m",       #...55
                "P_Jun_tot_m",       #...56
                "P_Jul_tot_m",       #...57
                "P_Aug_tot_m",       #...58
                "P_Sep_tot_m",       #...59
                "P_Oct_tot_m",       #...60
                "P_Nov_tot_m",       #...61
                "P_Dec_tot_m",       #...62
                "Temp_JJA_avg_m",    #...63
                "Temp_MAM_avg_m",    #...64
                "Temp_DJF_avg_m",    #...65
                "Temp_SON_avg_m",    #...66
                "Temp_Jan_avg_m",    #...67
                "Temp_Feb_avg_m",    #...68
                "Temp_Mar_avg_m",    #...69
                "Temp_Apr_avg_m",    #...70
                "Temp_May_avg_m",    #...71
                "Temp_Jun_avg_m",    #...72
                "Temp_Jul_avg_m",    #...73
                "Temp_Aug_avg_m",    #...74
                "Temp_Sep_avg_m",    #...75
                "Temp_Oct_avg_m",    #...76
                "Temp_Nov_avg_m",    #...77
                "Temp_Dec_avg_m",    #...78
                "PET_JJA_avg_m",     #...79
                "PET_MAM_avg_m",     #...80
                "PET_DJF_avg_m",     #...81
                "PET_SON_avg_m",     #...82
                "PET_JJA_tot_m",     #...83
                "PET_MAM_tot_m",     #...84
                "PET_DJF_tot_m",     #...85
                "PET_SON_tot_m",     #...86
                "PET_Jan_tot_m",     #...87
                "PET_Feb_tot_m",     #...88
                "PET_Mar_tot_m",     #...89
                "PET_Apr_tot_m",     #...90
                "PET_May_tot_m",     #...91
                "PET_Jun_tot_m",     #...92
                "PET_Jul_tot_m",     #...93
                "PET_Aug_tot_m",     #...94
                "PET_Sep_tot_m",     #...95
                "PET_Oct_tot_m",     #...96
                "PET_Nov_tot_m",     #...97
                "PET_Dec_tot_m",     #...98
                "PET_Jan_avg_m",     #...99
                "PET_Feb_avg_m",     #...100
                "PET_Mar_avg_m",     #...101
                "PET_Apr_avg_m",     #...102
                "PET_May_avg_m",     #...103
                "PET_Jun_avg_m",     #...104
                "PET_Jul_avg_m",     #...105
                "PET_Aug_avg_m",     #...106
                "PET_Sep_avg_m",     #...107
                "PET_Oct_avg_m",     #...108
                "PET_Nov_avg_m",     #...109
                "PET_Dec_avg_m",     #...110
                "PET_ann_seasRatio_m",      #...111
                "Radn_ann_avg_m",    #...112
                "Radn_ann_tot_m",    #...113
                "Radn_ann_rng_m",    #...114
                "Radn_ann_P5_m",     #...115
                "Radn_ann_P95_m",    #...116
                "Radn_JJA_avg_m",    #...117
                "Radn_MAM_avg_m",    #...118
                "Radn_DJF_avg_m",    #...119
                "Radn_SON_avg_m",    #...120
                "Radn_JJA_tot_m",    #...121
                "Radn_MAM_tot_m",    #...122
                "Radn_DJF_tot_m",    #...123
                "Radn_SON_tot_m",    #...124
                "Radn_Jan_tot_m",    #...125
                "Radn_Feb_tot_m",    #...126
                "Radn_Mar_tot_m",    #...127
                "Radn_Apr_tot_m",    #...128
                "Radn_May_tot_m",    #...129
                "Radn_Jun_tot_m",    #...130
                "Radn_Jul_tot_m",    #...131
                "Radn_Aug_tot_m",    #...132
                "Radn_Sep_tot_m",    #...133
                "Radn_Oct_tot_m",    #...134
                "Radn_Nov_tot_m",    #...135
                "Radn_Dec_tot_m",    #...136
                "Radn_Jan_avg_m",    #...137
                "Radn_Feb_avg_m",    #...138
                "Radn_Mar_avg_m",    #...139
                "Radn_Apr_avg_m",    #...140
                "Radn_May_avg_m",    #...141
                "Radn_Jun_avg_m",    #...142
                "Radn_Jul_avg_m",    #...143
                "Radn_Aug_avg_m",    #...144
                "Radn_Sep_avg_m",    #...145
                "Radn_Oct_avg_m",    #...146
                "Radn_Nov_avg_m",    #...147
                "Radn_Dec_avg_m",    #...148
                "Radn_ann_seasRatio_m"    #...149
                )
