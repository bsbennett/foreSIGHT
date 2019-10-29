#attribute calculator#


attributeCalculator<-function(obs=NULL,                    # scalar or vector of models to use
                              attSel=NULL,                   # vector of selected attributes
                              slice=NULL,                    #
                              window=10
                              
){
  IOmode="verbose"
  arrayID=NULL
  simLengthNyrs=NULL
  file<-filename(IOmode=IOmode,arrayID=arrayID)
  
  input<-input_check(obs,file,simLengthNyrs) #Checks for missing values/full years of data
  obs<-input$data
  
  if(!is.null(slice)){                       #For non historical records
    start=slice-window
    text<-paste("Note: Window is set ",window," years before slice",sep="")
    #    progress(text,file)
    obs<-obs[which(obs$year>=start&obs$year<=slice),]
  }
  
  #Get necessary variables for historical atts
  attInfo=attribute.info.check(attSel=attSel)              # vector of selected attributes (strings)
  simVar<-attInfo$varType
  simVar<-unique(simVar)
  nvar<-length(simVar)
  
  #  banner("INDEXING DATES",file)
  #  progress("Indexing dates...",file)                       # USE NEW APPENDED/CHECKED DATA
  yy=obs$year;mm=obs$month;dd=obs$day  
  # STORE DATE VECTORS
  
  datInd=list()
  datInd[["obs"]]=get.date.ind(dd=dd,mm=mm,yy=yy,nperiod=12,southHemi=TRUE)
  attInd=get.att.ind(attInfo=attInfo,simVar=simVar)
  
  #  progress("Dates indexed OK",file)
  
  #  banner("OBSERVED BASELINE ATTRIBUTE CALCULATION",file)
  #  progress("Calculating attributes...",file)
  
  attObs=list()                            #make this into its own function (also inserted into model sequencer)
  for(i in 1:nvar){
    attObs[[i]]=attribute.calculator(attSel=attSel[attInd[[i]]],
                                     data=obs[[simVar[i]]],
                                     datInd=datInd[["obs"]],
                                     attribute.funcs=attribute.funcs) 
  }
  
  #  progress("Attributes calculated OK",file)   #NEED SOME ACTUAL CHECKING HERE BEFORE PRONOUNCING OK
  attObs=unlist(attObs)
  names(attObs)=attSel
  #  progress(attObs)
  
  return(attObs)
}