# #----------------------------------------------------------------------------------------------------------------------------------------
# 
# performanceSpaces<-function(data=out,
#                             plotTag=NULL,                   # can be OAT, Binary, Contours, Heat, more to come
#                             plotArgs=NULL,                  # some plots will need levels specified, also titles, axis labels, etc.
#                             plotLayer=NULL,                 # can be used to pass in a data set for GCMs
#                             systemModel=NULL,
#                             systemArgs=NULL,
#                             climdata=NULL){
#   
#   #format of climdata
#   ## X axis #### Y axis #### RCM #### Performance ###
#   
#   simDat=data$data
#   nTarget=data$nTarget
#   attSel=data$attSel
#   attPrim=data$attPrim
#   targetMat=data$target
#   
#   plotArgsDefault<-list()
#   plotArgsDefault$title="Scenario Neutral Space"
#   plotArgsDefault$xlim=c(-2,2)
#   plotArgsDefault$ylim=c(0.9,1.1)
#   plotArgsDefault$performancelimits=NULL
#   
#   if(!is.null(plotArgs)){
#     plotArgs=modifyList(plotArgsDefault,plotArgs)
#   } else {
#     plotArgs=plotArgsDefault
#   }
#   
#   #Needs data set format
#   performance=rep(NA,nTarget)
#   for(i in 1:nTarget){
#     performance[i]=systemModel(data=simDat[[i]],systemArgs=systemArgs,seed=i)
#   }
#   
#   ###Create baseline plot
#   
#   if(plotTag=="Heat"){
#     if(modelTag[1]=="Simple-ann"){attPrim=attSel}
#     p1<-heatPlot(plotArgs=plotArgs,targetMat=targetMat, attSel=attSel,attPrim=attPrim,performance=performance,climdata=climdata)
#     
#     save_plot("HeatmapSpace.pdf",p1,base_aspect_ratio = 1,base_height = 7)
#     
#   } else if (plotTag=="OAT") {
#     # colnames(simTarget)<-attSel
#     p1<-oatplots(targetMat=targetMat,performance=performance,samp=data$exSpArgs$samp)
#     
#     save_plot("OATPlots.pdf",p1,base_width = 14,base_height = 7)
#     
#   } else if (plotTag=="Binary"){
#     
#     
#   } else if (plotTag=="Contours"){
#     if(modelTag[1]=="Simple-ann"){attPrim=attSel}
#     p1<-contourPlots(plotArgs=plotArgs,targetMat=targetMat, attSel=attSel,attPrim=attPrim,performance=performance,climdata=climdata)
#     
#     save_plot("ContourSpace.pdf",p1,base_aspect_ratio = 1,base_height = 7)
#   }
#   
#   
#   return(p1)
#   
# }
# 
