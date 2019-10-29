exSpArgsVisual<-function(exSpArgs=NULL){
  
  modelTag="Simple-ann"
  attSel=names(exSpArgs$bounds)
  
  modelInfo=list()
  modelInfo[[modelTag[1]]]=get.model.info(modelTag[1])
  attInfo=attribute.info.check(attSel=attSel)                                 # vector of selected attributes (strings)
  if(modelTag[1] == "Simple-ann"){simVar=attInfo$varType}
  attInd=get.att.ind(attInfo=attInfo,simVar=simVar)
  attInfo=update.att.Info(attInfo=attInfo,attInd=attInd,modelTag=modelTag,simVar=simVar)
  
  targetMat=expSpaceSampler(type=exSpArgs$type,
                            samp=exSpArgs$samp,
                            bounds=exSpArgs$bounds,
                            varType=attInfo$varType,
                            targetType=attInfo$targetType,
                            attSel=attSel,
                            file)
  
  out<-expSpace2dViz(targetMat[,2],targetMat[,1],x.lab=names(exSpArgs$bounds)[2],y.lab=names(exSpArgs$bounds)[1])
  
}