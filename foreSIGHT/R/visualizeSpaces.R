# ###DRAFT DRAFT####
# 
# 
# 
# 
# #########
# # plotArgs=list(yArg="P_ann_tot_m",
# #               xArg="P_an_seasRatio_m"
# #   
# # )
# 
# 
# visualizeSpaces<-function(data=NULL,
#                           plotTag=NULL,                   # can be OAT, Binary, Contours, Heat, more to come
#                           plotArgs=NULL,                  # some plots will need levels specified, also titles, axis labels, etc.
#                           topSeed=NULL,
#                           xSeed=NULL,
# ){
#   
#   #set-up coords
#   attPlot=data$request[c(1:nTarget),attPerturb]  #grab coords
#   
#   #pack fitnesses
#   nCol=dim(data$fitness)[2]; if(is.null(nCol)){nCol=1}
#   fitPack=array(NA,dim=c(nTarget,nCol,nSeed))
#   #pack performance
#   nCol=dim(data$performance)[2]; if(is.null(nCol)){nCol=1}
#   perfPack=array(NA,dim=c(nTarget,nCol,nSeed))
#   
#   for(i in 1:nSeed){
#     fitPack[,,i]=data$fitness[((i-1)*nTarget+1):(i*nTarget),]
#     perfPack[,,i]=data$performance[((i-1)*nTarget+1):(i*nTarget),]
#   }
#   
#  
#  #sort if required (just sortig based on rainfall fitness)
#  if(!is.null(topSeed)){
#    perfSort=matrix(0,nrow=data$nTarget,ncol=nSeed)
#    for(i in 1:data$nTarget){
#      ind=order(fitPack[i,1,],decreasing=TRUE)  #make the best performance 1st 
#      perfSort[i,]=perfPack[i,1,ind]
#    }
#  }
#   
#   perfAv=rowMeans(perfSort)
#   
# 
#   
#   # if(data$nSeed>1){
#   #   if(is.null(topSeed)){
#   #     cat('nSeed >1 (and no further instructions are provided) the averaged space will be plotted')
#   #     #manage data function
#   #     #unique(data$target[,'seed'])
#   #     #need a split seeds function
#   #     
#   #     
#   #   }else{
#   #     #cat('nSeed >1 (and no further instructions are provided) the averaged space will be plotted')
#   #     #average the top bunch here
#   #   }
#     
#   }
#   
# 
#   
#   # #stuff inside
#   # if(plotTag=='Heat'){
#   #   
#   # }
#   
#   #if it's oat the exSpArgs must also say 'OAT' otherwise... wont work
#   
#   
# }
# 
# 
# 
