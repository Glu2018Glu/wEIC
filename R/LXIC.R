# @title
#LXIC
#
# @ description
#XIC from LCreat Data
# @para
#DATA <subject list from LCeat>
#MZ <target m/z>
#ppm <ppmolution>
#LNa<-RDATA

#Dfa as data.frame
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

LXIC <- function(RAWlis,MZ,ppm=10) {

  relist<-list()
  plolist<-list()
  load("mzOb.RDa")
  if (missing(RAWlis)) {RAWlis<-mzOb} else {}
  if (missing(MZ)) {MZK<-read.table("clipboard",header=F,sep=",")
  MZ<-c()
  for (ii in 1:length(MZK$V1)) {MZ[ii]<-as.double(MZK$V1[ii])}
  } else {}


  Tolist<-matrix(nrow=length(MZ)*length(RAWlis),ncol = 4)
  colnames(Tolist)<-c("Retention time","Mearsure M/z","Intensity","Sample Name")
  for (jk in 1:length(RAWlis)) {
    RAWDATA<-RAWlis[[jk]]
    nc<-nchar(names(RAWlis[jk]))
    if (dir.exists(substring(names(RAWlis[jk]),1,nc-6))) {} else
    {dir.create(substring(names(RAWlis[jk]),1,nc-6))}
    TopRDATA<-c()
    for (kk in 1 : length(MZ)) {
      Mzz<-as.numeric(MZ[kk])
      MZMAX<-as.double(Mzz+Mzz*ppm/1000000)
      MZMIN<-as.double(Mzz-Mzz*ppm/1000000)
      RDATA<-RAWDATA
      for (i in 1:length(RDATA)) RDATA[[i]][,1][RDATA[[i]][,1]>MZMAX|RDATA[[i]][,1]<MZMIN]<-NA
      for (i in 1:length(RDATA)) RDATA[[i]]<-na.omit(RDATA[[i]])
      n=0;detL=0;oriL<-length(RDATA);for (i in 1 : length(RDATA)) {if (length(RDATA[[i-detL]])<1) {RDATA[[i-detL]]<-NULL;detL<-detL+1}}
      mz<-c()
      Intensity<-c()
      RT<-as.double(names(RDATA))
      for (i in 1 : length(RDATA)) {mz[i]<-RDATA[[i]][1]}
      for (i in 1 : length(RDATA)) {Intensity[i]<-RDATA[[i]][2]}

      RDATA<-data.frame(RT,mz,Intensity)
      relist[[kk]]<-RDATA


      TopRDATA<-RDATA[order(-RDATA$Intensity),]

      Tolist[kk+(jk-1)*length(MZ),1]<- round(TopRDATA[1,1],digits=1)
      Tolist[kk+(jk-1)*length(MZ),2]<-round(TopRDATA[1,2],digits = 4)
      Tolist[kk+(jk-1)*length(MZ),3]<-format(TopRDATA[1,3],scientific = TRUE,digits = 2)
      Tolist[kk+(jk-1)*length(MZ),4]<-names(RAWlis[jk])
      wkwd<-getwd()
      setwd(substring(names(RAWlis[jk]),1,nc-6))
      jpeg(file=paste(MZ[kk],".jpeg",seq=""),width=1200,height=1000,res=56*2)
      plot(RDATA$RT,RDATA$Intensity,type="b",)
      title=paste("EIC of ",MZ[kk],seq="")
      text(TopRDATA[1,1],TopRDATA[1,3],paste("RT= ",round(TopRDATA[1,1],digits=1),seq=""),pos=2,offset=3)
      text(TopRDATA[1,1],TopRDATA[1,3],paste("Measured m/z= ",round(TopRDATA[1,2],digits = 4),seq=""),pos=4,offset=3)
      dev.off()
      setwd(wkwd)

    }


  }

  TTo<-paste(Tolist[,2],Tolist[,1],Tolist[,3],Tolist[,4],sep = "\t")
  write.csv(Tolist,"temp.csv" )
  #return(writeClipboard(Tolist[,2]))
  return(writeClipboard(TTo))

}
