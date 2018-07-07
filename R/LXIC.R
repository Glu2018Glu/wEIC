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

LXIC <- function(MZ,ppm=10) {
  relist<-list()
  plolist<-list()
  RAWlis<-load("mzOb.RDa")
  #if (MZ==NULL) { MZ<-read.table("clipboard",header=F,sep=",")}
  #MZ<-read.table("clipboard",header=F,sep=",")
  Tolist<-matrix(nrow=length(MZ)*length(RAWlis),ncol = 4)
  for (jk in 1:length(RAWlis)) {
    RAWDATA<-RAWlis[[jk]]
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
RT<-signif(as.double(names(RDATA)),digits=2)
  for (i in 1 : length(RDATA)) {mz[i]<-RDATA[[i]][1]}
  for (i in 1 : length(RDATA)) {Intensity[i]<-RDATA[[i]][2]}

  RDATA<-data.frame(RT,mz,Intensity)
 # relist[[kk]]<-RDATA


  TopRDATA<-RDATA[order(-RDATA$Intensity),]

  Tolist[kk+(jk-1)*3,1]<- TopRDATA[1,1]
  Tolist[kk+(jk-1)*3,2]<-round(TopRDATA[1,2],digits = 4)
  Tolist[kk+(jk-1)*3,3]<-format(TopRDATA[1,3],scientific = TRUE,digits = 2)
  Tolist[kk+(jk-1)*3,4]<-names(RAWlis[jk])
  }


  }
  #plot(RDATA[,1],RDATA[,3],type="b",)
  return(Tolist)


}
