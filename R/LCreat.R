# @title
#LCreat
#
# @ description
#creat list and filter ;preparation for EIC
# @para
#InL=1000 <intensity low limited>
#StT=0 <start time>
#EnT  <end time>
#DPa <data path>
#DNa <data name>
#LNa <list name>
#msn <MSLevel>
#@return
#LNa as list
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#,backend = "Ramp"
LCreat <- function(DNalis,InL=1000,StT=0,EnT=1000000000000,MSn=1) {
  require(mzR);require(msdata);
  mzOb<-list()
  DPa<-getwd()

  for (jj in 1 : length(DNalis)) {
    DNa<-DNalis[jj]
  file<-list.files(DPa,pattern =DNa,full.names = TRUE,recursive =FALSE);
  aa<-openMSfile(file);
  LNa<-peaks(aa);
  rtna<-header(aa);
  rtna$msLevel[rtna$msLevel!=MSn]<-NA
  rtna<-rtna[-23]
  rtna<-na.omit(rtna)
  rtna$retentionTime<-rtna$retentionTime/60
  rtna$retentionTime[rtna$retentionTime<StT]<-NA
  rtna$retentionTime[rtna$retentionTime>EnT]<-NA
  rtna<-na.omit(rtna)
  LNa<-LNa[rtna$seqNum]
  names(LNa)<-rtna$retentionTime
  ScNu<-as.numeric(length(LNa))
  n=0;detL=0;oriL<-length(LNa);for (i in 1 : length(LNa)) {LNa[[i-detL]][,2][LNa[[i-detL]][,2]<InL]<-NA;detL<-oriL-length(LNa)}
  n=0;detL=0;oriL<-length(LNa);for (i in 1 : length(LNa)) {LNa[[i-detL]]<-na.omit(LNa[[i-detL]]);detL<-oriL-length(LNa)}
  mzOb[[jj]]<-LNa
  }
  names(mzOb)<-DNalis
  save(mzOb,file = "mzOb.RDa")
  return(0)
}
