# @title
#FullLCreatPlus
#
# @ description
#creat list and filter ;preparation for EIC
# @para
#InL=1000 <intensity low limited>
#StT=0 <start time>
#EnT  <end time>
#DPa <data path>
#DNa/DNalis  <data name>/<data name list>
#LNa <list name>
#msn <MSLevel>
#
#@return
#LNa as list
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#,backend = "Ramp"
FullLCreat <- function(DNalis=NULL,InL,MSn=1) {
  require(mzR);
  if (missing(DNalis)) {DNalis<-list.files(getwd(),pattern="mzXML")} else {}
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


    LNa<-LNa[rtna$seqNum]


    LNaa<-list()
    for (iii in 1:length(LNa)){nu<-c(1:nrow(LNa[[iii]]));LNaa[[iii]]<-data.frame(LNa[[iii]][,1],LNa[[iii]][,2],rtna$retentionTime[iii],nu)}
    LNa<-LNaa
    names(LNa)<-rtna$seqNum
    #ScNu<-as.numeric(length(LNa))
    #if (missing(InL)) {} else {
    #n=0;detL=0;oriL<-length(LNa);for (i in 1 : length(LNa)) {LNa[[i-detL]][,2][LNa[[i-detL]][,2]<InL]<-NA;detL<-oriL-length(LNa)}
    #n=0;detL=0;oriL<-length(LNa);for (i in 1 : length(LNa)) {LNa[[i-detL]]<-na.omit(LNa[[i-detL]]);detL<-oriL-length(LNa)}
    #}
    mzOb[[jj]]<-LNa
  }
  names(mzOb)<-DNalis
  save(mzOb,file = "mzOb.RDa")
  return()
}
