# @title
#LXICpus
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

RTlimited2 <- function(RAWlis,MZ,ppm=10,RTWin=1) {
  require(reshape2)
  require(plyr)
  relist<-list()
  plolist<-list()
  load("mzOb.RDa")
  options(stringsAsFactors = FALSE)
  if (missing(RAWlis)) {RAWlis<-mzOb} else {}
  if (missing(MZ))
  {MZK<-read.delim2("clipboard",header=F);MZ<-c();
  {if (length(MZK)==2)
  {if (length(unlist(strsplit(as.character(MZK$V1[1])[1],",")))==2)
  {
    MZ<-matrix(nrow=length(MZK$V1),ncol = 3)
    for (ii in 1:length(MZK$V1)) {lisi<-c();lisi<-unlist(strsplit(as.character(MZK$V1[ii])[1],","));MZ[ii,]<-c(lisi[1],lisi[2],as.double(MZK$V2[ii]))}
  }
    else
    { MZ<-matrix(nrow=length(MZK$V1),ncol = 2)
    for (ii in 1:length(MZK$V1)) {MZ[ii,]<-c(as.double(MZK$V1[ii]),as.double(MZK$V2[ii]))}
    }
  }
    else
    {if (length(unlist(strsplit(as.character(MZK$V1[1])[1],",")))==2)
    {
      MZ<-matrix(nrow=length(MZK$V1),ncol = 2)
      for (ii in 1:length(MZK$V1)) {lisi<-c();lisi<-unlist(strsplit(as.character(MZK$V1[ii])[1],","));MZ[ii,]<-c(lisi[1],lisi[2])}
    }
      else
      {MZ<-as.matrix(MZK)

      }

    }

  }
  }
  else
  {}

  Tolist<-matrix(nrow=nrow(MZ)*length(RAWlis),ncol = 4)
  colnames(Tolist)<-c("Retention time","Mearsure M/z","Intensity","Sample Name")
  #Sample list (RAWlis[[jk]]) cycle begain
  for (jk in 1:length(RAWlis)) {
    RAWDATA<-RAWlis[[jk]]
    nc<-nchar(names(RAWlis[jk]))
    # Creat sample folder (RAWlis[jk])
    if (dir.exists(substring(names(RAWlis[jk]),1,nc-6))) {} else
    {dir.create(substring(names(RAWlis[jk]),1,nc-6))}
    TopRDATA<-c()
    #MZ list cycle begain (MZ[kk])
    for (kk in 1 : nrow(MZ)) {
      #dataset (RAWDATA) initialization
      RDATA<-RAWDATA
      Mzz<-as.double(MZ[kk,])
      MZMAX1<-c()
      MZMIN1<-c()
      MZMAX1<-Mzz[1]+Mzz[1]*ppm/1000000
      MZMIN1<-Mzz[1]-Mzz[1]*ppm/1000000


      #MZ range got
      #filtering accroding to MZ range
      {if (length(unlist(strsplit(as.character(MZK$V1[1])[1],",")))==2)
      {
        MZMAX2<-c()
        MZMIN2<-c()
        MZMAX2<-Mzz[2]+Mzz[2]*ppm/1000000
        MZMIN2<-Mzz[2]-Mzz[2]*ppm/1000000
        RDATA<-llply(RDATA,function(x){x[,1][((x[,1]>MZMAX1|x[,1]<MZMIN1) )&( (x[,1]>MZMAX2|x[,1]<MZMIN2))]<-NA;return(x)})
      }
        else
        {RDATA<-llply(RDATA,function(x){x[,1][x[,1]>MZMAX1|x[,1]<MZMIN1]<-NA;return(x)})}
      }
      #delete NA
      RDATA<-llply(RDATA,function(x){x<-na.omit(x);return(x)})
      n=0;detL=0;oriL<-length(RDATA);for (i in 1 : oriL) {if (nrow(RDATA[[i-detL]])<1) {RDATA[[i-detL]]<-NULL;detL<-detL+1}}
      #filtering Finished

      #if num of mz=2
      {if (length(unlist(strsplit(as.character(MZK$V1[1])[1],",")))==2){

        #spectrum list of mz1
        RDATA1<-llply(RDATA,function(x){x[,1][(x[,1]>MZMAX1|x[,1]<MZMIN1)]<-NA;return(x)})
        RDATA1<-llply(RDATA1,function(x){x<-na.omit(x);return(x)})
        n=0;detL=0;oriL<-length(RDATA1);for (i in 1 : oriL) {if (nrow(RDATA1[[i-detL]])<1) {RDATA1[[i-detL]]<-NULL;detL<-detL+1}}
        RDATA1<-ldply(RDATA1)
        colnames(RDATA1)<-c("ScN","mz","Intensity","RT","SpN")




        #spectrum list of mz2
        RDATA2<-llply(RDATA,function(x){x[,1][(x[,1]>MZMAX2|x[,1]<MZMIN2)]<-NA;return(x)})
        RDATA2<-llply(RDATA2,function(x){x<-na.omit(x);return(x)})
        n=0;detL=0;oriL<-length(RDATA2);for (i in 1 : oriL) {if (nrow(RDATA2[[i-detL]])<1) {RDATA2[[i-detL]]<-NULL;detL<-detL+1}}

        RDATA2<-ldply(RDATA2)

        colnames(RDATA2)<-c("ScN","mz","Intensity","RT","SpN")

      } else {}}

      RDATART<-RDATA
      #filtering accroding to RT range
      {if (length(MZK)==2)
      {RTMIN<-c()
      RTMAX<-c()
      {if (length(unlist(strsplit(as.character(MZK$V1[1])[1],",")))==2) rt<-Mzz[3] else rt<-Mzz[2]}
      RTMAX<-rt+RTWin
      RTMIN<-rt-RTWin
      RDATART<-llply(RDATART,function(x){x[,3][x[,3]>RTMAX|x[,3]<RTMIN]<-NA;return(x)})
      }
        else {}

      }
      #delete NA
      RDATART<-llply(RDATART,function(x){x<-na.omit(x);return(x)})
      n=0;detL=0;oriL<-length(RDATART);for (i in 1 :length(RDATART)) {if (nrow(RDATART[[i-detL]])<1) {RDATART[[i-detL]]<-NULL;detL<-detL+1}}

      RDATART<-ldply(RDATART)
      colnames(RDATART)<-c("ScN","mz","Intensity","RT","SpN")
      RDATA<-ldply(RDATA)
      colnames(RDATA)<-c("ScN","mz","Intensity","RT","SpN")


      RDATART<-RDATART[-5]
      md<-melt(RDATART,id=c("ScN","mz","RT"))
      new<-dcast(md,ScN+mz+RT~variable,sum)
      TopRDATA<-new[order(-new$Intensity),]

      Tolist[kk+(jk-1)*nrow(MZ),1]<- round(TopRDATA[1,3],digits=1)
      intmz<-RDATART[RDATART$ScN==TopRDATA[1,1],]
      Tolist[kk+(jk-1)*nrow(MZ),2]<-intmz[order(-intmz$Intensity),][1,2]
      Tolist[kk+(jk-1)*nrow(MZ),3]<-format(TopRDATA[1,4],scientific = TRUE,digits = 2)

      Tolist[kk+(jk-1)*nrow(MZ),4]<-names(RAWlis[jk])

      #pic
      wkwd<-getwd()
      setwd(substring(names(RAWlis[jk]),1,nc-6))
      mzst<-paste(paste((MZ[kk,]),collapse = ", "),"RT= ",round(TopRDATA[1,3],digits=1),seq="")
      jpeg(file=paste(mzst,".jpeg",seq=""),width=1200,height=1000,res=56*2)
      opar<-par(no.readonly = TRUE)
      #par(mfrow=c(2,1))

      #EIC picture

      plot(RDATA$RT,(RDATA$Intensity/TopRDATA[1,4])*100, main=paste("EIC of ",mzst,"(MaxIntensity: ",format(TopRDATA[1,4],scientific = TRUE,digits = 2),")"),type="b",xlab = "Retention Time(min)",ylab = "Relative Intensity")
      text(TopRDATA[1,3],45,paste("RT= ",round(TopRDATA[1,3],digits=1),seq=""),pos=2,offset=2)
      text(TopRDATA[1,3],40,paste("Measured m/z= ",round(TopRDATA[1,2],digits = 4),seq=""),pos=2,offset=2)
      {if (length(MZK)==2) {abline(v=RTMAX,col = "red");abline(v=RTMIN,col = "red")}}
      abline(v=TopRDATA[1,3],col = "green")
      #Full spectrum
      #plot(RAWDATA[[paste(TopRDATA[1,4])]][,1],(RAWDATA[[paste(TopRDATA[1,4])]][,2]/TopRDATA[1,3])*100, main=paste("Spectrum of scan number",TopRDATA[1,4]," Spectrum number",TopRDATA[1,5],"(MaxIntensity: ",format(TopRDATA[1,3],scientific = TRUE,digits = 2),")"),type="h",xlab = "m/z",ylab = "Relative Intensity")

      dev.off()
      setwd(wkwd)







      write.csv(Tolist,"temp.csv" )

      #return(writeClipboard(Tolist[,2]))


    }
  }
  TTo<-paste(Tolist[,1],Tolist[,2],Tolist[,3],Tolist[,4],sep = "\t")
  return(writeClipboard(TTo))
}
