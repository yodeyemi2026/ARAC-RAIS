#===============================================
#Script to create a transition diagram from TM
#Version 1.7
#Revised: Feb-13-2018
#===============================================



#Stored as DataFrame
#Load Data

DPM_Plots<-function (RR1,RR2) {
pdf(paste0(UserID,"-",ModelName,"_RR_",Stamp,".pdf"),width=8,height=10)
par(mfrow=c(2,1))
hist(RR1)
hist(RR2)
dev.off()



Ex<-rTM
TPB<-rTPB
TPC<-rTPC 

#BASE CASE
#Create code list
Cd<-unique(TPB$ST0)
Ns<-length(Cd)
TMp<-array(0,c(Ns,Ns))
TMl<-array(0,c(Ns,Ns))
colnames(TMp)<-Cd
rownames(TMp)<-Cd
colnames(TMl)<-Cd
rownames(TMl)<-Cd

    #Replicate by number of age classes
    #Step thru TPE data frame rows to update matrix for each age class
      for (i in 1:length(TPB$Code)){
        eq<-(TPB[i,])
#determine cell to update
        ri<-which(Cd==as.character(eq[[4]]))
        ci<-which(Cd==as.character(eq[[5]]))
        Eq<-as.character(eq[[6]])
        TMp[ri,ci]<-(1)
        TMl[ri,ci]<-Eq
        }
 TMl<-t(TMl)
 
 
pdf(paste0(UserID,"-",ModelName,"_BCTM_",Stamp,".pdf"),width=10,height=7)
plotmat(TMl,pos=c(5),name=colnames(TMl),
  relsize=1,box.size=0.04,box.cex=0.5, curve=0.25,
  my=0.2,dtext=0.55, self.shifty=0.04,
  arr.len=0.6,arr.pos = 0.5,arr.type='curved',
  cex.txt=0.5,self.cex=0.5,
  )
dev.off()
#Counterfactual
#Create code list
Codes<-unique(subset(TPC,select = c('ST0','Prow','Pcol')))
Cd<-unique(TPC$ST0)
Ns<-length(Cd)
TMp<-array(0,c(Ns,Ns))
TMl<-array(0,c(Ns,Ns))
colnames(TMp)<-Cd
rownames(TMp)<-Cd
colnames(TMl)<-Cd
rownames(TMl)<-Cd

    #Replicate by number of age classes
    #Step thru TPE data frame rows to update matrix for each age class
      for (i in 1:length(TPC$Code)){
        eq<-(TPC[i,])
#determine cell to update
        ri<-which(Cd==as.character(eq[[4]]))
        ci<-which(Cd==as.character(eq[[5]]))
        Eq<-as.character(eq[[6]])
        TMp[ri,ci]<-(1)
        TMl[ri,ci]<-Eq
        }
TMl<-t(TMl)
ST0<-row.names(TMl)
SD<-data.frame(ST0)
SD$Sort<-row.names(SD)
#Add Sort index
TMs<-merge(SD,Codes)
TMs<-TMs[order(as.numeric(TMs$Sort)),]
PP<-data.matrix(cbind(TMs[4]/(5.5),(1-TMs[3]/(6.2))+0.06))
pdf(paste0(UserID,"-",ModelName,"_CFTM_",Stamp,".pdf"),width=10,height=7)
plotmat(TMl,name=colnames(TMl),pos=PP, #mx=-2,my=-2,
  relsize=0.8,box.size=0.05,box.cex=0.7, curve=0.15,
  dtext=-0.5, self.shifty=0.02,shadow.size=0,
  arr.len=0.6,arr.pos = 0.5,arr.type='curved',
  arr.lcol ='grey60',arr.col ='grey60', lcol='grey60',
  cex.txt=0.5,self.cex=0.5)
dev.off()
}


