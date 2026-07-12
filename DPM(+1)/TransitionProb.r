#====================================================================================
#Transition probability constructors
#Version 1.7
#Revised: Feb-13-2018
#===================================================================================
#DESCRIPTION: Reads transition probabilites from ModSpec file and uses formulas from the spec file to calculate state-to-state transition probabilities for all age categories and runs.

#two versions: TP operates on a single array of transistion probabilties (for QC and testing)
# TPm operates on an entire block of matrices for production runs

#-----------------------------------------------------------------------------------
#Single matrix version
#-----------------------------------------------------------------------------------

TP<- function (TM0,TPE,Snm) {
    #Create empty TP using:
      #TM0 transition matrix
      #TPE data frame that describes the transtion probabilites based on the TM
      #Snm= list of state names
    Ns<-length(Snm) #number of states
    nac<-length(TM0$AgeCl) #number of age classes from TM0
    TPb<-array(dim=c(Ns,Ns,nac),0) #Transiton probabilites matrix,empty
        #diag(TPb0)<-1  #Unless otherwsie specified, 100% probability of remaining in the same state
    colnames(TPb)<-Snm
    rownames(TPb)<-Snm
    #Replicate by number of age classes
    #Step thru TPE data frame rows to update matrix for each age class
    TPbeq<-TPE #Select base case
    for (ac in 1:nac){
      for (i in 1:length(TPbeq$Code)){
        TMc<-TM0[ac,]
        eq<-(TPbeq[i,])
#determine cell to update
        ri<-which(Snm==as.character(eq[[4]]))
        ci<-which(Snm==as.character(eq[[5]]))
        Eq<-as.character(eq[[3]])
        Eq<-(gsub('T','TMc$T',Eq))
        EQ<-eval(parse(text=Eq))
        TPb[ri,ci,ac]<-EQ
      }
    }
  return(TPb)
}

#-------------------------------------------------------------------------------------
#Matrix block version
#-------------------------------------------------------------------------------------

TPm<- function (rTM,TM0,TPE,Snm,Nruns) {
 
      #Create empty TP using:
      #TM0 transition matrix
      #TPE data frame that describes the transtion probabilites based on the TM
      #Snm= list of state names
    Ns<-length(Snm) #number of states
    nac<-dim(TM0)[1] #number of age classes from TM0
    #TPb<-ff(vmode="double",dim=c(Ns,Ns,nac,Nruns),0) #Transiton probabilites matrix,empty
    TPb<-array(dim=c(Ns,Ns,nac,Nruns),0)
    
        #diag(TPb0)<-1  #Unless otherwsie specified, 100% probability of remaining in the same state
    colnames(TPb)<-Snm
    rownames(TPb)<-Snm
    #Replicate by number of age classes
    #Step thru TPE data frame rows to update matrix for each age class
    TPbeq<-TPE #Select case
    for (ac in 1:nac){
      for (i in 1:length(TPbeq$Code)){
        TMc<-data.frame(t(TM0[ac,,]))
        names(TMc)<-names(rTM[2:length(rTM)])
        eq<-(TPbeq[i,])
        #determine cells to update
        ri<-which(Snm==as.character(eq[[4]]))
        ci<-which(Snm==as.character(eq[[5]]))
        Eq<-as.character(eq[[3]])
        Eq<-(gsub('T','TMc$T',Eq))
        EQ<-eval(parse(text=Eq))
        TPb[ri,ci,ac,]<-EQ
      }
    }
return(TPb)
}
    










