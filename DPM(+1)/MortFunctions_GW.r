#==================================================================
#Mortality Rate Functions
#Version 1.7
#Revised: Feb-13-2018
#==================================================================

#------------------------------------------------------------------
#TESTING CODE
#------------------------------------------------------------------

#m0 returns 1 % mortality for each age class (for diagnostic use only)
m0<-function(past,betas,wi,ma,RR=NULL){
  ycs<-0
 return(0.01)
}

#Speed opitmized but does not calculate correctly, for testing only
m3<-function(past,betas,wi,ma,SPath=TRUE){
  #State codes
  #NS  1
  #CS0 2
  #FS0 3
  #CSD 4
  #FSD 5
  YCS<-c(2,4)
  YQ1<-c(3)
  YQ2<-c(5)
  age<-wi*(length(past)-.5)+ma #use current category midpoint as age
  ycs<-0
  yq1<-0
  yq2<-0
  yqs<-0
 #Note that '==' is faster than %in% for single comparisons
 #for(i in 1:length(YCS)) ycs<-ycs+wi*sum(past==YCS[i])
  ycs<-wi*length(past %in% YCS)
  yq1<-wi*length(past == YQ1)
  yq2<-wi*length(past == YQ2)
  if(yq2==0) yqs <- yq1 else yqs<-yq2
 # nsm<-as.numeric(exp(-8.05681+0.0329335*age+0.000397*(age*age)))
 #Note,betas[betaref[1]] is not optimal should consider passing array in correct order 
  nsm<-as.numeric(exp(betas[1]+betas[2]*age+betas[3]*(age*age)))
  csm<-as.numeric(exp(betas[6]*ycs+betas[4]*age*ycs))
  fsm<-as.numeric(exp(betas[7]*yqs+betas[5]*age*yqs))
  mort<-wi*nsm*csm*fsm

  return(mort)
}

#==================================================================
#Mortalify Functions
#==================================================================

#------------------------------------------------------------------
#Base Case 
#------------------------------------------------------------------

#b1 follows paper description, only uses last period of former smoking
b1<-function(past,betas,wi=5,ma=13,SPath=TRUE){
  #state-variable cross reference vectors. assign a vector that contains states that increase the value of that variable.
  #state codes: NS=1, CS0=2, FS0=3, CSD=4, FSD=5
  ac<-length(past)+1# current age is  length, for 1st transtion past is null
  age<-ma + wi*(ac-1)+wi/2#use current category midpoint as age
  if(ac==1){lCode<-1} else {lCode<-past[ac-1]} #will remove lCode in final, just slows it down, but retained for clarity
  #and to comapre to CF function for now
  YCS<-c(2,4)
  YQ1<-c(3)
  YQ2<-c(5)
  ycs<-0
  yq1<-0
  yq2<-0
  yqs<-0
  r.nt<-as.numeric(exp(betas[,1]+betas[,2]*age+betas[,3]*(age*age)))
  if (lCode == 1){mort<-wi*r.nt} else {
    #Sum cs0 and csd
    ycs<-wi*sum(past %in% YCS)#sum CS0 and CSD , all smoking periods added
    yq1<-wi*sum(past == YQ1)
    yq2<-wi*sum(past == YQ2)
    #Sum FS0 and FSD
    yfs<-yq1 + yq2 #sum FS0 and FSD, all former smoking periods  are added
    RR.cs<-as.numeric(exp(betas[,6]*ycs+betas[,4]*age*ycs))
    RR.fs<-as.numeric(exp(betas[,7]*yfs+betas[,5]*age*yfs))
    mort<-wi*r.nt*RR.cs*RR.fs}
  if (SPath==TRUE) {
  opast<-paste(past,collapse=',')
  omort<-mort[1]
  bcPath<-data.frame(mort1,'past'=opast,'mort'=omort, stringsAsFactors = FALSE)
  BCPath<<-rbind(BCPath,bcPath)}
  #out.df = as.data.frame(do.call(rbind, out))
  #write.table(out.df,file="BC.txt",append=TRUE)
  return(mort)
}


#------------------------------------------------------------------
#Counterfactual Case 
#------------------------------------------------------------------
c1<-function(past,betas,wi=5,ma=13,SPath=TRUE){
  ac<-length(past)+1# current age is  length, for 1st transtion past is null
  age<-ma + wi*(ac-1)+wi/2#use current category midpoint as age
  if(ac==1){lCode<-1} else {lCode<-past[ac-1]}
  RR_sncs<-pRR[1]
  RR_qsnqcsm<-pRR[2]
  #ln_RR_sncsm<-(-2.99573) 
  #ln_RR_qsnqcsm<-(-2.99573)
  #RR_sncs<-exp(ln_RR_sncsm)
  #RR_qsnqcsm<-exp(ln_RR_qsnqcsm)
#state-variable cross reference vectors. assign a vector that contains states that increase the value of that variable.
  # State Codes
  #	nsA	  1
  #	csAt	2
  #	snAte	3
  #	csBt	4
  #	snBt	5
  #	snCt	6
  #	duAt	7
  #	csCt	8
  #	duBt	9
  #	fsnAt	10
  #	snDt	11
  #	fsAt	12
  #	csDt	13
  #	fsEt	14
  #	fsCt	15
  #	fsnCt	16
  #	fduAt	17
  #	fsnDt	18
  #	fsDt	19
  #	fduBt	20
  #	fsnBt	21
  #	snAtn	22

YNT<-c(1)
YCS0<-c(2,4)
YCS1<-c(13,8)
YA0<-c(3,22,5)
YA1<-c(6,11)
YDU<-c(7,9)
YFS0<-c(12,15)
YFS1<-c(14,19)
YFA0<-c(10,21)
YFA1<-c(18,16)
YFD0<-c(17,20)
#Add years of each behavior
ynt<-wi*sum(past %in% YNT)    #years no use
ycs0<-wi*sum(past %in% YCS0)  #years smoking 1st time
ycs1<-wi*sum(past %in% YCS1)  #years smoking 2nd time
yau0<-wi*sum(past %in% YA0)   #years snus 1st time
yau1<-wi*sum(past %in% YA1)   #years snus 2nd time
ydu0<-wi*sum(past %in% YDU)   #years dual use
yfs0<-wi*sum(past %in% YFS0)  #years former smoking 1st time
yfs1<-wi*sum(past %in% YFS1)  #years former smoking 2nd time
yfa0<-wi*sum(past %in% YFA0)  #years former snus 1st time
yfa1<-wi*sum(past %in% YFA1)  #years former snus 2nd time
yfdu0<-wi*sum(past %in% YFD0)  #years former dual use
#sums
ycs<-ycs0+ycs1+ydu0
yfs<-yfs0+yfs1+yfdu0
yfa<-yfa0+yfa1
yau<-yau0+yau1
#
#Calculate mortality
 r.nt<-as.numeric(exp(betas[,1]+betas[,2]*age+betas[,3]*(age*age)))
 if (lCode == 1){  #NT
    mort<-wi*r.nt
 } else if (lCode %in% c(2,12,13,14)){ #NT CS, NS CS FS; NT CS FS CS1; NT CS FS CS1 FS1
    RR.cs<-as.numeric(exp(betas[,6]*ycs+betas[,4]*age*ycs))
    RR.fs<-as.numeric(exp(betas[,7]*yfs+betas[,5]*age*yfs))
    mort<-wi*r.nt*RR.cs*RR.fs 
 } else if (lCode %in% c(3,22)) { #NT SN 
    RR.sn<-as.numeric(RR_sncs*r.nt*exp(betas[,6]*yau+betas[,4]*age*yau)+(1-RR_sncs)*r.nt)
    mort<-wi*RR.sn 
 } else if (lCode %in% c(4)) { #NT SN CS 
    RR.cs<-as.numeric(exp(betas[,6]*ycs+betas[,4]*age*ycs))
    RR.sn<-as.numeric(RR_sncs*exp(betas[,6]*yau+betas[,4]*age*yau)+(1-RR_sncs))
    mort<-wi*r.nt*RR.cs*RR.sn
 } else if (lCode %in% c(5)) { #NT CS SN 
    RR.cs<-as.numeric(exp(betas[,6]*ycs+betas[,4]*age*ycs))
    RR.sn<-as.numeric(RR_sncs*exp(betas[,6]*yau+betas[,4]*age*yau)+(1-RR_sncs))
    RR.f3<-as.numeric((1-RR_sncs)*exp(betas[,7]*yau0+betas[,5]*age*yau0)+(RR_sncs))
    mort<-wi*r.nt*RR.cs*RR.sn*RR.f3
 } else if (lCode %in% c(6)) { #NT SN CS SN 
    RR.cs<-as.numeric(exp(betas[,6]*ycs+betas[,4]*age*ycs))
    RR.sn<-as.numeric(RR_sncs*exp(betas[,7]*yau+betas[,5]*age*yau)+(1-RR_sncs))
    RR.f3<-as.numeric((1-RR_sncs)*exp(betas[,7]*yau1+betas[,5]*age*yau1)+(RR_sncs))
    mort<-wi*r.nt*RR.cs*RR.sn*RR.f3
 } else if (lCode %in% c(7)) { #NT SN DU  #Dual use
    RR.cs<-as.numeric(exp(betas[,6]*ycs+betas[,4]*age*ycs))
    RR.sn<-as.numeric(RR_sncs*exp(betas[,6]*(yau)+betas[,4]*age*(yau))+(1-RR_sncs))
    mort<-wi*r.nt*RR.cs*RR.sn
 } else if (lCode %in% c(8)) { #NT CS SN CS 
    RR.cs<-as.numeric(exp(betas[,6]*ycs+betas[,4]*age*ycs))
    RR.sn<-as.numeric(RR_sncs*exp(betas[,6]*yau+betas[,4]*age*yau)+(1-RR_sncs))
    RR.f3<-as.numeric((1-RR_sncs)*exp(betas[,7]*yau+betas[,5]*age*yau)+(RR_sncs))
    mort<-wi*r.nt*RR.cs*RR.sn*RR.f3
 } else if (lCode %in% c(9)) { #NT CS DU  #Dual use, check SN
    RR.cs<-as.numeric(exp(betas[,6]*(ycs)+betas[,4]*age*ycs))
    RR.sn<-as.numeric(RR_sncs*exp(betas[,6]*(yau)+betas[,4]*age*(yau))+(1-RR_sncs))
    mort<-wi*r.nt*RR.cs*RR.sn
    
 } else if (lCode %in% c(10)) { #NT SN FSN    #yfa0 vs yfa
    RR.sn<-as.numeric(exp(betas[,6]*yau+betas[,4]*age*yau))
    RR.fsn<-as.numeric(exp(betas[,7]*yfa0+betas[,5]*age*yfa0))
    RR.snfsn<-RR_qsnqcsm*r.nt*RR.sn*RR.fsn +(1-RR_qsnqcsm)*r.nt
    mort<-wi*RR.snfsn
    
 # } else if (lCode %in% c(10)) { #NT SN FSN    #yfa0 vs yfa  
 #   RR.sn<-as.numeric(exp(betas[,6]*yau+betas[,4]*age*yau)+(1-RR_qsnqcsm)*r.nt)
 #   RR.fsn<-as.numeric(exp(betas[,7]*yfa0+betas[,5]*age*yfa0))
 #   RR.snfsn<-RR_qsnqcsm*r.nt*RR.sn*RR.fsn+(1-RR_qsnqcsm)*r.nt
 #   mort<-wi*RR.snfsn
    
    
 } else if (lCode %in% c(11)) { #NT SN FSN SN    #yfa0 vs yfa
    RR.sn<-as.numeric(exp(betas[,6]*yau+betas[,4]*age*yau))
    RR.fsn<-as.numeric(exp(betas[,7]*yfa+betas[,5]*age*yfa))
    RR.snfsn<-RR_qsnqcsm*r.nt*RR.sn*RR.fsn+(1-RR_qsnqcsm)*r.nt
    mort<-wi*RR.snfsn
 } else if (lCode %in% c(15)) { #NT SN CS FS
    RR.cs<-as.numeric(exp(betas[,6]*ycs+betas[,4]*age*ycs))
    RR.fs<-as.numeric(exp(betas[,7]*yfs+betas[,5]*age*yfs))        
    RR.sn<-as.numeric(RR_sncs*exp(betas[,6]*yau+betas[,4]*age*yau)+(1-RR_sncs))
    mort<-wi*r.nt*RR.cs*RR.fs*RR.sn
    
 } else if (lCode %in% c(16)) { #NT SN CS SN FSN  
    RR.cs<-as.numeric(exp(betas[,6]*ycs+betas[,4]*age*ycs))
    ###RR.f3 only considered 2nd AU period
    RR.f3<-as.numeric(1-RR_sncs)*exp(betas[,7]*yau1+betas[,5]*age*yau1)+(RR_sncs)
    RR.sn<-as.numeric(RR_qsnqcsm*exp(betas[,6]*yau+betas[,4]*age*yau)*exp(betas[,7]*yfa+betas[,5]*age*yfa)+(1-RR_qsnqcsm))
    RR.sncsfsn<-r.nt*RR.cs*RR.f3*RR.sn
    mort<-wi*RR.sncsfsn
    
 } else if (lCode %in% c(17)) { #NT SN DU FDU 
    RR.cs<-as.numeric(exp(betas[,6]*ycs+betas[,4]*age*ycs))
    RR.fs<-as.numeric(exp(betas[,7]*yfs+betas[,5]*age*yfs))   
    RR.sn<-as.numeric(RR_sncs*exp(betas[,6]*(yau)+betas[,4]*age*(yau))+(1-RR_sncs))
    #does not consider AU during Dual use
    mort<-wi*r.nt*RR.cs*RR.fs*RR.sn
    
 } else if (lCode %in% c(18)) { #NT SN FSN SN FSN 
    RR.sn<-as.numeric(exp(betas[,6]*yau+betas[,4]*age*yau))
    RR.fsn<-as.numeric(exp(betas[,7]*yfa+betas[,5]*age*yfa))
    RR.snfsn<-RR_qsnqcsm*r.nt*RR.sn*RR.fsn+(1-RR_qsnqcsm)*r.nt
    mort<-wi*RR.snfsn
 } else if (lCode %in% c(19)) { #NT CS SN CS FS 
    RR.cs<-as.numeric(exp(betas[,6]*ycs+betas[,4]*age*ycs))
    RR.fs<-as.numeric(exp(betas[,7]*yfs+betas[,5]*age*yfs))
    RR.sn<-as.numeric(RR_sncs*exp(betas[,6]*yau+betas[,4]*age*yau)+(1-RR_sncs))
    RR.f3<-as.numeric((1-RR_sncs)*exp(betas[,7]*yau+betas[,5]*age*yau)+(RR_sncs))
    mort<-wi*r.nt*RR.cs*RR.fs*RR.sn*RR.f3 
 } else if (lCode %in% c(20)) { #NT CS DU FDU 
    RR.cs<-as.numeric(exp(betas[,6]*(ycs)+betas[,4]*age*(ycs)))
    RR.fs<-as.numeric(exp(betas[,7]*yfs+betas[,5]*age*yfs))
    #does not consider AU during Dual use
    mort<-wi*r.nt*RR.cs*RR.fs
 } else if (lCode %in% c(21)) { #NS CS SN FSN 
    RR.cs<-as.numeric(exp(betas[,6]*ycs+betas[,4]*age*ycs))
    RR.f3<-as.numeric(1-RR_sncs)*exp(betas[,7]*yau+betas[,5]*age*yau)*exp(betas[,7]*yfa+betas[,5]*age*yfa)+RR_sncs
    RR.sn<-as.numeric(RR_qsnqcsm*exp(betas[,6]*yau+betas[,4]*age*yau)*exp(betas[,7]*yfa+betas[,5]*age*yfa)+(1-RR_qsnqcsm))
    mort<-wi*r.nt*RR.cs*RR.f3*RR.sn
 } else {mort<-(-999)}
  if (SPath==TRUE) {
  opast<-paste(past,collapse=',' )
  omort<-mort[1]
  cfPath<-data.frame(mort1,'past'=opast,'mort'=omort, stringsAsFactors = FALSE)
  CFPath<<-rbind(CFPath,cfPath) }      
return(mort)
}

