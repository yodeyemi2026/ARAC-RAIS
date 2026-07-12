#==============================================================================
#RDPM Output Module
#Version 1.7
#Revised: Feb-13-2018
#==============================================================================

#DESCRIPTION

#Takes a list object (Blocks) consisting of one or more named SxTxN arrays (T and N should be the same across all arrays) and creates an array that holds modeled results for several subsets of the data:
      #1)Overall stats for each array, summed over all S, calculated by time period.
      #2)Differences between overall models (e.g., BC-CF1, BC-CF2, CF1-CF2 etc) displayed by time period
      #3)Results for each state in each array, diplayed by time period.
      #4)Result will be an array with (Nstates + Ndiff + Nprimary)*N_AgeCat rows, with columns being the the results of the Stats function.

#REQUIRES:plyr and abind package

#------------------------------------------------------------------------------


Output<-function(Blocks=NULL){
  Blocks<-Results         
  Bnames<-c("all0.t","all.t") #Get names of primary blocks
  NBlocks<-length(Blocks)#get number of primary blocks
  NAge<-length(Blocks[[1]][1,,1])#get number of age categories, which is common across all primary blocks
  N<-length(Blocks[[1]][1,1,])#get number of samples/trials

  # Create aggregated primary blocks by category and name  
  Pagg<-llply(Blocks,colSums)#will generate a list of NAge X N arrays where each cell is the total population fraction in that age catebory in that primary block for that trial
  names(Pagg)<-Bnames

  # Create aggregated difference blocks from aggregated primary blocks and name
  Diff<-list()#create placeholder list for differences
  Diffnames<-NULL#will hold difference names
  for (i in 1:(NBlocks-1)) {
     for (j in (i+1):NBlocks) {
       Diffnames<-c(Diffnames,paste0(names(Blocks[j]),"-",names(Blocks[i]))) #add a difference name to the name vector
       Diff[[length(Diff)+1]]<-Pagg[[j]]-Pagg[[i]] #subtract the total survival of Block i from Block j [difference of two NAge x N arrays, elementwise]
     }
  }
  
  names(Diff)<-Diffnames #assign names
    
  
  #Create aggregate classes 
  agglist<-list()
  agglist[["cs0.t"]]<-Blocks[[1]]["csA0t",,]+Blocks[[1]]["csD0t",,]#all current smokers in base case
  agglist[["fs0.t"]]<-Blocks[[1]]["fsA0t",,]+Blocks[[1]]["fsE0t",,]#all former smokers in base case
  cst<-0
  cst_states<-c("csAt","csBt","csCt","csDt")
  for (i in 1:length(cst_states)) cst<-cst+Blocks[[2]][cst_states[i],,]
  agglist[["cs.t"]]<-cst
  fst<-0
  fst_states<-c("fsAt","fsCt","fsDt","fsEt")
  for (i in 1:length(fst_states)) fst<-fst+Blocks[[2]][fst_states[i],,]
  agglist[["fs.t"]]<-fst
  
  snt<-0
  snt_states<-c("snAtn","snAte","snBt","snCt","snDt")
  for (i in 1:length(snt_states)) snt<-snt+Blocks[[2]][snt_states[i],,]
  agglist[["sn.t"]]<-snt
  sna<-0
  snAt_states<-c("snAtn","snAte")
  for (i in 1:length(snAt_states)) sna<-sna+Blocks[[2]][snAt_states[i],,]
  agglist[["snAt.t"]]<-sna
  fsnt<-0
  fsnt_states<-c("fsnAt","fsnBt","fsnCt","fsnDt")
  for (i in 1:length(fsnt_states)) fsnt<-fsnt+Blocks[[2]][fsnt_states[i],,]
  agglist[["fsn.t"]]<-fsnt
  dut<-0
  dut_states<-c("duAt","duBt")
  for (i in 1:length(dut_states)) dut<-dut+Blocks[[2]][dut_states[i],,]
  agglist[["du.t"]]<-dut
  fdut<-0
  fdut_states<-c("fduAt","fduBt")
  for (i in 1:length(fdut_states)) fdut<-fdut+Blocks[[2]][fdut_states[i],,]
  agglist[["fdu.t"]]<-fdut
  agglist[["nsA-nsA0"]]<-Blocks[[2]]["nsA",,]-Blocks[[1]]["nsA0",,]
  agglist[["cs.t-cs0.t"]]<-agglist[["cs.t"]]-agglist[["cs0.t"]]
  agglist[["fs.t-fs0.t"]]<-agglist[["fs.t"]]-agglist[["fs0.t"]]
    
  #Create a single block with rows representing different trajectories under analysis
  MainBlock<-abind(c(Pagg[1:NBlocks],Diff[1:length(Diff)],Blocks[1:NBlocks],agglist),along=1)
  
  # Stat function takes a vector specifed by each row and column in the above array [length will be number of simulations run (i.e., N)] and returns a named vector:    "Rowname"[row,column,[mean, sd, se, p05,p50,p95]
  statnames<-c("mean","sd","mc error","p025","median","p975",
    "LE_mean","LE_Sd","LE_p025","LE_median","LE_p975",
    "QALE_mean","QALE_Sd","QALE_p025","QALE_median","QALE_p975")
  Stat<-function(MainRow){
     avg<-mean(MainRow)
     stdev<-sd(MainRow)
     mcerr<-NA 
     Q<-quantile(MainRow,c(.025,.5,.975),type=6)
     LE<-c(NA,NA,NA,NA,NA)
     QALE<-c(NA,NA,NA,NA,NA)
     statvect<-c(avg,stdev,mcerr,Q,LE,QALE)
     names(statvect)<-statnames
     return(statvect)  
  }
 
   LEStat<-function(Vector,Type){
     Q<-quantile(Vector,c(.025,.5,.975),type=6)
     Mean<-mean(Vector)
     Sd<-sd(Vector)
     statvect<-c(Mean,Sd,Q)
     pn<-c('mean','Sd','p025','median','p975')
     names(statvect)<-c(paste(Type,pn,sep='_'))
     return(statvect)  
  }
  
  #use aaply on the bound array where you split by row and column and apply the Stats function to each
  output<-aaply(MainBlock,c(1,2),Stat)
  
 if(MinAge+CatWidth > 18) {print("LE and QALE not calculated because age 18 occurs before age category 2")} else { 
  
  if(MinAge+(NAge*CatWidth)<92) {print("LE and QALE not calculated because maximum age is less than 92")} else {
  
  #LE and QALE
    #Subset Blocks
    BC_S<-t(MainBlock[1,,]) #Survivors in Base Case (transposed)
    CF_S<-t(MainBlock[2,,]) #Survivors in Counterfactual (transposed)
    BC_D<-matrix(nrow=N,ncol=NAge)
    CF_D<-matrix(nrow=N,ncol=NAge)
    for (l in 2:(NAge-1)) {           
      BC_D[,l]<- BC_S[,l] -BC_S[,l+1]
      CF_D[,l]<- CF_S[,l] -CF_S[,l+1]
    }
    BC_D[,1]<-0
    CF_D[,1]<-0
    
    BC_PY0<-CatWidth*(BC_S-(BC_D/2)) #calculate person years
    CF_PY0<-CatWidth*(CF_S-(CF_D/2)) #calculate person years
    
    nAC0<-(18-MinAge)/CatWidth #number of age categories before age 18 (does not need to be a whole number)
    nAC<-ceiling((18-MinAge)/CatWidth)  #age category containing age 18 (will be a whole number)

    BC_PY<-BC_PY0[,(nAC+1):length(BC_PY0[1,])]  #PY, start at first full age category greater than 18
    CF_PY<-CF_PY0[,(nAC+1):length(CF_PY0[1,])]  #PY, start at first full age category greater than 18

    S_frac<-nAC-nAC0 #fraction of initial survivors in Age Category containing age 18
    
    BC_PY_frac<-BC_PY0[,nAC]*S_frac #BC PY, fraction 18+ of age category containing 18
    CF_PY_frac<-CF_PY0[,nAC]*S_frac #CF PY, fraction 18+ of age category containing 18
    
    
    BC_S0 <-BC_S[,nAC]*S_frac+BC_S[,(nAC+1)]*(1-S_frac) #initial BC survivors at age 18
    CF_S0 <-CF_S[,nAC]*S_frac+CF_S[,(nAC+1)]*(1-S_frac) #initial CF survivors at age 18
       
    BC_LE<-(rowSums(BC_PY,1)+BC_PY_frac)/BC_S0  #BC LE
    CF_LE<-(rowSums(CF_PY,1)+CF_PY_frac)/CF_S0  #CF LE
    
    #QALE
    
    #Calculate EQ5d for each age category
    DF<- data.frame(AgeCat=sort(rep(1:NAge,CatWidth)),Age=MinAge:(MinAge+(CatWidth*NAge)-1),aEQ5d=NA)
    DF$aEQ5d<-0.8505
    DF$aEQ5d[DF$Age>24]<-0.8219
    DF$aEQ5d[DF$Age>34]<-0.8104
    DF$aEQ5d[DF$Age>44]<-0.7859
    DF$aEQ5d[DF$Age>54]<-0.7779
    DF$aEQ5d[DF$Age>64]<-0.7445
    DF$aEQ5d[DF$Age>74]<-0.6725
    tEQ5d<-ddply(DF ,.(AgeCat),summarise,avgEQ5d=round(mean(aEQ5d,na.rm=TRUE),4))
    EQ5d <- as.vector(tEQ5d$avgEQ5d)
    #End calculate EQ5d for each age category
    
    BC_QPY<-t(t(BC_PY)*EQ5d[(nAC+1):NAge])   #BC PY*EQ5d, start at first full age category greater than 18
    CF_QPY<-t(t(CF_PY)*EQ5d[(nAC+1):NAge])   #CF PY*EQ5d, start at first full age category greater than 18
    
    BC_QPY_frac<-t(t(BC_PY_frac)*EQ5d[nAC])  #BC PY*EQ5d, fraction 18+ of age category containing 18
    CF_QPY_frac<-t(t(CF_PY_frac)*EQ5d[nAC])  #CF PY*EQ5d, fraction 18+ of age category containing 18
          
    BC_QALE<-(rowSums(BC_QPY,1)+BC_QPY_frac)/BC_S0    #BC QALE
    CF_QALE<-(rowSums(CF_QPY,1)+CF_QPY_frac)/CF_S0    #CF QALE
    
    sDF_LE<-LEStat(CF_LE-BC_LE,"LE")
    sBC_LE<-LEStat(BC_LE,"LE")
    sCF_LE<-LEStat(CF_LE,"LE")
    
    sDF_QALE<-LEStat(CF_QALE-BC_QALE,"QALE")
    sBC_QALE<-LEStat(BC_QALE,"QALE")
    sCF_QALE<-LEStat(CF_QALE,"QALE")

    #Load BCLE &QALE
     nms<-c(names(sBC_LE),names(sBC_QALE))
     output['all0.t',NAge,nms]<- c(sBC_LE,sBC_QALE)
    #Load CFLE &QALE
     nms<-c(names(sCF_LE),names(sCF_QALE))
     output['all.t',NAge,nms]<- c(sCF_LE,sCF_QALE) 
     #Load DFLE &DFQALE
     nms<-c(names(sDF_LE),names(sDF_QALE))
     output['all.t-all0.t',NAge,nms]<- c(sDF_LE,sDF_QALE)  
  
}}  
       #output['all0.t',,]
       #output['all.t',,]
    
  #Standardize format
  nvaluetypes<-length(output[,1,1])#number of different types of values being calculated
  valuenames<-names(output[,1,1])#get the names of the calculation classes
  nstats<-length(statnames)#the number of stats being calculated
  StdOutput<-array(dim=c(nvaluetypes*NAge,2+nstats)) #create placeholder array
  colnames(StdOutput)<-c("Result","AgeCat",statnames)#name the columns 
  outnames<-NULL#character vector to hold result names
  
  for( i in 1:nvaluetypes){
       for(j in 1:NAge){
           rowpos<-(i-1)*NAge+j
           StdOutput[rowpos,1]<-i
           StdOutput[rowpos,2]<-j
           StdOutput[rowpos,3:(nstats+2)]<-output[i,j,1:nstats]
           outnames<-c(outnames,paste0(valuenames[i]))#add the name of this calculation to outnames
       }
  
  }
  
  rownames(StdOutput)<-outnames

  
  
  return(StdOutput) 
  
  

}
