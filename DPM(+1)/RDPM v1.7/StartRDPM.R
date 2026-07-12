# library(odbc)
# setwd("F:/RDPM/RDPM v1.7")
# SQLC<-'SELECT * from DPM.dbo.vw_ModSpec where JobID = 68363'
# conn_str<-dbConnect(odbc::odbc(),"DPM",uid="<db_username>",pwd="<db_password>");
# rINIT<-dbGetQuery(conn_str,SQLC)
# Input<-rINIT
# BatchNum=1
# Paths<<-dbGetQuery(conn_str,"SELECT * FROM RPaths WHERE IsActive = 'True'")

#==============================================================================
#Initialization script file for starting RDPM and capturing output to file
#Version 1.7
#Revised: Feb-13-2018
#==============================================================================

log_capture <- vector('character')
con    <- textConnection('log_capture', 'wr', local = TRUE)
sink(con)
SourcePath<-getwd() #save the location where RDPM was called from
SinkPath <- SourcePath #Get location of sink file (which will become the .log file) 

#------------------------------------------------------------------------------
#Load Model Files and R Packages
#------------------------------------------------------------------------------
aid<-1
time_check<-data.frame("aId"=aid,"activity"="Load Model Files and R Packages","time"=Sys.time())

#Load external files and data
library(compiler)  #allows us to pre-compile functions for speed improvements using "cmpfun"
library(readxl)    #for reading Excel workbooks (.xlsx)
library(plyr)      #fast array computations
library(abind)     #for piecing together arrays
library(truncnorm) #For truncated normal used in ERR
library(tmvtnorm)  #for truncated multivariate normal
library(diagram)   #for creating transition diagram
library(ff)        #Allows for larger transition matrices
library(DBI)       #database interface for SQL server connections (replaces RODBCext)
library(odbc)      #ODBC driver for DBI
library(tidyverse) #format data
#library(data.table) #Added to test format and export to SQL database

#load version-controlled RDPM support files (these should be in the same directory as RDPM)
#Echo output to workspace so the code is saved in the log file.
source("TransitionProb.r",echo=TRUE,max.deparse.length=1E100, keep.source=TRUE)#Functions that create transition matrix from transition probability definitions in specification file 
source("PathWalk.r",echo=TRUE,max.deparse.length=1E100, keep.source=TRUE) # PathWalk function
source("OutputModule.r",echo=TRUE,max.deparse.length=1E100, keep.source=TRUE)# Output functions
source("DPMPlot.r",echo=TRUE,max.deparse.length=1E100, keep.source=TRUE) # For plotting transition diagrams


#------------------------------------------------------------------------------
#Set RDPM Parameters
#------------------------------------------------------------------------------
aid<-aid+1
time_check<-rbind(time_check,data.frame("aId"=aid, "activity"="Set RDPM Parameters","time"=Sys.time()))

#Check for desktop or server mode
if(exists("Server"))
    {if(Server) {Desktop<-FALSE}  else {Desktop<-TRUE}
    }  else {Desktop<-TRUE
    }

#In desktop applications, RDPM will look to a local INIT.xlsx file to get the startup arguments.
if (Desktop){
  Args<-read_excel("INIT.xlsx", sheet='INIT', skip=1)
  names(Args)<-c("Name","Value","Description")
  }  else {
  Args<-Paths #Paths created from SQL db using StartSQL.r code
  }



#check for file path errors
Args$Value<-gsub('\\','/',Args$Value, fixed=TRUE)


ModSpecPath<-as.character(Args$Value[Args$Name=="ModSpecPath"]) #FULL file path to the directory holding Model Specification Excel workbook(s),e.g.: "C:/Users/user/Desktop/PROJECTS/rDPM/Builds/ACTIVE/"
ResultsTo<-as.character(Args$Value[Args$Name=="ResultsTo"]) #FULL file path to location where log file, summary stats, and iteration data are to be saved. E.g.: "C:/Users/user/Desktop/PROJECTS/rDPM/Builds/TEST"
BetaPath<-as.character(Args$Value[Args$Name=="BetaPath"]) #FULL file path to location where current versions of the Betas are saved, e.g.: "C:/Users/user/Desktop/PROJECTS/rDPM/Builds/ACTIVE/"
MortPath<-as.character(Args$Value[Args$Name=="MortPath"]) #FULL file path to location where the motality functions are saved, e.g.: "C:/Users/user/Desktop/PROJECTS/rDPM/Builds/ACTIVE/"

#Check if we need to run in "Batch" mode (relevant in Desktop version only) Web version will be run in batch from outside the R environment
ModSpecFile<-as.character(Args$Value[Args$Name=="ModSpecFile"]) #File name (w/o extension...it will be added automatically) of the Model Specification Excel workbookE.g.: "JOBID01010"

if(Desktop) {
ModelList<-list()#this list will hold the various models to run (will only contain 1 model if not in BATCH mode) .

if(ModSpecFile == "BATCH"){#if user specificed batch mode
  
  Models<-read_excel("INIT.xlsx", sheet='BATCH')[[1]] #read in "BATCH" tab
  nMods<-length(Models)#get a count of the number of models 
  for (i in 1:nMods) {
    ModelList[length(ModelList)+1]<-paste0(Models[i],".xlsx") #create list of filenames.
  }
} else {
    nMods<-1  #indicate to RDPM that we only have one model to run
    ModelList[length(ModelList)+1]<-paste0(ModSpecFile,".xlsx") #add single model to model list

}

} else {
  JOBID<-as.character(rINIT$Value[rINIT$Name=='JobID'])
  ModelList<-JOBID
  nMods<-1
  } #SQL: always set number of models in server mode to 1
   

#-----------------------------------------------------------------------------
#Run RDPM model(s) with echo to sinkfile
#-----------------------------------------------------------------------------
aid<-aid+1
time_check<-rbind(time_check,data.frame("aId"=aid, "activity"="Run RDPM model","time"=Sys.time()))

sink()
for (BatchNum in 1:nMods){

  #Set up a sink for R ouput for this model run [this file will be converted into the .log file at the end of the run and moved to the ResultsTo folder]
  if(Desktop) {   #SQL: for server we should consider adding the jobID to the sinkfile name in case more than one job is running at the same time
  sinkfile<-"RDPMoutput.out"
  sink(file=sinkfile,type="output")
  print(log_capture)
  ModSpecFile<-ModelList[[BatchNum]] #set model to current ModSpecFile
  } else {    #Update once linking up to SQL server
  sinkfile<-paste("RDPMoutput_",JOBID,".out",sep='')
  sink(file=sinkfile,type="output")
  print(log_capture)
  ModSpecFile<-ModelList[[BatchNum]] #set model to current ModSpecFile
  }

  aid<-aid+1
  time_check<-rbind(time_check,data.frame("aId"=aid, "activity"="Run RDPM.r","time"=Sys.time()))
  
  source('RDPM.r',echo=TRUE,max.deparse.length=1E100, keep.source=TRUE) #run RDPM.r code
  
  sink() #close sink file
  
  if(Desktop) {
  current_filepath <- paste0(SinkPath,"/RDPMoutput.out") #get filepath of sink file
  new_filepath <- paste0(ResultsTo,"/",Filename,".log") #rename sink file to a .log file, and set new log file location
  file.rename(from=current_filepath,to=new_filepath) #move log file to new location
  } else {    #might add code in server mode to insert log file into SQL db
  current_filepath <- paste0(SinkPath,"/RDPMoutput_",JOBID,".out") #get filepath of sink file
  new_filepath <- paste0(ResultsTo,"/",Filename,".log") #rename sink file to a .log file, and set new log file location
  file.rename(from=current_filepath,to=new_filepath) #move log file to new location
  }
  #Memory Cleanup
  aid<-aid+1
  time_check<-rbind(time_check,data.frame("aId"=aid, "activity"="Memory Cleanup","time"=Sys.time()))
  
  rm(randTM)
  #rm(TM_BCp)
  #rm(TM_CFp)
  #rm(Tr)
  #sort( sapply(ls(),function(x){object.size(get(x))}))
  aid<-aid+1
  time_check<-rbind(time_check,data.frame("aId"=aid, "activity"="End","time"=Sys.time()))
  if(!Desktop) {  # JOBID and conn_str are only defined in server mode
  time_check$JOBID<-JOBID
  #sqlSave(conn_str,time_check,tablename='LBTempTableTimeCheck',rownames=FALSE,append=TRUE) #Append Time Check to SQL table
  dbWriteTable(conn_str,name='LBTempTableTimeCheck',value=time_check,rownames=FALSE,append=TRUE) #Append Time Check to SQL table
  }
}

#-----------------------------------------------------------------------------
#END StartRDPM
#-----------------------------------------------------------------------------





