#==============================================================================
#PATHWALK Algorithm
#Version 1.7
#Revised: Feb-13-2018
#==============================================================================

#PathWalk recursivley creates a "tree" of all possible transitions among states, as constrained by the user-supplied transition matrix. Each set of possible transitions (the set will have one element per age category) is a "path" through the "transition space".

#At each step, Pathwalk applies the mortality rate function to a vector of length Nruns to calcluate the mortality for that particular path for each run (with associated Betas and entering population fractions). Calulating the mortality for all runs simultaneously (not sequentially i.e., run by run) dramatically increases the speed of the algorithm. The vector of transition probabilities for that particular state and age category (it's a vector because each run can have a different value for this transition probability) is used to generate the vector for the next set of paths that come from the particular calling state/age combination. 

#------------------------------------------------------------------------------
#GLOBAL Dependencies (unbound variables)
#------------------------------------------------------------------------------

#An SxTxNruns array, called P, initialized to 0, that holds the fraction of the population that is in state s in S at time t in T, for run N in Nruns.

#SxSxTxNruns transition array, named Tr, which indicates the probability of a transition from state i in S to state j in S at time t in T for run N in Nruns.

#mBetas: betas for the risk function (Nruns x 7)

#pRR: Relative risk matrix (Nruns x 2) of log RRs

#The SPath argument in PathWalk is a boolean variable that PathWalk passes to the mortaility functions, telling htem to to store the betas, path, and resulting mortality for each step of each path. E.g. 1, 11, 112,1123, etc. Results are stored in two global data frames: the "BCPath" and "CFPath" data.frames.

#-------------------------------------------------------------------------------
#Variable Definitions
#-------------------------------------------------------------------------------

#Ns is the number of states
#Nt is the number of time periods
#t is the current age category index value [1...Nt]
#s is the current state index number [1...Ns]
#past is a vector holding the previous t-1 states on the trajectory
#p is a vector of length Nruns, which holds population fractions that correspond to the population ENTERING state s and time t from the trajectory "past" for each run
#mr is the mortality rate function, which maps the past state, betas, and age cagtegory width to a mortality rate (as a fraction of the population)
#wi is the (fixed) age category width

#-------------------------------------------------------------------------------
#Principle of operation: 
#-------------------------------------------------------------------------------
#Paths to a given (s,t) pair are mutually exclusive, so we can simply add fractions of the population coming from each unique (sub)-trajectory to a particular (s,t) for each run.

#===============================================================================
#===============================================================================


PathWalk<-function(t=1,s=1,past=NULL,p=1,mr,wi,ma,Ns,Nt,SPath){
     
  if(t<Nt){#If we are not at the  
    P[s,t,]<<-P[s,t,]+p #Add entering population fraction to appropariate survival matrix element.[See note in comments below the function code for discussion of "<<-" vs "<-"]
    pnext<-(1-mr(past,mBetas,wi,ma,SPath))*p
    st<-1:Ns
    pNext<-t(pnext*t(Tr[s,st,t,]))
    st<-st[pNext[,1]>0]
    if (length(st) == 0) {} else {
        for (j in 1:length(st)) { 
          i<-st[j] 
          PathWalk(t+1,i,c(past,i),pNext[i,],mr,wi,ma,Ns,Nt,SPath) #Create copies of PathWalk that perform calculations for the "next" set of possible states for this trajectory. 
        }
    }
  }else {
    P[s,t,]<<-P[s,t,]+p  #if at the final state, then do additions and stop calling pathwalk for this particular branch.
    }
}

#NOTE: The "<<" operator tells R that the variable being assigned to is "outside" of the function PathWalk. R will look for the variable "P" in the general R environment. This allows P to serve as a "global" data structure that can be accessed by all branches of the recursive algorithm. Otherwise, many copies of P would need to be coordinated, making this approach infeasible.




