# load data for job scheduling problem
source("readInstance.R")

# set seed for reproducability of stochastic process
set.seed(12345)

# initialize + create empty vectors of predefined length nJobs
nRuns = 100 
bestJobOrder = rep(0, nJobs)
bestCompletionTimes = rep(0, nJobs)
bestFinesAct = rep(0, nJobs)
countImp = 0
improvement = rep(0,1)

# run algoritm
for (runs in 1:nRuns){
  # start at the first node + create empty vectors of lenght nJobs
  completed = rep(FALSE, nJobs)
  JobOrder = rep(0, nJobs)
  FinesAct = rep(0, nJobs)
  
  # keep track of the total completion time of jobs
  CompletionTime = 0
  CompletionTimes = rep(0, nJobs)
  
  # start single extension run
  for(iter in 1:nJobs){
    #browser()
    candidates = rep(0,1)
    CandidateFines = rep(0,1)
    count = 0
    
    for(i in 1:nJobs){
      if(!completed[i]){
        
        #check if precedences met
        if(sum(precedences[i,]) == 0 || 
           sum(precedences[i,]) >= 1 && which(precedences[i,] == 1) %in% JobOrder){
          count = count + 1
          candidates[count] = i
          CandidateFines[count] = ((CompletionTime + durations[i]) - deadlines[i]) * fines[i]
        }
      }
    }
    
    # randomly select candidate from 3 top candidates
    if(length(candidates) >= 3){
      rcl = candidates[order(CandidateFines, decreasing = TRUE)[1:3]]
      bestNode = sample(rcl, 1)
    }
   
    # if than 2 left, randomly select candidate from 2 top candidates
    else if (length(candidates) == 2){
      rcl = candidates[order(CandidateFines, decreasing = TRUE)[1:2]]
      bestNode = sample(rcl, 1)
    }
    
    # if 1 left, select that candidate
    else{ 
      rcl = candidates[order(CandidateFines, decreasing = TRUE)[1]]
      bestNode = rcl
    }
    
    # Update the current node
    bestFine = CandidateFines[which(candidates == bestNode)]
    currentNode = bestNode
    CompletionTime = CompletionTime + durations[bestNode]
    CompletionTimes[iter] = CompletionTime
    
    if (bestFine >= 0){
      FinesAct[iter] = bestFine
      }
    
    completed[bestNode] = TRUE
    JobOrder[iter] = bestNode
  }
  
  if(runs == 1 || lowestFine > sum(FinesAct)){
  lowestFine = sum(FinesAct)
  bestJobOrder = JobOrder
  bestCompletionTimes = CompletionTimes
  bestFinesAct = FinesAct
  
  # set improvement tracker
  countImp = countImp + 1
  improvement[countImp] = lowestFine
  run[countImp] = runs
  }
}

# print and plot results
cat("Total fine amount: ", lowestFine, "\n")
plot(run,improvement, main = "Measured improvements in total fine")
