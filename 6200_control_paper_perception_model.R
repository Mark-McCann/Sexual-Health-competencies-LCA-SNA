rm(list = ls())

#################
#               #
#      Name     #
#               #
#################

# 6000 Ergms

# Chiara Broccatelli developed ergm script
# Mark McCann modified to loop over imputations


#############
#  Purpose  #
#############

# Running ergms across imp samples

##############
#            #
#    Notes   #
#            #
##############

# 

#########################
#                       #
#  Outstanding actions  #
#                       #
#########################


#########################
#                       #
#    Load packages      #
#                       #
#########################

require(parallel)
library(network)
library(ergm)
library(mice)

#########################
#                       #
#     Load functions    #
#                       #
#########################


###############################################################################
perception.ergm.6.iter <- function(dataset = NULL, savename = NULL, start_iter = NA, end_iter = NA) {
  
  #If iters are specified, use that number of iterations.
  # Otherwise, use the whole length of the file
  start <- ifelse(is.na(start_iter) ,  1, start_iter ) 
  end   <- ifelse(is.na(end_iter) ,  length(dataset), end_iter ) 
  
  for (j in start:end) {
    df1 <- dataset[[j]]
    for (i in c(1,2,3,5,6) ) {
      df <- df1[[i]]
      
      school.result.list[[i]] <- ergm(df ~ edges 
                                      + mutual
                                      + gwesp(0.25, fixed = T)
                                      + idegree1.5()
                                      #Homophily terms 
                                      + nodematch("sex.var", diff = T)
                                      + nodematch("gender")
                                      + nodematch("sex.percep.var")
                                      + absdiff("std.know")
                                      + absdiff("std.att")
                                      + absdiff("std.conf")
                                      #Indegree terms
                                      + nodeifactor("gender")
                                      + nodeifactor("sex.var")
                                      + nodeicov("std.know")
                                      + nodeicov("std.att")
                                      + nodeicov("std.conf")
                                      + nodeicov("sex.percep.var")
                                      #Outdegree terms
                                      # trans coef not converging - just estimate men : women / trans
                                      + nodeofactor("gender", levels = -(2:3))
                                      + nodeofactor("sex.var")
                                      + nodeocov("std.know")
                                      + nodeocov("std.att")
                                      + nodeocov("std.conf")
                                      + nodeocov("sex.percep.var")
                                      ,
                                      directed=T, verbose = T,
                                      constraints=~bd(maxout=6),
                                      control=control.ergm(main.method=c("MCMLE"), MCMLE.maxit = 6, 
                                                           force.main=F , seed = 274,
                                                           parallel=30, parallel.type="PSOCK"),
                                      eval.loglik = F ###This cuts computation time but doesnt update screen
      )
      print(paste0("Imputation",j,"School",i,"completed"))
    }
    imputation.list[[j]] <- school.result.list
    print(j)
    print(paste0("All schools for imputation",j,"finished"))
    write.table(1, file = paste0("completed perception iteration ",j,"so far.txt"))
    save(imputation.list, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/perception_ergm_imp",j,"_",savename,".rdata"))
    
  }
  
  return(imputation.list)
  
}
###############################################################################


###############################################################################
perception.ergm.restart <- function(dataset = NULL, savename = NULL, start_iter = NA, end_iter = NA, prev = NA) {
  
  #If iters are specified, use that number of iterations.
  # Otherwise, use the whole length of the file
  start <- ifelse(is.na(start_iter) ,  1, start_iter ) 
  end   <- ifelse(is.na(end_iter) ,  length(dataset), end_iter ) 
  
  for (j in start:end) {
    df1 <- dataset[[j]]
    for (i in c(1,2,3,5,6) ) {
      df <- df1[[i]]
      
      school.result.list[[i]] <- ergm(df ~ edges 
                                      + mutual
                                      + gwesp(0.25, fixed = T)
                                      + idegree1.5()
                                      #Homophily terms 
                                      + nodematch("sex.var", diff = T)
                                      + nodematch("gender")
                                      + nodematch("sex.percep.var")
                                      + absdiff("std.know")
                                      + absdiff("std.att")
                                      + absdiff("std.conf")
                                      #Indegree terms
                                      + nodeifactor("gender")
                                      + nodeifactor("sex.var")
                                      + nodeicov("std.know")
                                      + nodeicov("std.att")
                                      + nodeicov("std.conf")
                                      + nodeicov("sex.percep.var")
                                      #Outdegree terms
                                      # trans coef not converging - just estimate men : women / trans
                                      + nodeofactor("gender", levels = -(2:3))
                                      + nodeofactor("sex.var")
                                      + nodeocov("std.know")
                                      + nodeocov("std.att")
                                      + nodeocov("std.conf")
                                      + nodeocov("sex.percep.var")
                                      ,
                                      directed=T, verbose = T,
                                      constraints=~bd(maxout=6),
                                      control=control.ergm(main.method=c("MCMLE"), MCMLE.maxit = 60, 
                                                           force.main=F , seed = 274,
                                                           parallel=30, parallel.type="PSOCK",
                                                           init = coef(prev[[j]][[i]])),
                                      eval.loglik = F ###This cuts computation time but doesnt update screen
      )
      print(paste0("Imputation",j,"School",i,"completed"))
    }
    imputation.list[[j]] <- school.result.list
    print(j)
    print(paste0("All schools for imputation",j,"finished"))
    write.table(1, file = paste0("completed perception restart iteration ",j,"so far.txt"))
    save(imputation.list, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/perception_ergm_imp",j,"_",savename,".rdata"))
    
  }
  
  return(imputation.list)
  
}



#########################
#                       #
#  Main body of script  #
#                       #
#########################

setwd("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/")


load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/ergm_data_control_imputed_5501.rdata")
load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/ergm_data_baseline_imputed_5501.rdata")




#####Run these before calling the run.imputed.ergms function     
imputation.list <- list()
school.result.list <- list()

control.perception.start <- perception.ergm.6.iter(dataset = ergm.data.control.imputed, 
                                       savename = "control.perception",
                                       start_iter = 1, end_iter = 20)

save(control.perception.start , file= "control.perception.start.rdata")


control.perception.restart <- perception.ergm.restart(dataset = ergm.data.control.imputed, 
                                                   savename = "control.perception.restart",
                                                   start_iter = 1, end_iter = 20,
                                                   prev = control.perception.start)


control.perception.restart <- imputation.list

save(control.perception.restart , file= "control.perception.rdata")


control.perception.restart[[1]]

###Perception question wasn't included in baseline so not run on baseline schools.
#   Nb Baseline data was included in multiple imputation


# #####Code to patch together iterations that stalled  

# load the temp files
load("perception_ergm_imp20_control.perception.rdata")
cp_11to20 <- imputation.list

load("perception_ergm_imp10_control.perception.rdata")
cp_10 <- imputation.list

load("perception_ergm_imp9_control.perception.rdata")
cp_1to9 <- imputation.list

##Load the temp files into a single list object
control.perception <- list()
for (i in 1:9){
control.perception[[i]] <- cp_1to9[[i]]
}
control.perception[[10]] <- cp_10[[10]]
for (i in 11:20){
  control.perception[[i]] <- cp_11to20[[i]]
}


save(control.perception , file= "control.perception.rdata")

