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
main.ergm <- function(dataset = NULL, savename = NULL, start_iter = NA, end_iter = NA) {
  
  #If num_iters is specified, use that number of iterations.
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
                                      + absdiff("std.know")
                                      + absdiff("std.att")
                                      + absdiff("std.conf")
                                      #Indegree terms
                                      + nodeifactor("gender")
                                      + nodeifactor("sex.var")
                                      + nodeicov("std.know")
                                      + nodeicov("std.att")
                                      + nodeicov("std.conf")
                                      #Outdegree terms
                                      # trans coef not converging - just estimate men : women / trans
                                      + nodeofactor("gender", levels = -(2:3))
                                      + nodeofactor("sex.var")
                                      + nodeocov("std.know")
                                      + nodeocov("std.att")
                                      + nodeocov("std.conf")
                                      ,
                                      directed=T, verbose = T,
                                      constraints=~bd(maxout=6),
                                      control=control.ergm(main.method=c("MCMLE"), MCMLE.maxit = 60, 
                                                           force.main=F , seed = 274,
                                                           parallel=45, parallel.type="PSOCK"),
                                      eval.loglik = F ###This cuts computation time but doesnt update screen
 # other control.ergm option - checkpoint="step_%03d.RData"
 # the saved object can be passed to another run with resume                                       
                                      
                                           )
      print(paste0("Imputation",j,"School",i,"completed"))
    }
    imputation.list[[j]] <- school.result.list
    print(paste0("All schools for imputation",j,"finished"))
    write.table(1, file = paste0("completed iteration ",j,"so far.txt"))
    #save(imputation.list, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/ergm_imp",j,"_",savename,".rdata"))
    save(imputation.list, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/main_ergm_imp",j,"_",savename,".rdata"))
    
  }
  
  return(imputation.list)
  
}
###############################################################################


#Iteration 15 wasn't converging

###############################################################################
restart.main.ergm  <- function(dataset = NULL, savename = NULL, start_iter = NA, end_iter = NA) {
  
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
                                      + absdiff("std.know")
                                      + absdiff("std.att")
                                      + absdiff("std.conf")
                                      #Indegree terms
                                      + nodeifactor("gender")
                                      + nodeifactor("sex.var")
                                      + nodeicov("std.know")
                                      + nodeicov("std.att")
                                      + nodeicov("std.conf")
                                      #Outdegree terms
                                      # trans coef not converging - just estimate men : women / trans
                                      + nodeofactor("gender", levels = -(2:3))
                                      + nodeofactor("sex.var")
                                      + nodeocov("std.know")
                                      + nodeocov("std.att")
                                      + nodeocov("std.conf")
                                      ,
                                      directed=T, verbose = T,
                                      constraints=~bd(maxout=6),
                                      control=control.ergm(main.method=c("MCMLE"), MCMLE.maxit = 60, 
                                                           force.main=F , seed = 274,
                                                           parallel=45, parallel.type="PSOCK",
                                                           init = coef(imputation.list[[j]][[i]])),
                                      eval.loglik = F ###This cuts computation time but doesnt update screen
                                      
                                      # other control.ergm option - checkpoint="step_%03d.RData"
                                      # the saved object can be passed to another run with resume                                       
                                      
      )
      print(paste0("Imputation",j,"School",i,"completed"))
    }
    imputation.list[[j]] <- school.result.list
    print(paste0("All schools for imputation",j,"finished"))
    write.table(1, file = paste0("completed iteration ",j,"so far.txt"))
    #save(imputation.list, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/ergm_imp",j,"_",savename,".rdata"))
    save(imputation.list, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/main_ergm_imp",j,"_",savename,".rdata"))
    
  }
  
  return(imputation.list)
  
}
###############################################################################





#########################
#                       #
#  Main body of script  #
#                       #
#########################

setwd("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/")


load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/ergm_data_control_imputed_5501.rdata")
load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/ergm_data_baseline_imputed_5501.rdata")


#####Run these before calling the function     
imputation.list <- list()
school.result.list <- list()

control.main.1to14 <- main.ergm(dataset = ergm.data.control.imputed, 
                                       savename = "control.main",
                                       start_iter = 1, end_iter = 14)

save(control.main.1to14 , file= "control.main.1to14.rdata")



control.main.16to20 <- main.ergm(dataset = ergm.data.control.imputed, 
                          savename = "control.main",
                          start_iter = 16, end_iter = 20)

save(control.main.16to20 , file= "control.main.16to20.rdata")


baseline.main <- main.ergm(dataset = ergm.data.baseline.imputed, 
                                                savename = "baseline.main",
                                                start_iter = 1, end_iter = 20)


save(baseline.main , file= "baseline.main.rdata")



###Iter 15 wasn't converging.

control.main.15 <- main.ergm(dataset = ergm.data.control.imputed, 
                             savename = "control.main",
                             start_iter = 15, end_iter = 15)

save(control.main.15 , file= "control.main.15.firstrun.rdata")


load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/main_ergm_imp15_control.main.rdata")

res.15.control <- restart.main.ergm(
  dataset = ergm.data.control.imputed,
  savename = "control.main.res",
  start_iter = 15,
  end_iter = 15
)

save(res.15.control , file= "control.imp15.restart.rdata")


###Knit together imputations. 



load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/main_ergm_imp14_control.main.rdata")
control.main <- imputation.list

load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.main.15.firstrun.rdata")

control.main[[15]] <- control.main.15[[15]]


load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/main_ergm_imp20_control.main.rdata")

for (i in 16:20){
  control.main[[i]] <- imputation.list[[i]]
  }

save(control.main , file= "control.main.rdata")



load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/main_ergm_imp10_baseline.main.rdata")
baseline.main <- imputation.list

load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/main_ergm_imp20_baseline.main.rdata")

for (i in 11:20){
  baseline.main[[i]] <- imputation.list[[i]]
}

save(baseline.main , file= "baseline.main.rdata")


