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
interaction.ergm <- function(dataset = NULL, savename = NULL, start_iter = NA, end_iter = NA, prev = NA) {
  
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
                                      + absdiff("know.int")
                                      + absdiff("att.int")
                                      + absdiff("conf.int")
                                      #Indegree terms
                                      + nodeifactor("gender")
                                      + nodeifactor("sex.var")
                                      + nodeicov("std.know")
                                      + nodeicov("std.att")
                                      + nodeicov("std.conf")
                                      + nodeicov("know.int")
                                      + nodeicov("att.int")
                                      + nodeicov("conf.int")
                                      #Outdegree terms
                                      # trans coef not converging - just estimate men : women / trans
                                      + nodeofactor("gender", levels = -(2:3))
                                      + nodeofactor("sex.var")
                                      + nodeocov("std.know")
                                      + nodeocov("std.att")
                                      + nodeocov("std.conf")
                                      + nodeocov("know.int")
                                      + nodeocov("att.int")
                                      + nodeocov("conf.int")
                                      ,
                                      directed=T, verbose = T,
                                      constraints=~bd(maxout=6),
                                      control=control.ergm(main.method=c("MCMLE"), MCMLE.maxit = 60, 
                                                           force.main=F , seed = 274,
                                                           parallel=50, parallel.type="PSOCK"),
                                      eval.loglik = F ###This cuts computation time but doesnt update screen
      )
      print(paste0("Imputation",j,"School",i,"completed"))
            }
    imputation.list[[j]] <- school.result.list
    print(j)
    write.table(1, file = paste0("baseline interaction completed iteration ",j,"so far.txt"))
    #save(imputation.list, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/ergm_imp",j,"_",savename,".rdata"))
    save(imputation.list, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/interaction_ergm_imp",j,"_",savename,".rdata"))
    
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



#####Run these before calling the ergm functions     
imputation.list <- list()
school.result.list <- list()

##This was run without the start end code



control.interaction <-
  interaction.ergm(
    dataset = ergm.data.control.imputed,
    savename = "control.interaction",
    start_iter = 1,
    end_iter = 20
  )

###Save failed at iter 17


control.interaction <-
  interaction.ergm(
    dataset = ergm.data.control.imputed,
    savename = "control.interaction",
    start_iter = 17,
    end_iter = 20
  )



save(control.interaction , file= "control.interaction 17 to 20.rdata")

##put 1 to 16 in with 17 to 20
load("interaction_ergm_imp16_control.interaction.rdata")
for (i in 1:16){
  control.interaction[[i]] <- imputation.list[[i]]
}
save(control.interaction , file= "control.interaction.rdata")




baseline.interaction <-
  interaction.ergm(
    dataset = ergm.data.baseline.imputed,
    savename = "baseline.interaction",
    start_iter = 1,
    end_iter = 20
  )


save(baseline.interaction , file= "baseline.interaction.rdata")


