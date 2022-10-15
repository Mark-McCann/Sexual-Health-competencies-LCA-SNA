rm(list = ls())

#################
#               #
#      Name     #
#               #
#################

# Emily Long developed script for PaLS 
# Mark McCann modified for STASH

#############
#  Purpose  #
#############

# Pooling results of MI models in 6100, 6200, 6300

##############
#            #
#    Notes   #
#            #
##############

# interaction pool now works for all models

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

library(ergm)
library(mice)



#########################
#                       #
#     Load functions    #
#                       #
#########################
####################################### 
#   Pool results using Rubin's rules  #
#######################################


############################################################################
pool.7000.models <-
  function(input.results = NULL,
           schoolnum = NULL) {
    leng <- length(input.results)
    #Two tables for coefs and se
    mice.coef.table <-
      matrix(0,
             nc = leng + 1,
             nr = length(input.results[[1]][[1]]$coefficients))
    mice.se.table <- mice.coef.table
    #Table for results of rubin's rules pool
    mice.pooled.est     <-
      data.frame(matrix(
        NA,
        nr = length(input.results[[1]][[1]]$coefficients),
        nc = 7
      ))
    colnames(mice.pooled.est) <-
      c("variable",
        "pool.est",
        "pool.se",
        "btw.var",
        "est/se",
        "l.cl",
        "u.cl")
    #Put names in the coef table
    for (x in 1:length(input.results[[1]][[1]]$coef)) {
      mice.pooled.est[x, 1] <-
        names(input.results[[1]][[1]]$coefficients)[x]
      #    mice.coef.table[x,1] <- names(input.results[[1]][[1]]$coefficients)[x]
    }
    
    rownames(mice.coef.table) <-
      names(input.results[[1]][[1]]$coefficients)
    rownames(mice.se.table)   <-
      names(input.results[[1]][[1]]$coefficients)
    
    
    
    
    for (i in names(input.results[[1]][[1]]$coefficients)) {
      for (iterpos in 1:leng) {
        col.id <- iterpos + 1
        mice.coef.table[i, col.id] <-
          coef(input.results[[iterpos]][[schoolnum]])[i]
        mice.se.table[i, col.id] <-
          summary(input.results[[iterpos]][[schoolnum]])$asyse[i]
        
      }
    }
    
    
      mice.coef.table[, 2:leng] <- as.numeric(mice.coef.table[, 2:leng])
    
    for (i in 1:dim(mice.coef.table)[1]) {
      pooled.res <-
        pool.scalar(as.numeric(mice.coef.table[i, 2:(leng + 1)]),
                    as.numeric(mice.se.table[i, 2:(leng + 1)]),
                    k = 1)
      mice.pooled.est[i, 2] <-  round(pooled.res$qbar, 2)
      mice.pooled.est[i, 3] <-  round(pooled.res$ubar, 2)
      mice.pooled.est[i, 4] <-  round(pooled.res$b, 2)
      mice.pooled.est[i, 5] <-
        round(pooled.res$qbar / pooled.res$ubar , 2)
      mice.pooled.est[i, 6] <-
        round(pooled.res$qbar - (1.96 * pooled.res$ubar), 2)
      mice.pooled.est[i, 7] <-
        round(pooled.res$qbar + (1.96 * pooled.res$ubar), 2)
      
    }
    
    mice.array <- list(coefs = mice.coef.table,
                       ses = mice.se.table,
                       pooled.ests = mice.pooled.est)
    
    return(mice.array)
  }
############################################################################

#########################
#                       #
#  Main body of script  #
#                       #
#########################

# Rubin's pools requires an input object that has the indexing format:
#     object[[imputation]][schoolid]

#########################
#                       #
#      main models      #
#                       #
#########################

#Stitch together iterations that stalled  


setwd("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data")





####Load 6000 main models

load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.main.rdata")
load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline.main.rdata")


###########################
#     Pool results 
###########################



for (i in c(1:3,5:6)){
  assign(paste0("baseline.main.6000.sch",i), pool.7000.models(input.results = baseline.main, schoolnum = i))
  assign(paste0("control.main.6000.sch" ,i), pool.7000.models(input.results = control.main, schoolnum = i))
}



save(baseline.main.6000.sch1, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline.main.6000.sch1.rdata"))
save(baseline.main.6000.sch2, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline.main.6000.sch2.rdata"))
save(baseline.main.6000.sch3, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline.main.6000.sch3.rdata"))
save(baseline.main.6000.sch5, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline.main.6000.sch5.rdata"))
save(baseline.main.6000.sch6, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline.main.6000.sch6.rdata"))

save(control.main.6000.sch1, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.main.6000.sch1.rdata"))
save(control.main.6000.sch2, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.main.6000.sch2.rdata"))
save(control.main.6000.sch3, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.main.6000.sch3.rdata"))
save(control.main.6000.sch5, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.main.6000.sch5.rdata"))
save(control.main.6000.sch6, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.main.6000.sch6.rdata"))


#####Take model results and put into a csv for an article table
pooled.results.table <- cbind(control.main.6000.sch1[[3]]$variable,
                              ##Baseline coefs
                              paste0(baseline.main.6000.sch1[[3]]$pool.est," (",
                                     baseline.main.6000.sch1[[3]]$l.cl    ,", ",
                                     baseline.main.6000.sch1[[3]]$u.cl    ,")"),
                              paste0(baseline.main.6000.sch2[[3]]$pool.est," (",
                                     baseline.main.6000.sch2[[3]]$l.cl    ,", ",
                                     baseline.main.6000.sch2[[3]]$u.cl    ,")"),
                              paste0(baseline.main.6000.sch3[[3]]$pool.est," (",
                                     baseline.main.6000.sch3[[3]]$l.cl    ,", ",
                                     baseline.main.6000.sch3[[3]]$u.cl    ,")"),
                              paste0(baseline.main.6000.sch5[[3]]$pool.est," (",
                                     baseline.main.6000.sch5[[3]]$l.cl    ,", ",
                                     baseline.main.6000.sch5[[3]]$u.cl    ,")"),
                              paste0(baseline.main.6000.sch6[[3]]$pool.est," (",
                                     baseline.main.6000.sch6[[3]]$l.cl    ,", ",
                                     baseline.main.6000.sch6[[3]]$u.cl    ,")")
                              ,
                              ######Control schools
                              paste0(control.main.6000.sch1[[3]]$pool.est," (",
                                     control.main.6000.sch1[[3]]$l.cl    ,", ",
                                     control.main.6000.sch1[[3]]$u.cl    ,")"),
                              paste0(control.main.6000.sch2[[3]]$pool.est," (",
                                     control.main.6000.sch2[[3]]$l.cl    ,", ",
                                     control.main.6000.sch2[[3]]$u.cl    ,")"),
                              paste0(control.main.6000.sch3[[3]]$pool.est," (",
                                     control.main.6000.sch3[[3]]$l.cl    ,", ",
                                     control.main.6000.sch3[[3]]$u.cl    ,")"),
                              paste0(control.main.6000.sch5[[3]]$pool.est," (",
                                     control.main.6000.sch5[[3]]$l.cl    ,", ",
                                     control.main.6000.sch5[[3]]$u.cl    ,")"),
                              paste0(control.main.6000.sch6[[3]]$pool.est," (",
                                     control.main.6000.sch6[[3]]$l.cl    ,", ",
                                     control.main.6000.sch6[[3]]$u.cl    ,")")
)

colnames(pooled.results.table) <- c("Parameter","School 1b","School 2b","School 3b",
                                    "School 5b", "School 6b",
                                    "School 1c","School 2c","School 3c",
                                    "School 5c", "School 6c")

pooled.results.table


write.csv(pooled.results.table, file = "T:/projects/stash_trial/09 STASH SNA/DisseminationAndImpact/Manuscripts_Papers/Soc Sex Dev Paper/baseline control main pooled ergm for each school.csv")




###Files are slow to load - delete out files other than pool function to free up memory

rm(list = ls()[ls()!="pool.7000.models"] )

#########################
#                       #
#  interaction models   #
#                       #
#########################


####Load 6200 interaction models

load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.interaction.rdata")

load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline.interaction.rdata")

# ###temp while the iterations are finishing
# load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/interaction_ergm_imp17_control.interaction.rdata")
# control.interaction <- imputation.list
###temp end

for (i in c(1:3,5:6)){
  assign(paste0("baseline.interaction.6100.sch",i), pool.7000.models(input.results = baseline.interaction, schoolnum = i))
  assign(paste0("control.interaction.6100.sch" ,i), pool.7000.models(input.results = control.interaction, schoolnum = i))
}

control.interaction[[1]]

save(baseline.interaction.6100.sch1, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline.interaction.6100.sch1.rdata"))
save(baseline.interaction.6100.sch2, file = paste0("T:/projects/stash_trial/09 STASH SNA//Data/AnonymisedData/working data/baseline.interaction.6100.sch2.rdata"))
save(baseline.interaction.6100.sch3, file = paste0("T:/projects/stash_trial/09 STASH SNA//Data/AnonymisedData/working data/baseline.interaction.6100.sch3.rdata"))
#save(baseline.pooled.sch4, file = paste0("T:/projects/stash_trial/09 STASH SNA//Data/AnonymisedData/working data/control.interaction.6100.sch4.rdata"))
save(baseline.interaction.6100.sch5, file = paste0("T:/projects/stash_trial/09 STASH SNA//Data/AnonymisedData/working data/baseline.interaction.6100.sch5.rdata"))
save(baseline.interaction.6100.sch6, file = paste0("T:/projects/stash_trial/09 STASH SNA//Data/AnonymisedData/working data/baseline.interaction.6100.sch6.rdata"))

save(control.interaction.6100.sch1, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.interaction.6100.sch1.rdata"))
save(control.interaction.6100.sch2, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.interaction.6100.sch2.rdata"))
save(control.interaction.6100.sch3, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.interaction.6100.sch3.rdata"))
#save(control.pooled.sch4, file = paste0("T:/projects/stash_trial/09 STASH SNA//Data/AnonymisedData/working data/control.interaction.6100.sch4.rdata"))
save(control.interaction.6100.sch5, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.interaction.6100.sch5.rdata"))
save(control.interaction.6100.sch6, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.interaction.6100.sch6.rdata"))


#####Take model results and put into a csv for an article table
pooled.results.table <- cbind(control.interaction.6100.sch1[[3]]$variable,
                              ##Baseline coefs
                             paste0(baseline.interaction.6100.sch1[[3]]$pool.est," (",
                                    baseline.interaction.6100.sch1[[3]]$l.cl    ,", ",
                                    baseline.interaction.6100.sch1[[3]]$u.cl    ,")"),
                             paste0(baseline.interaction.6100.sch2[[3]]$pool.est," (",
                                    baseline.interaction.6100.sch2[[3]]$l.cl    ,", ",
                                    baseline.interaction.6100.sch2[[3]]$u.cl    ,")"),
                             paste0(baseline.interaction.6100.sch3[[3]]$pool.est," (",
                                    baseline.interaction.6100.sch3[[3]]$l.cl    ,", ",
                                    baseline.interaction.6100.sch3[[3]]$u.cl    ,")"),
                             #paste0(baseline.interaction.6100.sch4[[3]]$pool.est," (",
                             #       baseline.interaction.6100.sch4[[3]]$l.cl    ,", ",
                             #       baseline.interaction.6100.sch4[[3]]$u.cl    ,")"),
                             paste0(baseline.interaction.6100.sch5[[3]]$pool.est," (",
                                    baseline.interaction.6100.sch5[[3]]$l.cl    ,", ",
                                    baseline.interaction.6100.sch5[[3]]$u.cl    ,")"),
                             paste0(baseline.interaction.6100.sch6[[3]]$pool.est," (",
                                    baseline.interaction.6100.sch6[[3]]$l.cl    ,", ",
                                    baseline.interaction.6100.sch6[[3]]$u.cl    ,")")
                             ,
                              ######Control schools
                              paste0(control.interaction.6100.sch1[[3]]$pool.est," (",
                                     control.interaction.6100.sch1[[3]]$l.cl    ,", ",
                                     control.interaction.6100.sch1[[3]]$u.cl    ,")"),
                              paste0(control.interaction.6100.sch2[[3]]$pool.est," (",
                                     control.interaction.6100.sch2[[3]]$l.cl    ,", ",
                                     control.interaction.6100.sch2[[3]]$u.cl    ,")"),
                              paste0(control.interaction.6100.sch3[[3]]$pool.est," (",
                                     control.interaction.6100.sch3[[3]]$l.cl    ,", ",
                                     control.interaction.6100.sch3[[3]]$u.cl    ,")"),
                              #paste0(control.interaction.6100.sch4[[3]]$pool.est," (",
                              #       control.interaction.6100.sch4[[3]]$l.cl    ,", ",
                              #       control.interaction.6100.sch4[[3]]$u.cl    ,")"),
                              paste0(control.interaction.6100.sch5[[3]]$pool.est," (",
                                     control.interaction.6100.sch5[[3]]$l.cl    ,", ",
                                     control.interaction.6100.sch5[[3]]$u.cl    ,")"),
                              paste0(control.interaction.6100.sch6[[3]]$pool.est," (",
                                     control.interaction.6100.sch6[[3]]$l.cl    ,", ",
                                     control.interaction.6100.sch6[[3]]$u.cl    ,")")
)

colnames(pooled.results.table) <- c("Parameter","School 1b","School 2b","School 3b",
                                    "School 5b", "School 6b",
                                    "School 1c","School 2c","School 3c",
                                    "School 5c", "School 6c")

pooled.results.table


write.csv(pooled.results.table, file = "T:/projects/stash_trial/09 STASH SNA/DisseminationAndImpact/Manuscripts_Papers/Soc Sex Dev Paper/baseline control interaction pooled ergm for each school.csv")



#########################
#                       #
#  perception model     #
#                       #
#########################

load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.perception.rdata")

control.perception <- control.perception.restart


for (i in c(1:3,5:6)){
  assign(paste0("control.perception.6200.sch" ,i), pool.7000.models(input.results = control.perception, schoolnum = i))
}





save(control.perception.6200.sch1, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.perception.6200.sch1.rdata"))
save(control.perception.6200.sch2, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.perception.6200.sch2.rdata"))
save(control.perception.6200.sch3, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.perception.6200.sch3.rdata"))
#save(control.pooled.sch4, file = paste0("T:/projects/stash_trial/09 STASH SNA//Data/AnonymisedData/working data/control.perception.6200.sch4.rdata"))
save(control.perception.6200.sch5, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.perception.6200.sch5.rdata"))
save(control.perception.6200.sch6, file = paste0("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.perception.6200.sch6.rdata"))


#####Take model results and put into a csv for an article table
pooled.results.table <- cbind(control.perception.6200.sch1[[3]]$variable,
                              ######Control schools
                              paste0(control.perception.6200.sch1[[3]]$pool.est," (",
                                     control.perception.6200.sch1[[3]]$l.cl    ,", ",
                                     control.perception.6200.sch1[[3]]$u.cl    ,")"),
                              paste0(control.perception.6200.sch2[[3]]$pool.est," (",
                                     control.perception.6200.sch2[[3]]$l.cl    ,", ",
                                     control.perception.6200.sch2[[3]]$u.cl    ,")"),
                              paste0(control.perception.6200.sch3[[3]]$pool.est," (",
                                     control.perception.6200.sch3[[3]]$l.cl    ,", ",
                                     control.perception.6200.sch3[[3]]$u.cl    ,")"),
                              #paste0(control.perception.6200.sch4[[3]]$pool.est," (",
                              #       control.perception.6200.sch4[[3]]$l.cl    ,", ",
                              #       control.perception.6200.sch4[[3]]$u.cl    ,")"),
                              paste0(control.perception.6200.sch5[[3]]$pool.est," (",
                                     control.perception.6200.sch5[[3]]$l.cl    ,", ",
                                     control.perception.6200.sch5[[3]]$u.cl    ,")"),
                              paste0(control.perception.6200.sch6[[3]]$pool.est," (",
                                     control.perception.6200.sch6[[3]]$l.cl    ,", ",
                                     control.perception.6200.sch6[[3]]$u.cl    ,")")
)

colnames(pooled.results.table) <- c("Parameter",
                                    "School 1c","School 2c","School 3c",
                                    "School 5c", "School 6c")

pooled.results.table


write.csv(pooled.results.table, file = "T:/projects/stash_trial/09 STASH SNA/DisseminationAndImpact/Manuscripts_Papers/Soc Sex Dev Paper/control perception pooled ergm for each school.csv")





#############################################################################
######           Create pdfs of mcmc diagnostics and gof  #######
#############################################################################


load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.main.rdata")
load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline.main.rdata")


####Load 6200 interaction models

load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.interaction.rdata")

load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline.interaction.rdata")


load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.perception.rdata")

control.perception <- control.perception.restart



setwd("T:/projects/stash_trial/09 STASH SNA/DisseminationAndImpact/Manuscripts_Papers/Soc Sex Dev Paper")
iternums <- 1:20
for (sch in c(1:3,5:6)){
  pdf(paste0("6000_control_main_mcmc_diag_sch",sch,".pdf"))
  for (iter in iternums){    
    mcmc.diagnostics(control.main[[iter]][[sch]])
    ideg <- gof(control.main[[iter]][[sch]]~idegree)
    plot(ideg)
    odeg <- gof(control.main[[iter]][[sch]]~idegree)
    plot(odeg)
    tricen <- gof(control.main[[iter]][[sch]]~triadcensus)
    plot(tricen)
  }
  dev.off()
}

for (sch in c(1:3,5:6)){
  pdf(paste0("6000_baseline_main_mcmc_diag_sch",sch,".pdf"))
  for (iter in iternums){    
    mcmc.diagnostics(baseline.main[[iter]][[sch]])
    ideg <- gof(baseline.main[[iter]][[sch]]~idegree)
    plot(ideg)
    odeg <- gof(baseline.main[[iter]][[sch]]~idegree)
    plot(odeg)
    tricen <- gof(baseline.main[[iter]][[sch]]~triadcensus)
    plot(tricen)
  }
  dev.off()
}


#############################################################################
######           Create pdfs of mcmc diagnostics and gof  #######
#############################################################################

setwd("T:/projects/stash_trial/09 STASH SNA/DisseminationAndImpact/Manuscripts_Papers/Soc Sex Dev Paper")
iternums <- 1:20
#for (sch in 1:6){
for (sch in c(1:3,5:6)){
  pdf(paste0("6100_control_interaction_mcmc_diag_sch",sch,".pdf"))
  for (iter in iternums){    
    mcmc.diagnostics(control.interaction[[iter]][[sch]])
    ideg <- gof(control.interaction[[iter]][[sch]]~idegree)
    plot(ideg)
    odeg <- gof(control.interaction[[iter]][[sch]]~idegree)
    plot(odeg)
    tricen <- gof(control.interaction[[iter]][[sch]]~triadcensus)
    plot(tricen)
  }
  dev.off()
}

#for (sch in 1:6){
for (sch in c(1:3,5:6)){
  pdf(paste0("6100_baseline_interaction_mcmc_diag_sch",sch,".pdf"))
  for (iter in iternums){    
    mcmc.diagnostics(baseline.interaction[[iter]][[sch]])
    ideg <- gof(baseline.interaction[[iter]][[sch]]~idegree)
    plot(ideg)
    odeg <- gof(baseline.interaction[[iter]][[sch]]~idegree)
    plot(odeg)
    tricen <- gof(baseline.interaction[[iter]][[sch]]~triadcensus)
    plot(tricen)
  }
  dev.off()
}


#############################################################################
######           Create pdfs of mcmc diagnostics and gof  #######
#############################################################################

setwd("T:/projects/stash_trial/09 STASH SNA/DisseminationAndImpact/Manuscripts_Papers/Soc Sex Dev Paper")
iternums <- 1:20
#for (sch in 1:6){
for (sch in c(1:3,5:6)){
  pdf(paste0("6000_control_perception_mcmc_diag_sch",sch,".pdf"))
  for (iter in iternums){    
    mcmc.diagnostics(control.perception[[iter]][[sch]])
    ideg <- gof(control.perception[[iter]][[sch]]~idegree)
    plot(ideg)
    odeg <- gof(control.perception[[iter]][[sch]]~idegree)
    plot(odeg)
    tricen <- gof(control.perception[[iter]][[sch]]~triadcensus)
    plot(tricen)
  }
  dev.off()
}

