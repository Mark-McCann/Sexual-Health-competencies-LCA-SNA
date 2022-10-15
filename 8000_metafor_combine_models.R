rm(list = ls())
#################
#               #
#      Name     #
#               #
#################

# 8000 metafor combine models 

#     Mark McCann developed the script 

#############
#  Purpose  #
#############

#Metafor meta analysis of final ERGM estimates

##############
#            #
#    Notes   #
#            #
##############


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

library("metafor")

#########################
#                       #
#     Load functions    # 
#                       #
#########################


####################################################
forestplot <- function(data = NULL,
                       coefrow = NULL ,
                       variable = "",
                       varname = "",
                       conf = 95
){
  ##Create a 12 * 3 table, 12 schools in rows, coefficient, SE, and baseline/control in columns
  coef.table <- matrix(NA, nr=10, nc = 3) 
  for (i in 1:length(indata)){
    #Fill the table with varname row 1st col coef  
    coef.table[i,1] <- indata[[i]]$pool.est[which(indata[[i]]$variable==varname)]
    #Fill the table with varname row 2nd col SE  
    coef.table[i,2] <- indata[[i]]$pool.se[which(indata[[i]]$variable==varname)]
  }
  
  ###Add a baseline / control dummy var  
  coef.table[1:5,3] <- 0
  coef.table[6:10,3] <- 1
  colnames(coef.table) <- c("coef","se","control")
  coef.table <- coef.table[!is.na(coef.table[,1]),] 
  #Run meta analysis
  metareg <- rma(yi=coef.table[,1],
                 sei=coef.table[,2],
                 slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline"
                        , "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control") 
                 , method = "REML", level = conf)
  
  metareg.wave <- rma(yi=coef.table[,1],
                      sei=coef.table[,2], 
                      slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline"
                             , "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control") 
                      , method = "REML" 
                      , mods = coef.table[,3])
  print(metareg.wave)
  #Plot results
  setwd("T:/projects/stash_trial/09 STASH SNA/DisseminationAndImpact/Manuscripts_Papers/Soc Sex Dev Paper/")
  pdf(paste0(variable," forest 12 networks.pdf"))
  
  forest(metareg, 
         main = paste0("Tie probability by difference in ",variable),
         xlab = "Odds ratio: forming a tie",
         transf = exp,
         refline = 1)
  dev.off()
  return(metareg)  
}
####################################################

####################################################
perception.forestplot <- function(data = NULL,
                       coefrow = NULL ,
                       variable = "",
                       varname = "",
                       conf = 95
){
  ##Create a 5 * 2 table, 5 schools in rows, coefficient and SE in columns
  coef.table <- matrix(NA, nr=5, nc = 2) 
  for (i in 1:length(indata)){
    #Fill the table with varname row 1st col coef  
    coef.table[i,1] <- indata[[i]]$pool.est[which(indata[[i]]$variable==varname)]
    #Fill the table with varname row 2nd col SE  
    coef.table[i,2] <- indata[[i]]$pool.se[which(indata[[i]]$variable==varname)]
  }
  
  #Run meta analysis
  metareg <- rma(yi=coef.table[,1],
                 sei=coef.table[,2],
                 slab=c("Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control") 
                 , method = "REML", level = conf)
  #Plot results
  setwd("T:/projects/stash_trial/09 STASH SNA/DisseminationAndImpact/Manuscripts_Papers/Soc Sex Dev Paper/")
  pdf(paste0(variable," forest 5 networks.pdf"))
  
  forest(metareg, 
         main = paste0("Tie probability by ",variable),
         xlab = "Odds ratio: forming a tie",
         transf = exp,
         refline = 1)
  dev.off()
  return(metareg)  
  
}
####################################################

#########################
#                       #
#  Main body of script  #
#                       #
#########################

#########################
#                       #
#   Main models         #
#                       #
#########################


##################################################
###Load in the pooled imputation files for each school
indata <- list() 

setwd("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data")
filename <- list.files(pattern = "main.6000.sch*")

for (sch in 1:10) {
  load(filename[sch])
}

indata <- list()
indata[[1]] <- baseline.main.6000.sch1[[3]]
indata[[2]] <- baseline.main.6000.sch2[[3]]
##Gender 3 is NA as no nonbinary/trans in this network
#   Fix to zero to allow plot
indata[[2]][13,2:3] <- c(.001, 0.1)

indata[[3]] <- baseline.main.6000.sch3[[3]]
indata[[4]] <- baseline.main.6000.sch5[[3]]
indata[[5]] <- baseline.main.6000.sch6[[3]]
##Gender 3 is NA as no nonbinary/trans in this network
#   Fix to zero to allow plot
indata[[5]][13,2:3] <- c(.001, 0.1)


indata[[6]]  <- control.main.6000.sch1[[3]]
indata[[7]]  <- control.main.6000.sch2[[3]]
indata[[8]]  <- control.main.6000.sch3[[3]]
##Gender 3 is NA as no nonbinary/trans in this network
#   Fix to zero to allow plot
indata[[8]][13,2:3] <- c(.001, 0.1)


indata[[9]] <- control.main.6000.sch5[[3]]
indata[[10]] <- control.main.6000.sch6[[3]]

### To use forestplot
#    Variable is the name of the file and title of plot
#    Varname must match that in the indata output
#    Conf is confidence intervals for the plot 
#    It saves a pdf plot in the control schools folder
#    And outputs a metaregression with control school dummy to the console

meta.list <- list()
counter <- 1
for (i in indata[[3]]$variable){
  
  meta.list[[counter]] <- forestplot(data =  indata ,variable = paste0("Main 6000 ", i," param"), varname = i)
  counter <- counter + 1
}


#####layout of meta analysis summary table

# Variable labels 
varnames <- indata[[3]]$variable
##Intercept lci uci 


# I squared variability

meta.table <- data.frame(matrix(NA, nr = length(varnames), nc = 3))
colnames(meta.table) <- c("Variable","beta (95% CI)", "I Squared")
meta.table[,1] <- varnames
counter    <- 1


for (i in 1:length(varnames)){
  xp <-  predict(meta.list[[i]], transf = exp, digits = 4)
  meta.table[counter,2] <- paste0(round(xp$pred,2)," (",round(xp$ci.lb,2),", ",round(xp$ci.ub,2),")")
  meta.table[counter,3] <- round(meta.list[[i]]$I2,2)
  counter <- counter + 1
}

write.csv(meta.table, file = "main model - metaanalysis table 10 schools 20 imputations omit sch 4.csv")
##################################################



##################################################



#########################
#                       #
#  interaction models   #
#                       #
#########################

##################################################
###Load in the pooled imputation files for each school
indata <- list() 

setwd("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data")
filename <- list.files(pattern = "interaction.6100.sch*")

for (sch in 1:10) {
  load(filename[sch])
}

indata <- list()
indata[[1]] <- baseline.interaction.6100.sch1[[3]]
indata[[2]] <- baseline.interaction.6100.sch2[[3]]
##Gender 3 is NA as no nonbinary/trans in this network
#   Fix to zero to allow plot
indata[[2]][16,2:3] <- c(.001, 0.1)

indata[[3]] <- baseline.interaction.6100.sch3[[3]]
indata[[4]] <- baseline.interaction.6100.sch5[[3]]
indata[[5]] <- baseline.interaction.6100.sch6[[3]]
##Gender 3 is NA as no nonbinary/trans in this network
#   Fix to zero to allow plot
indata[[5]][16,2:3] <- c(.001, 0.1)


indata[[6]]  <- control.interaction.6100.sch1[[3]]
indata[[7]]  <- control.interaction.6100.sch2[[3]]
indata[[8]]  <- control.interaction.6100.sch3[[3]]
##Gender 3 is NA as no nonbinary/trans in this network
#   Fix to zero to allow plot
indata[[8]][16,2:3] <- c(.001, 0.1)


indata[[9]] <- control.interaction.6100.sch5[[3]]
indata[[10]] <- control.interaction.6100.sch6[[3]]

### To use forestplot
#    Variable is the name of the file and title of plot
#    Varname must match that in the indata output
#    Conf is confidence intervals for the plot 
#    It saves a pdf plot in the control schools folder
#    And outputs a metaregression with control school dummy to the console

meta.list <- list()
counter <- 1
for (i in indata[[3]]$variable){
  
  meta.list[[counter]] <- forestplot(data =  indata ,variable = paste0("6100 ", i," param"), varname = i)
  counter <- counter + 1
}


#####layout of meta analysis summary table

# Variable labels 
varnames <- indata[[3]]$variable
##Intercept lci uci 


# I squared variability

meta.table <- data.frame(matrix(NA, nr = length(varnames), nc = 3))
colnames(meta.table) <- c("Variable","beta (95% CI)", "I Squared")
meta.table[,1] <- varnames
counter    <- 1


for (i in 1:length(varnames)){
  xp <-  predict(meta.list[[i]], transf = exp, digits = 4)
  meta.table[counter,2] <- paste0(round(xp$pred,2)," (",round(xp$ci.lb,2),", ",round(xp$ci.ub,2),")")
  meta.table[counter,3] <- round(meta.list[[i]]$I2,2)
  counter <- counter + 1
}

write.csv(meta.table, file = "Interaction model - metaanalysis table 10 schools 20 imputations omit sch 4.csv")
##################################################


##################################################

#########################
#                       #
#   perception models   #
#                       #
#########################


###Load in the pooled imputation files for each school
indata <- list() 

setwd("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data")
filename <- list.files(pattern = "perception.6200.sch*")

for (sch in 1:5) {
  load(filename[sch])
}

indata <- list()
indata[[1]] <- control.perception.6200.sch1[[3]]
indata[[2]] <- control.perception.6200.sch2[[3]]

indata[[3]] <- control.perception.6200.sch3[[3]]
##Gender 3 is NA as no nonbinary/trans in this network
#   Fix to zero to allow plot
indata[[3]][14,2:3] <- c(.001, 0.1)

indata[[4]] <- control.perception.6200.sch5[[3]]
indata[[5]] <- control.perception.6200.sch6[[3]]

### To use forestplot
#    Variable is the name of the file and title of plot
#    Varname must match that in the indata output
#    Conf is confidence intervals for the plot 
#    It saves a pdf plot in the control schools folder
#    And outputs a metaregression with control school dummy to the console

meta.list <- list()
counter <- 1
for (i in indata[[3]]$variable){
  meta.list[[counter]] <- perception.forestplot(data =  indata ,variable = paste0("6200 perception ", i," param"), varname = i)
  counter <- counter + 1
}

#####layout of meta analysis summary table

# Variable labels 
varnames <- indata[[3]]$variable
##Intercept lci uci 

# I squared variability
meta.table <- data.frame(matrix(NA, nr = length(varnames), nc = 3))
colnames(meta.table) <- c("Variable","beta (95% CI)", "I Squared")
meta.table[,1] <- varnames
counter    <- 1


for (i in 1:length(varnames)){
  xp <-  predict(meta.list[[i]], transf = exp, digits = 4)
  meta.table[counter,2] <- paste0(round(xp$pred,2)," (",round(xp$ci.lb,2),", ",round(xp$ci.ub,2),")")
  meta.table[counter,3] <- round(meta.list[[i]]$I2,2)
  counter <- counter + 1
}

write.csv(meta.table, file = "Perception model - metaanalysis table 5 control schools 20 imputations omit sch 4.csv")

##################################################


