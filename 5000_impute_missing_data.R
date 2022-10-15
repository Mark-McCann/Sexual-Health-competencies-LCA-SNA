rm(list = ls())

#################
#               #
#      Name     #
#               #
#################

# Mark McCann developed this script


#############
#  Purpose  #
#############

##Multiple imputation using indegree attributes

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

library(foreign)
library(network)
library(dplyr)
library(mice)
require(network)

#########################
#                       #
#     Load functions    #
#                       #
#########################

source('T:/projects/stash_trial/09 STASH SNA/DataAnalysis/Syntax/+MELNET+/combineLists.R')

#########################
#                       #
#  Main body of script  #
#                       #
#########################

load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control_net_dataset.rdata")
load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline_net_dataset.rdata")

###Create a flat file with all network data
control.flat.file  <- control.net.dataset[[1]]
baseline.flat.file <- baseline.net.dataset[[1]]
for (i in 2:6) {
  control.flat.file  <- rbind(control.flat.file,control.net.dataset[[i]])
  baseline.flat.file <- rbind(baseline.flat.file,baseline.net.dataset[[i]])
  
}

##Create a control dummy variable
baseline.flat.file$control <- 0
control.flat.file$control  <- 1


###Ids aren't unique across sweeps, add 1000 to baseline IDs
control.flat.file$id <- as.numeric(as.character(control.flat.file$id))
baseline.flat.file$id <- as.numeric(as.character(baseline.flat.file$id))
baseline.flat.file$id <- baseline.flat.file$id + 1000

####Add an empty perception var to the baseline schools
baseline.flat.file$sex.percep.var <- NA
baseline.flat.file <- baseline.flat.file[, c(1:16,26,17:25)] 

str(baseline.flat.file)
str(control.flat.file)
###Create the full attributes file
full.attributes <- rbind(baseline.flat.file,control.flat.file)

full.attributes <- full.attributes[, c(1,2,26,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25)]

colnames(full.attributes)

#View(full.attributes)

###Change variables to appropriate format for mice
summary(full.attributes)
str(full.attributes)

full.attributes$gender                <- as.factor(full.attributes$gender)
full.attributes$school.id             <- as.factor(full.attributes$school.id)
full.attributes$know.var              <- as.numeric(as.character(full.attributes$know.var)) 
full.attributes$att.var               <- as.numeric(as.character(full.attributes$att.var)) 
full.attributes$conf.var              <- as.numeric(as.character(full.attributes$conf.var)) 
full.attributes$std.know              <- as.numeric(as.character(full.attributes$std.know)) 
full.attributes$std.att               <- as.numeric(as.character(full.attributes$std.att)) 
full.attributes$std.conf              <- as.numeric(as.character(full.attributes$std.conf)) 
full.attributes$scale.var             <- as.numeric(as.character(full.attributes$scale.var)) 
full.attributes$sex.var               <- as.factor(ifelse(full.attributes$sex.var =="None", 0,
                                                ifelse(full.attributes$sex.var =="Some experience", 1,
                                                        ifelse(full.attributes$sex.var =="Experience", 2,NA))))
                                                             



full.attributes$talk.var               <- as.factor(ifelse(full.attributes$talk.var =="None", 0,
                                                          ifelse(full.attributes$talk.var =="Some experience", 1,
                                                                 ifelse(full.attributes$talk.var =="Oral", 2,
                                                                        ifelse(full.attributes$talk.var =="Vaginal", 3,NA)))))



full.attributes$know.int             <- as.numeric(as.character(full.attributes$know.int)) 
full.attributes$conf.int             <- as.numeric(as.character(full.attributes$conf.int)) 
full.attributes$att.int             <- as.numeric(as.character(full.attributes$att.int)) 
full.attributes$sex.active             <- as.factor(as.character(full.attributes$sex.active)) 
full.attributes$sex.percep.var         <- as.factor(as.character(full.attributes$sex.percep.var)) 
full.attributes$outschool.var       <- as.numeric(as.character(full.attributes$outschool.var)) 
full.attributes$mean.nom.ftalk        <- as.numeric(as.character(full.attributes$mean.nom.ftalk)) 
full.attributes$mean.nom.ftimein      <- as.numeric(as.character(full.attributes$mean.nom.ftimein)) 
full.attributes$mean.nom.ftimeonline  <- as.numeric(as.character(full.attributes$mean.nom.ftimeonline)) 
full.attributes$mean.nom.ftimeout     <- as.numeric(as.character(full.attributes$mean.nom.ftimeout)) 
full.attributes$mean.nom.sex          <- as.numeric(as.character(full.attributes$mean.nom.sex)) 
full.attributes$indegree              <- as.numeric(as.character(full.attributes$indegree)) 
full.attributes$outdegree             <- as.numeric(as.character(full.attributes$outdegree)) 

summary(full.attributes)
full.attributes$na_count <- apply(full.attributes[,c("gender","know.var",
                                                     "att.var","conf.var",
                                                     "scale.var","talk.var")], 1, function(x) sum(is.na(x)))


#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
#Look at pattern of missingness
md.pattern(full.attributes)

#Does the missing pattern of sex var depend on gender
miss_sex <- is.na(full.attributes$sex.var)


#   #    #     #   #  IMPUTE  #     #     #     #     #     #

quickpred(full.attributes, 
          mincor = 0.1,
          include = c('gender',
                      'school.id',
                      'control'),
          exclude = c("id","na_count","know.var",
                      "att.var","conf.var", "sex.active")
          #                                                  defaultMethod = c()
)

######Corr below 0.1 
imputed <- mice(full.attributes, pred = quickpred(full.attributes, 
                                                  mincor = 0.1,
                                                  include = c('gender',
                                                              'school.id',
                                                              'control'),
                                                  exclude = c("id","na_count","know.var",
                                                              "att.var","conf.var","sex.active","sex.percep.var"),
                                                  ),
                                                  m=20,seed=123)

# What method was used?
imputed$meth


# for what the default is, see "?mice": it is predictive mean matching.

# check out  imputed values
imputed$imp$gender

#compare imputed to observed with missingness
summary(imputed$imp$gender)
summary(full.attributes$gender) 

summary(full.attributes)

summary(imputed$imp$scale.var)
summary(full.attributes$scale.var) 

summary(imputed$imp$std.know)
summary(full.attributes$std.know) 

summary(imputed$imp$know.int)
summary(full.attributes$know.int) 

summary(imputed$imp$std.conf)
summary(full.attributes$std.conf) 

summary(imputed$imp$std.att)
summary(full.attributes$std.att) 

summary(imputed$imp$std.sex)
summary(full.attributes$std.sex) 

summary(imputed$imp$outschool.var)
summary(full.attributes$outschool.var) 

summary(imputed$imp$sex.percep.var)
summary(full.attributes$sex.percep.var) 


#Look at which variables are used to predict what
imputed$predictorMatrix

#If need to do more iterations to check convergence
#imp40 <- mice.mids(imputed, maxit=40, print=F)
#plot(imp40)

# Checking convergence of imputation model
#plot(imputed)
#densityplot(imputed, scales = list(relation = "free", layout = c(5, 1)))
#densityplot(imputed, ~conf.var|.imp)

#densityplot(imp40, scales = list(relation = "free", layout = c(5, 1)))
#densityplot(imp40, ~conf.var|.imp)

# Plot using lattice
#par( mfrow = c(1, 1) )
#stripplot(imputed, pch = 20, cex = 1.2)
#stripplot(imputed)
#xyplot(imputed, conf.var ~ sex3.var | .imp, pch = 20, cex = 1.4)

#stripplot(imp40, pch = 20, cex = 1.2)
#stripplot(imp40)
#xyplot(imp40, conf.var ~ sex3.var | .imp, pch = 20, cex = 1.4)


all.imputed <- list()
for (i in 1:imputed$m){
all.imputed[[i]] <- complete(imputed, i)
}

for (i in 1:length(all.imputed)) {
all.imputed[[i]][all.imputed[[i]]$control==0,]$id <- all.imputed[[i]][all.imputed[[i]]$control==0,]$id - 1000
}

control.imputed  <- lapply(all.imputed, function(x) filter(x, control == 1) )
baseline.imputed <- lapply(all.imputed, function(x) filter(x, control == 0) )


save(control.imputed, file = "T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control.imputed.rdata")
save(baseline.imputed, file = "T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline.imputed.rdata")

