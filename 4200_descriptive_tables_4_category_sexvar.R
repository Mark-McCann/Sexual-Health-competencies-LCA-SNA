rm(list = ls())

#################
#               #
#      Name     #
#               #
#################

# 4000 Descriptive tables

# Mark McCann developed this script
#    3500 latent class sex items also calculated some values for the tables

#############
#  Purpose  #
#############

##############
#            #
#    Notes   #
#            #
##############

# this is the version of the descriptives with the 4 category sex variable

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


load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline_full_network_with_missing.rdata")
load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control_full_network_with_missing.rdata")



net <- combine.lists(control.full.network.with.missing,baseline.full.network.with.missing)



###Create table to hold descriptives

###Change nr to number of rows you add 
desctable <- data.frame(matrix(NA, nc = 14, nr = 19))

colnames(desctable) <- c("Variable",
                         "Net1", "Net2", "Net3", "Net4",
                         "Net5", "Net6", "Net7", "Net8",
                         "Net9", "Net10", "Net11", "Net12",
                         "Total")

###add rows with labels here
desctable[1,1] <- "Pupils"
desctable[2,1] <- "Gender"
desctable[3,1] <- "Boy"
desctable[4,1] <- "Girl"
desctable[5,1] <- "Trans / Non-binary"
desctable[6,1] <- "Missing gender"
desctable[7,1] <- "Sexual activity missing"
desctable[8,1] <- "Inactive"
desctable[9,1] <- "Active no experience"
desctable[10,1] <- "Oral sex"
desctable[11,1] <- "Vaginal sex"
desctable[12,1] <- "Knowledge"
desctable[13,1] <- "Missing Knowledge"
desctable[14,1] <- "Attitudes"
desctable[15,1] <- "Missing Attitudes"
desctable[16,1] <- "Confidence"
desctable[17,1] <- "Missing confidence"
desctable[18,1] <- "Outside school friends"
desctable[19,1] <- "Non-responding pupils"

for (i in 1:12){


descnet <- net[[i]]
column <- i + 1
know   <- as.numeric(get.vertex.attribute(descnet, "know.var"))
att    <- as.numeric(get.vertex.attribute(descnet, "att.var"))
conf   <- as.numeric(get.vertex.attribute(descnet, "conf.var"))
gender <- as.numeric(get.vertex.attribute(descnet, "gender"))
sex    <- get.vertex.attribute(descnet, "talk.var")
outfrn <- as.numeric(get.vertex.attribute(descnet, "outschool.var"))

tot.in.network <- descnet$gal$n
###Fill in table values
desctable[1,column] <- tot.in.network
#Gender
desctable[3,column] <- paste0(table(gender, useNA = "always")[1]      ," (", round((table(gender, useNA = "always")[1] / tot.in.network) *100 ,1) ,")") 
desctable[4,column] <- paste0(table(gender, useNA = "always")[2]      ," (", round((table(gender, useNA = "always")[2] / tot.in.network) *100 ,1) ,")") 
desctable[5,column] <- paste0(table(gender, useNA = "always")[3]      ," (", round((table(gender, useNA = "always")[3] / tot.in.network) *100 ,1) ,")") 
desctable[6,column] <- paste0(tail(table(gender, useNA = "always"),1) ," (", round((tail(table(gender, useNA = "always"),1) / tot.in.network) *100 ,1) ,")") 

#Sexual activity
desctable[11,column] <- paste0(table(sex, useNA = "always")[4]      ," (", round((table(sex, useNA = "always")[4] / tot.in.network) *100 ,1) ,")") 


desctable[8,column] <- paste0(table(sex, useNA = "always")[1]      ," (", round((table(sex, useNA = "always")[1] / tot.in.network) *100 ,1) ,")") 
desctable[10,column]  <- paste0(table(sex, useNA = "always")[2]      ," (", round((table(sex, useNA = "always")[2] / tot.in.network) *100 ,1) ,")") 
desctable[9,column]  <- paste0(table(sex, useNA = "always")[3]      ," (", round((table(sex, useNA = "always")[3] / tot.in.network) *100 ,1) ,")") 

#Knowledge
desctable[12,column]   <- paste0(round(mean(know, na.rm = T),2)                   ," (",round(sd(know, na.rm = T),2),")")
desctable[13,column]   <- paste0(tail(table(know, useNA = "always"),1) ," (", round((tail(table(know, useNA = "always"),1) / tot.in.network) *100 ,1) ,")") 


#Attitudes 
desctable[14,column] <-  paste0(round(mean(att, na.rm = T),2)                   ," (",round(sd(att, na.rm = T),2),")") 
desctable[15,column] <-  paste0(tail(table(att, useNA = "always"),1) ," (", round((tail(table(att, useNA = "always"),1) / tot.in.network) *100 ,1) ,")") 

#Confidence 
desctable[16,column] <- paste0(round(mean(conf, na.rm = T),2)                   ," (",round(sd(conf, na.rm = T),2),")")
desctable[17,column] <- paste0(tail(table(conf, useNA = "always"),1) ," (", round((tail(table(conf, useNA = "always"),1) / tot.in.network) *100 ,1) ,")")
#Outside school friends
desctable[18,column] <- paste0(round(mean(outfrn, na.rm = T),2)                   ," (",round(sd(outfrn, na.rm = T),2),")")

}



######################################################################
####################Total columns and absent pupils###################
######################################################################



############Load the networks dataset. The pupil count is higher than respondent count 
load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control_net_dataset.rdata")
load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline_net_dataset.rdata")


c.net.desc.df <- rbind(control.net.dataset[[1]],
  control.net.dataset[[2]],
  control.net.dataset[[3]],
  control.net.dataset[[4]],
  control.net.dataset[[5]],
  control.net.dataset[[6]])
b.net.desc.df <- rbind(baseline.net.dataset[[1]],
  baseline.net.dataset[[2]],
  baseline.net.dataset[[3]],
  baseline.net.dataset[[4]],
  baseline.net.dataset[[5]],
  baseline.net.dataset[[6]])

c.net.desc.df$control <- 1
b.net.desc.df$control  <- 0

net.desc.df <- rbind(c.net.desc.df, b.net.desc.df)


net.desc.df$know.var <- as.numeric(as.character(net.desc.df$know.var))
net.desc.df$att.var <- as.numeric(as.character(net.desc.df$att.var))
net.desc.df$conf.var <- as.numeric(as.character(net.desc.df$conf.var))


for (i in 1:dim(net.desc.df)[2]){
  net.desc.df[,i] <- as.numeric(net.desc.df[,i])
}



count_na <- function(x) sum(is.na(x))
#View(net.desc.df)

net.desc.df$misscount <- NULL
for (i in 1:dim(net.desc.df)[1]){
net.desc.df$misscount[i] <- sum(is.na(net.desc.df[i,c(2:9)]))
}

net.desc.df$misscount

net.desc.df$nodemiss <- net.desc.df$misscount==7

missing <- rep(NA,12)
missing[1] <- table(net.desc.df$nodemiss[which(net.desc.df$control==1 & net.desc.df$school.id == 1)])[2]
missing[2] <- table(net.desc.df$nodemiss[which(net.desc.df$control==1 & net.desc.df$school.id == 2)])[2]
missing[3] <- table(net.desc.df$nodemiss[which(net.desc.df$control==1 & net.desc.df$school.id == 3)])[2]
missing[4] <- table(net.desc.df$nodemiss[which(net.desc.df$control==1 & net.desc.df$school.id == 4)])[2]
missing[5] <- table(net.desc.df$nodemiss[which(net.desc.df$control==1 & net.desc.df$school.id == 5)])[2]
missing[6] <- table(net.desc.df$nodemiss[which(net.desc.df$control==1 & net.desc.df$school.id == 6)])[2]

missing[7] <- table(net.desc.df$nodemiss[which(net.desc.df$control==0 & net.desc.df$school.id == 1)])[2]
missing[8] <- table(net.desc.df$nodemiss[which(net.desc.df$control==0 & net.desc.df$school.id == 2)])[2]
missing[9] <- table(net.desc.df$nodemiss[which(net.desc.df$control==0 & net.desc.df$school.id == 3)])[2]
missing[10] <- table(net.desc.df$nodemiss[which(net.desc.df$control==0 & net.desc.df$school.id == 4)])[2]
missing[11] <- table(net.desc.df$nodemiss[which(net.desc.df$control==0 & net.desc.df$school.id == 5)])[2]
missing[12] <- table(net.desc.df$nodemiss[which(net.desc.df$control==0 & net.desc.df$school.id == 6)])[2]



#setwd("T:/projects/stash_trial/09 STASH SNA/DisseminationAndImpact/Manuscripts_Papers/Control Schools Paper")
#write.csv(desctable, file = "Control school descriptives 4 cat sex var 3rd July 2020.csv")

setwd("T:/projects/stash_trial/09 STASH SNA/DisseminationAndImpact/Manuscripts_Papers/Soc Sex Dev Paper")
write.csv(desctable, file = "Control school descriptives 4 cat sex var.csv")





##################################################
##               Haven't updated this              ##

#####################################

#  Mixing matrices 



#setwd("\\\\192.168.0.17/stash_sna/DisseminationAndImpact/Manuscripts_Papers/Control Schools Paper")


#######CSV with mixing matrices for sex variables

###Baseline schools

mixmat.3cat <- matrix(NA,nc = 5, nr = 25)


colnames(mixmat.3cat) <- c("From", colnames(mixingmatrix(baseline.full.network.with.missing[[1]],'sex.var')) )

rowcount <- 1
for (i in c(1,2,3,5,6)){
  mixmat.3cat[rowcount,1] <- paste0("School ",i)
  mixmat.3cat[(rowcount + 1): (rowcount + 4) ,2:5] <- mixingmatrix(baseline.full.network.with.missing[[i]],'sex.var') 
  mixmat.3cat[(rowcount + 1): (rowcount + 4) ,1]   <- rownames(mixingmatrix(baseline.full.network.with.missing[[i]],'sex.var') )
  rowcount <- rowcount + 5
}


write.csv(mixmat.3cat, "Mixing Matrix for 3 category sexual activity variable Baseline Schools.csv")

#######CSV with mixing matrices for sex variables

###Control schools

mixmat.3cat <- matrix(NA,nc = 4, nr = 24)

colnames(mixmat.3cat) <- c("From", colnames(mixingmatrix(control.full.network.with.missing[[1]],'sex.var')$mat) )

rowcount <- 1
for (i in 1:6){
  mixmat.3cat[rowcount,1] <- paste0("School ",i)
  mixmat.3cat[(rowcount + 1): (rowcount + 3) ,2:4] <- mixingmatrix(control.full.network.with.missing[[i]],'sex.var')$mat 
  mixmat.3cat[(rowcount + 1): (rowcount + 3) ,1]   <- rownames(mixingmatrix(control.full.network.with.missing[[i]],'sex.var')$mat )
  rowcount <- rowcount + 4
}


write.csv(mixmat.3cat, "Mixing Matrix for 3 category sexual activity variable Control Schools.csv")

###Same for 4 category variable 

mixmat.4cat <- matrix(NA,nc = 6, nr = 36)

colnames(mixmat.4cat) <- c("From", colnames(mixingmatrix(baseline.full.network.with.missing[[1]],'talk.var')) )

rowcount <- 1
for (i in 1:6){
  mixmat.4cat[rowcount,1] <- paste0("School ",i)
  mixmat.4cat[(rowcount + 1): (rowcount + 5) ,2:6] <- mixingmatrix(baseline.full.network.with.missing[[i]],'talk.var') 
  mixmat.4cat[(rowcount + 1): (rowcount + 5) ,1]   <- rownames(mixingmatrix(baseline.full.network.with.missing[[i]],'talk.var') )
  rowcount <- rowcount + 6
}


write.csv(mixmat.4cat, "Mixing Matrix for 4 category sexual activity variable Baseline Schools.csv")



###Same for 4 category variable 

mixmat.4cat <- matrix(NA,nc = 6, nr = 36)

colnames(mixmat.4cat) <- c("From", colnames(mixingmatrix(control.full.network.with.missing[[1]],'talk.var')) )



rowcount <- 1
for (i in c(1,2,3,5,6)){
  mixmat.4cat[rowcount,1] <- paste0("School ",i)
  mixmat.4cat[(rowcount + 1): (rowcount + 5) ,2:6] <- mixingmatrix(control.full.network.with.missing[[i]],'talk.var') 
  mixmat.4cat[(rowcount + 1): (rowcount + 5) ,1]   <- rownames(mixingmatrix(control.full.network.with.missing[[i]],'talk.var') )
  rowcount <- rowcount + 6
}


write.csv(mixmat.4cat, "Mixing Matrix for 4 category sexual activity variable control Schools.csv")


