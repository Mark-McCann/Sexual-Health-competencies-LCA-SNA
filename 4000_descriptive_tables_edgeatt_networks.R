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
desctable <- data.frame(matrix(NA, nc = 14, nr = 18))

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
desctable[10,1] <- "Sexually active"
desctable[11,1] <- "Knowledge"
desctable[12,1] <- "Missing Knowledge"
desctable[13,1] <- "Attitudes"
desctable[14,1] <- "Missing Attitudes"
desctable[15,1] <- "Confidence"
desctable[16,1] <- "Missing confidence"
desctable[17,1] <- "Outside school friends"
desctable[18,1] <- "Non-responding pupils"


#Vectors  to create grand total
grand.total <- 0

vec.total.boys   <- 0
vec.total.girls  <- 0
vec.total.trans  <- 0
vec.gender.miss  <- 0
vec.sex.miss     <- 0
vec.inact.miss   <- 0
vec.nosex.miss   <- 0
vec.active.miss <- 0 

for (i in 1:12){

descnet <- net[[i]]
column <- i + 1
know   <- as.numeric(get.vertex.attribute(descnet, "know.var"))
att    <- as.numeric(get.vertex.attribute(descnet, "att.var"))
conf   <- as.numeric(get.vertex.attribute(descnet, "conf.var"))
gender <- as.numeric(get.vertex.attribute(descnet, "gender"))
sex    <- get.vertex.attribute(descnet, "sex.var")
outfrn <- as.numeric(get.vertex.attribute(descnet, "outschool.var"))

tot.in.network <- descnet$gal$n
grand.total <- c(grand.total,tot.in.network)

###Fill in table values
desctable[1,column] <- tot.in.network
#Gender
desctable[3,column] <- paste0(table(gender, useNA = "always")[1]      ," (", round((table(gender, useNA = "always")[1] / tot.in.network) *100 ,1) ,")") 
vec.total.boys <- c(vec.total.boys, table(gender, useNA = "always")[1]) 
desctable[4,column] <- paste0(table(gender, useNA = "always")[2]      ," (", round((table(gender, useNA = "always")[2] / tot.in.network) *100 ,1) ,")") 
vec.total.girls <- c(vec.total.girls, table(gender, useNA = "always")[2]) 

desctable[5,column] <- paste0(table(gender, useNA = "always")[3]      ," (", round((table(gender, useNA = "always")[3] / tot.in.network) *100 ,1) ,")") 

###If no trans students in the school, replace the above column with zero
if(length(table(gender, useNA = "always")) ==3) desctable[5,column] <- "0 (0)"
vec.total.trans <- c(vec.total.trans, ifelse(length(table(gender, useNA = "always")) ==3, 0, table(gender, useNA = "always")[3])) 


desctable[6,column] <- paste0(tail(table(gender, useNA = "always"),1) ," (", round((tail(table(gender, useNA = "always"),1) / tot.in.network) *100 ,1) ,")") 
vec.gender.miss <- c(vec.gender.miss, tail(table(gender, useNA = "always"),1))

#Sexual activity
desctable[7,column] <- paste0(table(sex, useNA = "always")[4]      ," (", round((table(sex, useNA = "always")[4] / tot.in.network) *100 ,1) ,")") 
vec.sex.miss <- c(vec.sex.miss, tail(table(sex, useNA = "always"),1))

desctable[10,column] <- paste0(table(sex, useNA = "always")[1]      ," (", round((table(sex, useNA = "always")[1] / tot.in.network) *100 ,1) ,")") 
vec.active.miss <- c(vec.active.miss, table(sex, useNA = "always")[1])

desctable[8,column]  <- paste0(table(sex, useNA = "always")[2]      ," (", round((table(sex, useNA = "always")[2] / tot.in.network) *100 ,1) ,")") 
vec.inact.miss <- c(vec.inact.miss, table(sex, useNA = "always")[2])

desctable[9,column]  <- paste0(table(sex, useNA = "always")[3]      ," (", round((table(sex, useNA = "always")[3] / tot.in.network) *100 ,1) ,")") 
vec.nosex.miss <- c(vec.nosex.miss, table(sex, useNA = "always")[3])

#Knowledge
desctable[11,column]   <- paste0(round(mean(know, na.rm = T),2)                   ," (",round(sd(know, na.rm = T),2),")")
desctable[12,column]   <- paste0(tail(table(know, useNA = "always"),1) ," (", round((tail(table(know, useNA = "always"),1) / tot.in.network) *100 ,1) ,")") 


#Attitudes 
desctable[13,column] <-  paste0(round(mean(att, na.rm = T),2)                   ," (",round(sd(att, na.rm = T),2),")") 
desctable[14,column] <-  paste0(tail(table(att, useNA = "always"),1) ," (", round((tail(table(att, useNA = "always"),1) / tot.in.network) *100 ,1) ,")") 

#Confidence 
desctable[15,column] <- paste0(round(mean(conf, na.rm = T),2)                   ," (",round(sd(conf, na.rm = T),2),")")
desctable[16,column] <- paste0(tail(table(conf, useNA = "always"),1) ," (", round((tail(table(conf, useNA = "always"),1) / tot.in.network) *100 ,1) ,")")
#Outside school friends
desctable[17,column] <- paste0(round(mean(outfrn, na.rm = T),2)                   ," (",round(sd(outfrn, na.rm = T),2),")")

}



###Look at sex.percep.var

sex.percep.descs <- data.frame(matrix(NA, nc = 8, nr = 5))
colnames(sex.percep.descs) <- c("Variable",
                         "Net1", "Net2", "Net3", "Net4",
                         "Net5", "Net6","Total")

###add rows with labels here
sex.percep.descs[1,1] <- "Pupils"
sex.percep.descs[2,1] <- "Perception peer sex"
sex.percep.descs[3,1] <- "None"
sex.percep.descs[4,1] <- "Some"
sex.percep.descs[5,1] <- "Most"
sex.percep.descs[6,1] <- "All"
sex.percep.descs[7,1] <- "Missing"



for (i in 1:6){
  
  descnet <- control.full.network.with.missing[[i]]
  column <- i + 1
  percep <- as.numeric(get.vertex.attribute(descnet, "sex_percep.var"))
  
  tot.in.network <- descnet$gal$n
  ###Fill in table values
  sex.percep.descs[1,column] <- tot.in.network
  #Percep
  sex.percep.descs[3,column] <- paste0(table(percep, useNA = "always")[1]      ," (", round((table(percep, useNA = "always")[1] / tot.in.network) *100 ,1) ,")") 
  sex.percep.descs[4,column] <- paste0(table(percep, useNA = "always")[2]      ," (", round((table(percep, useNA = "always")[2] / tot.in.network) *100 ,1) ,")") 
  sex.percep.descs[5,column] <- paste0(table(percep, useNA = "always")[3]      ," (", round((table(percep, useNA = "always")[3] / tot.in.network) *100 ,1) ,")") 
  sex.percep.descs[6,column] <- paste0(table(percep, useNA = "always")[4]      ," (", round((table(percep, useNA = "always")[4] / tot.in.network) *100 ,1) ,")") 
  sex.percep.descs[7,column] <- paste0(table(percep, useNA = "always")[5]      ," (", round((table(percep, useNA = "always")[5] / tot.in.network) *100 ,1) ,")") 
  
}

setwd("T:/projects/stash_trial/09 STASH SNA/DisseminationAndImpact/Manuscripts_Papers/Soc Sex Dev Paper/")

write.csv(sex.percep.descs, file = "Control school sex perception descriptives.csv")




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
b.net.desc.df$sex.percep.var <- NA


b.net.desc.df <- b.net.desc.df[,colnames(c.net.desc.df)]


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



#####################################################
##               Total columns                     ##


###Take total sample from the tables, as this includes the not present



desctable[1,14] <- sum(grand.total)
total.sample <- sum(grand.total)


total.boys  <- sum(vec.total.boys)
total.girls <- sum(vec.total.girls)
total.trans <- sum(vec.total.trans)
gender.miss <- sum(vec.gender.miss)
sex.miss    <- sum(vec.sex.miss)
inact.miss  <- sum(vec.inact.miss)
nosex.miss  <- sum(vec.nosex.miss)
active.miss <- sum(vec.active.miss)

totcol <- function(input = NULL){
  x <- paste0(input," (", round((input / total.sample * 100),2),")")
  return(x)
}


desctable[1,14] <- total.sample

desctable[3,14] <- totcol(total.boys)
desctable[4,14] <- totcol(total.girls)
desctable[5,14] <- totcol(total.trans)
desctable[6,14] <- totcol(gender.miss)
desctable[7,14] <- totcol(sex.miss)
desctable[8,14] <- totcol(inact.miss)
desctable[9,14] <- totcol(nosex.miss)
desctable[10,14] <- totcol(active.miss)

know.missing <- sum( as.numeric(substr(desctable[12,2:13], 1,2)) )
att.missing  <- sum( as.numeric(substr(desctable[14,2:13], 1,2)) )
conf.missing <- sum( as.numeric(substr(desctable[16,2:13], 1,2)) )


paste0("Table 1 knowledge mean (se): ",(round( mean(net.desc.df$know.var, na.rm = T) , 2)),
       " (",round( sd(net.desc.df$know.var, na.rm = T) , 2),")")


desctable[11,14] <- paste0(round( mean(net.desc.df$know.var, na.rm = T) , 2),
                           " (",round( sd(net.desc.df$know.var, na.rm = T) , 2),")")


paste0("Table 1 knowledge missing: ",know.missing," (",
       round(know.missing/total.sample,2),")")

desctable[12,14] <- paste0(know.missing," (",
                           round(know.missing/total.sample,2),")")



paste0("Table 1 att mean (se): ",(round( mean(net.desc.df$att.var, na.rm = T) , 2)),
       " (",round( sd(net.desc.df$att.var, na.rm = T) , 2),")")

desctable[13,14] <- paste0(round( mean(net.desc.df$att.var, na.rm = T) , 2),
                           " (",round( sd(net.desc.df$att.var, na.rm = T) , 2),")")

paste0("Table 1 att missing: ",att.missing," (",round(att.missing/total.sample,2),")")

desctable[14,14] <- paste0(att.missing," (",round(att.missing/total.sample,2),")")
                           
paste0("Table 1 conf mean (se): ",(round( mean(net.desc.df$conf.var, na.rm = T) , 2))," (",round( sd(net.desc.df$conf.var, na.rm = T) , 2),")")

desctable[15,14] <- paste0(round( mean(net.desc.df$conf.var, na.rm = T) , 2)," (",round( sd(net.desc.df$conf.var, na.rm = T) , 2),")")

paste0("Table 1 conf missing: ",conf.missing," (",round(conf.missing/total.sample,2),")")

desctable[16,14] <- paste0(conf.missing," (",round(conf.missing/total.sample,2),")")

grand.total <- grand.total[2:13]


for (i in 1:12){
  col <- i + 1
  desctable[18,col] <- paste0(missing[[i]]," (", round(missing[[i]] / grand.total[[i]] * 100,2),")")
}

desctable[18,14] <-  paste0(sum(missing)," (", round(sum(missing) / total.sample * 100,2),")")

#desctable[18,13] <- paste0(sum(missing)," (", round(sum(missing) / sum(netpupils) * 100,2),")")

#desctable <- desctable[c(1:6,8:10,7,11:18),]


setwd("T:/projects/stash_trial/09 STASH SNA/DisseminationAndImpact/Manuscripts_Papers/Soc Sex Dev Paper")

write.csv(desctable, file = "Control school descriptives.csv")

