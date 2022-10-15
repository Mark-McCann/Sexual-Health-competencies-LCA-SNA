rm(list = ls())
#################
#               #
#      Name     #
#               #
#################

# Chiara Broccatelli developed this script
# Mark McCann modified 


#############
#  Purpose  #
#############

# add attributes to networks


#########################
#                       #
#    Load packages      #
#                       #
#########################
library(dplyr)
library(sna)
library(network)

#########################
#                       #
#     Load functions    #
#                       #
#########################


#########################
#                       #
#  Main body of script  #
#                       #
#########################

# - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ##
#--------------------------------------------------------------------------------
#             + + +         Preparing Control and Baseline nets and attributes         + + + 
#--------------------------------------------------------------------------------
# - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ##

#===============================================================================
#                       IMPORTING NETWORKS
#===============================================================================

load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline_attributes.rdata")
load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control_attributes.rdata")

#  +  control schools  +
load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control_edge_att_networks.rdata")
#  +  baseline schools  +
load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline_edge_att_networks.rdata")

# - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ##
#-------------------------------------------------------------
# + + + IMPORTING THE ATTRIBUTES 
#-------------------------------------------------------------

# ATTRIBUTES SCHOOLS
#Control school


# let's start with control schools
c.gender <- list()
c.std.know <- list()
c.std.att <- list()
c.std.conf <- list()
c.know.var <- list()
c.att.var <- list()
c.conf.var <- list()
c.scale.var <- list()
c.info.var <- list()
c.sex.var <- list()
c.talk.var <- list()
c.outschool.var <- list()
c.schoolid.var <- list()
c.sex_percep.var <- list()

load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control_outschool_friends.rdata")

for (i in 1:6){

  link.atts <- filter(control.attributes, respondent_school == i)
  c.gender[[i]]    <- as.character(link.atts$gender[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
  c.sex.var[[i]]   <- as.numeric(link.atts$sex3.var[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
  c.scale.var[[i]] <- as.numeric(link.atts$scale.var[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
  c.talk.var[[i]]  <- as.numeric(link.atts$talk.var[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
  c.std.know[[i]]  <- as.numeric(link.atts$std.know[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
  c.std.conf[[i]]  <- as.numeric(link.atts$std.conf[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
  c.std.att[[i]]   <- as.numeric(link.atts$std.att[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])

  c.know.var[[i]]  <- as.numeric(link.atts$know.var[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
  c.conf.var[[i]]  <- as.numeric(link.atts$conf.var[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
  c.att.var[[i]]   <- as.numeric(link.atts$att.var[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])

  c.sex_percep.var[[i]]   <- as.numeric(link.atts$sex_percep[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])

  c.schoolid.var[[i]] <- as.numeric(link.atts$respondent_school[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
  c.outschool.var[[i]] <- as.numeric(control.outside.school[[i]]$outschfriends[match(network.vertex.names(control.edge.att.network[[i]]), control.outside.school[[i]]$respondent_id)])



#table(c.std.att[[1]], useNA = "ifany")
#table(c.gender[[1]], useNA = "ifany")
#table(c.sex.var[[1]], useNA = "ifany")
#table(c.scale.var[[1]], useNA = "ifany")
#table(c.talk.var[[1]], useNA = "ifany")
#table(c.std.know[[1]], useNA = "ifany")
#table(c.std.conf[[1]], useNA = "ifany")
#table(c.std.att[[1]], useNA = "ifany")
#table(c.schoolid.var[[1]], useNA = "ifany")
#table(c.outschool.var[[1]], useNA = "ifany")


 set.vertex.attribute(control.edge.att.network[[i]], "gender"   , c.gender[[i]])
 set.vertex.attribute(control.edge.att.network[[i]], "sex.var"  , c.sex.var[[i]])
 set.vertex.attribute(control.edge.att.network[[i]], "scale.var", c.scale.var[[i]])
 set.vertex.attribute(control.edge.att.network[[i]], "talk.var" , c.talk.var[[i]])
 set.vertex.attribute(control.edge.att.network[[i]], "std.know" , c.std.know[[i]])
 set.vertex.attribute(control.edge.att.network[[i]], "std.conf" , c.std.conf[[i]])
 set.vertex.attribute(control.edge.att.network[[i]], "std.att"  , c.std.att[[i]])
 set.vertex.attribute(control.edge.att.network[[i]], "know.var" , c.know.var[[i]])
 set.vertex.attribute(control.edge.att.network[[i]], "conf.var" , c.conf.var[[i]])
 set.vertex.attribute(control.edge.att.network[[i]], "att.var"  , c.att.var[[i]])
 set.vertex.attribute(control.edge.att.network[[i]], "school.id"  , c.schoolid.var[[i]])
 set.vertex.attribute(control.edge.att.network[[i]], "outschool.var"  , c.outschool.var[[i]])
 set.vertex.attribute(control.edge.att.network[[i]], "sex_percep.var"  , c.sex_percep.var[[i]])
 
}

head(link.atts)
for (i in 1:6) {
  link.atts <- filter(control.attributes, respondent_school == i)
  
  control.edge.att.network[[i]] %v% "gender" <- as.character(link.atts$gender[match(network.vertex.names(control.edge.att.network[[i]]),
                                                                                    link.atts$id)])
  control.edge.att.network[[i]] %v% "sex.var" <- as.character(link.atts$sex3.var[match(network.vertex.names(control.edge.att.network[[i]]),
                                                                                       link.atts$id)])
  control.edge.att.network[[i]] %v% "scale.var" <-
    link.atts$scale.var[match(network.vertex.names(control.edge.att.network[[i]]),
                              link.atts$id)]
  
  #  Change to sex.var format control.edge.att.network[[i]] %v% "talk.var" <- link.atts$talk.var[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)]
  
  control.edge.att.network[[i]] %v% "talk.var" <- as.character(link.atts$talk.var[match(network.vertex.names(control.edge.att.network[[i]]),
                                                                                        link.atts$id)])
  
  control.edge.att.network[[i]] %v% "std.know" <-
    link.atts$std.know[match(network.vertex.names(control.edge.att.network[[i]]),
                             link.atts$id)]
  control.edge.att.network[[i]] %v% "std.conf" <-
    link.atts$std.conf[match(network.vertex.names(control.edge.att.network[[i]]),
                             link.atts$id)]
  control.edge.att.network[[i]] %v% "std.att"  <-
    link.atts$std.att[match(network.vertex.names(control.edge.att.network[[i]]),
                            link.atts$id)]
  control.edge.att.network[[i]] %v% "know.var" <-
    link.atts$know.var[match(network.vertex.names(control.edge.att.network[[i]]),
                             link.atts$id)]
  control.edge.att.network[[i]] %v% "conf.var" <-
    link.atts$conf.var[match(network.vertex.names(control.edge.att.network[[i]]),
                             link.atts$id)]
  control.edge.att.network[[i]] %v% "att.var"  <-
    link.atts$att.var[match(network.vertex.names(control.edge.att.network[[i]]),
                            link.atts$id)]
  control.edge.att.network[[i]] %v% "school.id" <-
    link.atts$respondent_school[match(network.vertex.names(control.edge.att.network[[i]]),
                                      link.atts$id)]
  
  control.edge.att.network[[i]] %v% "outschool" <- as.character(as.numeric(control.outside.school[[i]]$outschfriends[match(
    network.vertex.names(control.edge.att.network[[i]]),
    control.outside.school[[i]]$respondent_id
  )]))
  
  ###Add interaction terms
  control.edge.att.network[[i]] %v% "sex.active" <-
    ifelse(get.vertex.attribute(control.edge.att.network[[i]] , "sex.var") == "None",
           1,
           0)
  control.edge.att.network[[i]] %v% "know.int" <-
    get.vertex.attribute(control.edge.att.network[[i]] , "sex.active") * get.vertex.attribute(control.edge.att.network[[i]] , "std.know")
  control.edge.att.network[[i]] %v% "conf.int" <-
    get.vertex.attribute(control.edge.att.network[[i]] , "sex.active") * get.vertex.attribute(control.edge.att.network[[i]] , "std.conf")
  control.edge.att.network[[i]] %v% "att.int" <-
    get.vertex.attribute(control.edge.att.network[[i]] , "sex.active") * get.vertex.attribute(control.edge.att.network[[i]] , "std.att")
  
  
  get.vertex.attribute(control.edge.att.network[[i]] , "know.int")
  
  ###Check isolates - i.e. didn't receive a nomination
  iso.check <- degree(control.edge.att.network[[i]]) == 0
  ###Check nodes without a linked gender - i.e. responded but didn't send a nomination
  miss.check <-
    is.na(get.vertex.attribute(control.edge.att.network[[i]], 'gender'))
  ##Check where these are the same i.e. not in data or nominations
  #    These are true missing, weren't a respondent or recipient of nomination
  iso.check & miss.check
  true.missing.ids <-
    network.vertex.names(control.edge.att.network[[i]])[iso.check &
                                                          miss.check]
  #Drop the true missing ids
  control.edge.att.network[[i]] <-
    delete.vertices(control.edge.att.network[[i]], true.missing.ids)
  
}

table(get.vertex.attribute(control.edge.att.network[[6]], "std.know") , get.vertex.attribute(control.edge.att.network[[6]], "know.var") )
table(get.vertex.attribute(control.edge.att.network[[6]], "talk.var") , get.vertex.attribute(control.edge.att.network[[6]], "know.var") )

#  baseline schools
b.gender <- list()
b.std.know <- list()
b.std.att <- list()
b.std.conf <- list()
b.know.var <- list()
b.att.var <- list()
b.conf.var <- list()
b.scale.var <- list()
b.sex.var <- list()
b.talk.var <- list()
b.schoolid.var <- list()
b.outschool.var <- list()

load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline_outschool_friends.rdata")

for (i in 1:6){
  link.atts <- filter(baseline.attributes, respondent_school == i)
  b.gender[[i]] <- as.character(link.atts$gender[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  b.sex.var[[i]] <- as.numeric(link.atts$sex3.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  b.scale.var[[i]] <- as.numeric(link.atts$scale.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  b.talk.var[[i]] <- as.numeric(link.atts$talk.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  b.std.know[[i]] <- as.numeric(link.atts$std.know[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  b.std.conf[[i]] <- as.numeric(link.atts$std.conf[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  b.std.att[[i]] <- as.numeric(link.atts$std.att[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  b.know.var[[i]] <- as.numeric(link.atts$know.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  b.conf.var[[i]] <- as.numeric(link.atts$conf.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  b.att.var[[i]] <- as.numeric(link.atts$att.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  b.schoolid.var[[i]] <- as.numeric(link.atts$respondent_school[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  b.outschool.var[[i]] <- as.numeric(baseline.outside.school[[i]]$outschfriends[match(network.vertex.names(baseline.edge.att.network[[i]]), baseline.outside.school[[i]]$respondent_id)])
  }


for (i in 1:6){
   set.vertex.attribute(baseline.edge.att.network[[i]], "gender", b.gender[[i]])
   set.vertex.attribute(baseline.edge.att.network[[i]], "sex.var", b.sex.var[[i]])
   set.vertex.attribute(baseline.edge.att.network[[i]], "scale.var", b.scale.var[[i]])
   set.vertex.attribute(baseline.edge.att.network[[i]], "talk.var", b.talk.var[[i]])
   set.vertex.attribute(baseline.edge.att.network[[i]], "std.know", b.std.know[[i]])
   set.vertex.attribute(baseline.edge.att.network[[i]], "std.conf", b.std.conf[[i]])
   set.vertex.attribute(baseline.edge.att.network[[i]], "std.att", b.std.att[[i]])
   set.vertex.attribute(baseline.edge.att.network[[i]], "know.var", b.std.know[[i]])
   set.vertex.attribute(baseline.edge.att.network[[i]], "conf.var", b.std.conf[[i]])
   set.vertex.attribute(baseline.edge.att.network[[i]], "att.var", b.std.att[[i]])
   set.vertex.attribute(baseline.edge.att.network[[i]], "school.id"  , b.schoolid.var[[i]])
   set.vertex.attribute(baseline.edge.att.network[[i]], "outschool.var", b.outschool.var[[i]])
  
  
link.atts <- filter(baseline.attributes, respondent_school == i)
  
  baseline.edge.att.network[[i]] %v% "gender" <- as.character(
    link.atts$gender[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  baseline.edge.att.network[[i]] %v% "sex.var" <- as.character(
    link.atts$sex3.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  baseline.edge.att.network[[i]] %v% "scale.var" <- link.atts$scale.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)]


## changes to sex.var variable 

  baseline.edge.att.network[[i]] %v% "talk.var" <- as.character(
    link.atts$talk.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)] )
  baseline.edge.att.network[[i]] %v% "std.know" <- link.atts$std.know[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)]
  baseline.edge.att.network[[i]] %v% "std.conf" <- link.atts$std.conf[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)]
  baseline.edge.att.network[[i]] %v% "std.att" <-  link.atts$std.att[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)]
  baseline.edge.att.network[[i]] %v% "know.var" <- link.atts$know.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)]
  baseline.edge.att.network[[i]] %v% "conf.var" <- link.atts$conf.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)]
  baseline.edge.att.network[[i]] %v% "att.var"  <- link.atts$att.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)]
  baseline.edge.att.network[[i]] %v% "school.id" <- link.atts$respondent_school[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)]
  
  baseline.edge.att.network[[i]] %v% "outschool" <- as.numeric(baseline.outside.school[[i]]$outschfriends[match(network.vertex.names(baseline.edge.att.network[[i]]), baseline.outside.school[[i]]$respondent_id)]) 
  
  
  ###Add interaction terms
  baseline.edge.att.network[[i]] %v% "sex.active" <- ifelse(get.vertex.attribute(baseline.edge.att.network[[i]] , "sex.var") == "None",1,0)
  baseline.edge.att.network[[i]] %v% "know.int" <- get.vertex.attribute(baseline.edge.att.network[[i]] , "sex.active") * get.vertex.attribute(baseline.edge.att.network[[i]] , "std.know")
  baseline.edge.att.network[[i]] %v% "conf.int" <- get.vertex.attribute(baseline.edge.att.network[[i]] , "sex.active") * get.vertex.attribute(baseline.edge.att.network[[i]] , "std.conf")
  baseline.edge.att.network[[i]] %v% "att.int" <- get.vertex.attribute(baseline.edge.att.network[[i]] , "sex.active") * get.vertex.attribute(baseline.edge.att.network[[i]] , "std.att")
  

###Check isolates - i.e. didn't receive a nomination
     iso.check <- degree(baseline.edge.att.network[[i]]) == 0
###Check nodes without a linked school id - i.e. didn't send a nomination
     miss.check <- is.na(get.vertex.attribute(baseline.edge.att.network[[i]],'gender') )

##Check where these are the same i.e. not in data or nominations
#    These are true missing, weren't a respondent or recipient of nomination
#        iso.check & miss.check 

true.missing.ids <- network.vertex.names(baseline.edge.att.network[[i]])[iso.check & miss.check]

#Drop the true missing ids

baseline.edge.att.network[[i]] <- delete.vertices(baseline.edge.att.network[[i]],true.missing.ids)


##This code shouldnt be needed with ids fixed up####Drop ids linked to a different school id 
####Drop Ids for other schools
#wrong.schid <- get.vertex.attribute(baseline.edge.att.network[[i]],'school.id') != i
#drop.ids <- network.vertex.names(baseline.edge.att.network[[i]])[wrong.schid]
###remove NAs to pass to delete vertices command
#drop.ids <- drop.ids[complete.cases(drop.ids)]
#baseline.edge.att.network[[i]] <- delete.vertices(baseline.edge.att.network[[i]],drop.ids)
}

table(get.vertex.attribute(baseline.edge.att.network[[1]], "know.var" ), 
      get.vertex.attribute(baseline.edge.att.network[[1]], "std.know" ), useNA = "ifany")



baseline.full.network.with.missing <- list()
for (i in 1:6){
baseline.full.network.with.missing[[i]] <- baseline.edge.att.network[[i]]
}

control.full.network.with.missing <- list()
for (i in 1:6){
  control.full.network.with.missing[[i]] <- control.edge.att.network[[i]]
}

###############################################################
###Create node attributes based on in-edge attributes         #
###############################################################

for (i in 1:6){
###COunt the number of nodes in the network
leng <- control.full.network.with.missing[[i]]$gal$n

##Create a list with NAs, this prevents an error whenever the next loop returns no value
mean.indeg.sex         <- rep(NA, leng)
mean.indeg.ftalk       <- rep(NA, leng)
mean.indeg.ftimein     <- rep(NA, leng)
mean.indeg.ftimeout    <- rep(NA, leng)
mean.indeg.ftimeonline <- rep(NA, leng)


####Take the mean sex nominated by friends
for (j in 1:leng){
mean.indeg.sex[j]         <- mean( get.edge.attribute(get.edges(control.full.network.with.missing[[i]],j, neighborhood = "in") ,"fsex") ,  na.rm = T)
mean.indeg.ftalk[j]       <- mean( get.edge.attribute(get.edges(control.full.network.with.missing[[i]],j, neighborhood = "in") ,"ftalk") ,  na.rm = T)
mean.indeg.ftimein[j]     <- mean( get.edge.attribute(get.edges(control.full.network.with.missing[[i]],j, neighborhood = "in") ,"ftimein") ,  na.rm = T)
mean.indeg.ftimeout[j]    <- mean( get.edge.attribute(get.edges(control.full.network.with.missing[[i]],j, neighborhood = "in") ,"ftimeout") ,  na.rm = T)
mean.indeg.ftimeonline[j] <- mean( get.edge.attribute(get.edges(control.full.network.with.missing[[i]],j, neighborhood = "in") ,"ftimeonline") ,  na.rm = T)

}
####Create this as a node attribute
set.vertex.attribute(control.full.network.with.missing[[i]],'mean.nom.sex',mean.indeg.sex)
set.vertex.attribute(control.full.network.with.missing[[i]],'mean.nom.ftalk',mean.indeg.ftalk)
set.vertex.attribute(control.full.network.with.missing[[i]],'mean.nom.ftimein',mean.indeg.ftimein)
set.vertex.attribute(control.full.network.with.missing[[i]],'mean.nom.ftimeout',mean.indeg.ftimeout)
set.vertex.attribute(control.full.network.with.missing[[i]],'mean.nom.ftimeonline',mean.indeg.ftimeonline)

}

#####            #####
#####   Baseline #####
#####            #####
#####            #####

for (i in 1:6){
  ###COunt the number of nodes in the network
  leng <- baseline.full.network.with.missing[[i]]$gal$n
  
  ##Create a list with NAs, this prevents an error whenever the next loop returns no value
  mean.indeg.sex         <- rep(NA, leng)
  mean.indeg.ftalk       <- rep(NA, leng)
  mean.indeg.ftimein     <- rep(NA, leng)
  mean.indeg.ftimeout    <- rep(NA, leng)
  mean.indeg.ftimeonline <- rep(NA, leng)
  
  
  ####Take the mean sex nominated by friends
  for (j in 1:leng){
    mean.indeg.sex[j]         <- mean( get.edge.attribute(get.edges(baseline.full.network.with.missing[[i]],j, neighborhood = "in") ,"fsex") ,  na.rm = T)
    mean.indeg.ftalk[j]       <- mean( get.edge.attribute(get.edges(baseline.full.network.with.missing[[i]],j, neighborhood = "in") ,"ftalk") ,  na.rm = T)
    mean.indeg.ftimein[j]     <- mean( get.edge.attribute(get.edges(baseline.full.network.with.missing[[i]],j, neighborhood = "in") ,"ftimein") ,  na.rm = T)
    mean.indeg.ftimeout[j]    <- mean( get.edge.attribute(get.edges(baseline.full.network.with.missing[[i]],j, neighborhood = "in") ,"ftimeout") ,  na.rm = T)
    mean.indeg.ftimeonline[j] <- mean( get.edge.attribute(get.edges(baseline.full.network.with.missing[[i]],j, neighborhood = "in") ,"ftimeonline") ,  na.rm = T)
    
  }
  ####Create this as a node attribute
  set.vertex.attribute(baseline.full.network.with.missing[[i]],'mean.nom.sex',mean.indeg.sex)
  set.vertex.attribute(baseline.full.network.with.missing[[i]],'mean.nom.ftalk',mean.indeg.ftalk)
  set.vertex.attribute(baseline.full.network.with.missing[[i]],'mean.nom.ftimein',mean.indeg.ftimein)
  set.vertex.attribute(baseline.full.network.with.missing[[i]],'mean.nom.ftimeout',mean.indeg.ftimeout)
  set.vertex.attribute(baseline.full.network.with.missing[[i]],'mean.nom.ftimeonline',mean.indeg.ftimeonline)
  
}


table(get.vertex.attribute(baseline.full.network.with.missing[[6]], "std.know") ,
      get.vertex.attribute(baseline.full.network.with.missing[[6]], "know.var") )

table(get.vertex.attribute(baseline.full.network.with.missing[[5]], "std.know") ,
      get.vertex.attribute(baseline.full.network.with.missing[[5]], "know.var") )

table(get.vertex.attribute(baseline.full.network.with.missing[[5]], "att.int") )
table(get.vertex.attribute(control.full.network.with.missing[[5]], "att.int") )


get.vertex.attribute(control.full.network.with.missing[[1]],'sex_percep.var')
get.vertex.attribute(control.full.network.with.missing[[1]],'mean.nom.sex')
get.vertex.attribute(control.full.network.with.missing[[1]],'outschool')
get.vertex.attribute(control.full.network.with.missing[[1]],'outschool.var')

network.vertex.names(control.full.network.with.missing[[1]])

save(baseline.full.network.with.missing, file = "T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline_full_network_with_missing.rdata")
save(control.full.network.with.missing, file = "T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control_full_network_with_missing.rdata")



#-------------------------------------------------------------
# + + + SAVE THE DATA + + + + + + + +  
#------------------------------------------------------------- 
# BASELINE
baseline.net.dataset <- list()
for (i in 1:6){
  baseline.net.dataset[[i]] <- as.data.frame(cbind(network.vertex.names(baseline.full.network.with.missing[[i]]),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'school.id'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'gender'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'std.know'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'std.att'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'std.conf'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'know.var'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'att.var'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'conf.var'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'scale.var'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'sex.var'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'sex.active'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'know.int'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'conf.int'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'att.int'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'talk.var'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'outschool.var'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'mean.nom.ftalk'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'mean.nom.ftimein'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'mean.nom.ftimeonline'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'mean.nom.ftimeout'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'mean.nom.sex'),
                                                   degree(baseline.full.network.with.missing[[i]], cmode = "indegree"),
                                                   degree(baseline.full.network.with.missing[[i]], cmode = "outdegree")
                                                  )
                                             )

colnames(baseline.net.dataset[[i]]) <- c( "id", "school.id", "gender",
                                          'std.know','std.att','std.conf'   ,'know.var',
                                          'att.var','conf.var','scale.var'  ,'sex.var','sex.active',
                                          'know.int','conf.int','att.int'   ,'talk.var',
                                          'outschool.var','mean.nom.ftalk'  ,'mean.nom.ftimein','mean.nom.ftimeonline',
                                          'mean.nom.ftimeout','mean.nom.sex',"indegree","outdegree")
  

  }


# Control
control.net.dataset <- list()


for (i in 1:6){
  control.net.dataset[[i]] <- as.data.frame(cbind(network.vertex.names(control.full.network.with.missing[[i]]),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'school.id'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'gender'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'std.know'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'std.att'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'std.conf'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'know.var'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'att.var'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'conf.var'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'scale.var'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'sex.var'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'sex.active'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'know.int'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'conf.int'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'att.int'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'sex_percep.var'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'talk.var'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'outschool.var'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'mean.nom.ftalk'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'mean.nom.ftimein'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'mean.nom.ftimeonline'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'mean.nom.ftimeout'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'mean.nom.sex'),
                                                   degree(control.full.network.with.missing[[i]], cmode = "indegree"),
                                                   degree(control.full.network.with.missing[[i]], cmode = "outdegree")
                                                   )
                                            )
  colnames(control.net.dataset[[i]]) <- c( "id", "school.id", "gender",
                                           'std.know','std.att','std.conf','know.var',
                                           'att.var','conf.var','scale.var','sex.var',
                                           'sex.active','know.int','conf.int','att.int','sex.percep.var','talk.var', 
                                           'outschool.var','mean.nom.ftalk','mean.nom.ftimein','mean.nom.ftimeonline',
                                           'mean.nom.ftimeout','mean.nom.sex',"indegree","outdegree")
}

str(control.net.dataset[[1]])
head(control.net.dataset[[6]])

summary(control.net.dataset[[1]])
summary(control.net.dataset[[2]])
dim(control.net.dataset[[3]])
dim(control.net.dataset[[4]])
dim(control.net.dataset[[5]])
dim(control.net.dataset[[6]])

dim(baseline.net.dataset[[1]])
dim(baseline.net.dataset[[2]])
dim(baseline.net.dataset[[3]])
dim(baseline.net.dataset[[4]])
dim(baseline.net.dataset[[5]])
dim(baseline.net.dataset[[6]])

#View(baseline.net.dataset[[1]])
#View(control.net.dataset[[1]])
summary(baseline.net.dataset[[2]])
summary(baseline.net.dataset[[3]])
summary(baseline.net.dataset[[4]])
summary(baseline.net.dataset[[5]])
summary(baseline.net.dataset[[6]])


for (i in 1:6)  baseline.net.dataset[[i]]$school.id <- i
for (i in 1:6)  control.net.dataset[[i]]$school.id <- i



save(control.net.dataset, file = "T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control_net_dataset.rdata")
save(baseline.net.dataset, file = "T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline_net_dataset.rdata")
