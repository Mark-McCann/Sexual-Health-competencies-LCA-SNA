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

# prepare network data from coded data

#########################
#                       #
#    Load packages      #
#                       #
#########################
library(network)
library(dplyr)
library(reshape2)

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


#########################################################################
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#    +    +    +       CONTROL STASH    +    +    +    +    +    +    +
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#########################################################################


##Raw data
raw.sch.list      <-list()
raw.sch.el        <-list()
school            <-list()
net.sch           <-list()
vert.attr         <-list()
net               <-list()

load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/recoded_control.rdata")

raw.sch.list <- list()
for (i in 1:6){
  raw.sch.list[[i]] <-  filter(recoded.control, respondent_school==i)
}

lapply(raw.sch.list, function(x) dim(x))


  ###Strip out all the variables I need for each school
for (i in 1:6){
  raw.sch.list[[i]] <- raw.sch.list[[i]][, c("respondent_id","q_1", "q_19_friend_1_hidden_id", "q_19_friend_2_hidden_id", "q_19_friend_3_hidden_id",
                                             "q_19_friend_4_hidden_id", "q_19_friend_5_hidden_id", "q_19_friend_6_hidden_id", "q_19_friend_1_q_1", "q_19_friend_1_q_2", "q_19_friend_1_q_3", 
                                             "q_19_friend_1_q_4", "q_19_friend_1_q_5", "q_19_friend_1_q_6", "q_19_friend_1_q_7",
                                             "q_19_friend_2_q_1",	"q_19_friend_2_q_2", "q_19_friend_2_q_3", "q_19_friend_2_q_4", 	"q_19_friend_2_q_5", "q_19_friend_2_q_6", "q_19_friend_2_q_7", 
                                             "q_19_friend_3_q_1",	"q_19_friend_3_q_2",	"q_19_friend_3_q_3",	"q_19_friend_3_q_4",	"q_19_friend_3_q_5",	"q_19_friend_3_q_6",	"q_19_friend_3_q_7",
                                             "q_19_friend_4_q_1",	"q_19_friend_4_q_2",	"q_19_friend_4_q_3",	"q_19_friend_4_q_4",	"q_19_friend_4_q_5", 	"q_19_friend_4_q_6", 	"q_19_friend_4_q_7",
                                             "q_19_friend_5_q_1",	"q_19_friend_5_q_2"	, "q_19_friend_5_q_3",	"q_19_friend_5_q_4",	"q_19_friend_5_q_5",	"q_19_friend_5_q_6",	"q_19_friend_5_q_7",
                                             "q_19_friend_6_q_1",	"q_19_friend_6_q_2",	"q_19_friend_6_q_3",	"q_19_friend_6_q_4",	"q_19_friend_6_q_5",	"q_19_friend_6_q_6",	"q_19_friend_6_q_7"
  )]
}

# rename columns
for (i in 1:6){
  colnames(raw.sch.list[[i]]) <- c("respondent_id", "respondent_sex","q_19_friend_1_hidden_id", "q_19_friend_2_hidden_id", "q_19_friend_3_hidden_id",
                                   "q_19_friend_4_hidden_id", "q_19_friend_5_hidden_id", "q_19_friend_6_hidden_id", 
                                   "friend_1_sex", "friend_1_school", "friend_1_FB", 
                                   "friend_1_talk", "friend_1_timein", "friend_1_timeout", 
                                   "friend_1_timeonline", "friend_2_sex", "friend_2_school", "friend_2_FB",
                                   "friend_2_talk", "friend_2_timein", "friend_2_timeout", "friend_2_timeonline", 
                                   "friend_3_sex", "friend_3_school", "friend_3_FB",
                                   "friend_3_talk", "friend_3_timein", "friend_3_timeout", "friend_3_timeonline",
                                   "friend_4_sex", "friend_4_school", "friend_4_FB",
                                   "friend_4_talk", "friend_4_timein", "friend_4_timeout", "friend_4_timeonline", 
                                   "friend_5_sex", "friend_5_school", "friend_5_FB",
                                   "friend_5_talk", "friend_5_timein", "friend_5_timeout", "friend_5_timeonline", 
                                   "friend_6_sex", "friend_6_school", "friend_6_FB",
                                   "friend_6_talk", "friend_6_timein", "friend_6_timeout", "friend_6_timeonline")
}                                 
colnames(raw.sch.list[[1]])                               

# Set missing values to NA
for (i in 1:6){
  is.na(raw.sch.list[[i]]) <- !raw.sch.list[[i]]
} 

raw.control.network.questions <- raw.sch.list

save(raw.control.network.questions, file = "T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control_raw_net_qs.rdata")

#########################################################################
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#    +    +    +       Baseline STASH    +    +    +    +    +    +    +
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


##Raw data
raw.sch.list      <-list()
raw.sch.el        <-list()
school            <-list()
net.sch           <-list()
vert.attr         <-list()
net               <-list()

load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/recoded_baseline.rdata")

raw.sch.list <- list()
for (i in 1:6){
  raw.sch.list[[i]] <-  filter(recoded.baseline, respondent_school==i)
}

###Strip out all the variables I need for each school
for (i in 1:6){
  raw.sch.list[[i]] <- raw.sch.list[[i]][, c("respondent_id","q_1", "q_19_friend_1_hidden_id", "q_19_friend_2_hidden_id", "q_19_friend_3_hidden_id",
                                             "q_19_friend_4_hidden_id", "q_19_friend_5_hidden_id", "q_19_friend_6_hidden_id", "q_19_friend_1_q_1", "q_19_friend_1_q_2", "q_19_friend_1_q_3", 
                                             "q_19_friend_1_q_4", "q_19_friend_1_q_5", "q_19_friend_1_q_6", "q_19_friend_1_q_7",
                                             "q_19_friend_2_q_1",	"q_19_friend_2_q_2", "q_19_friend_2_q_3", "q_19_friend_2_q_4", 	"q_19_friend_2_q_5", "q_19_friend_2_q_6", "q_19_friend_2_q_7", 
                                             "q_19_friend_3_q_1",	"q_19_friend_3_q_2",	"q_19_friend_3_q_3",	"q_19_friend_3_q_4",	"q_19_friend_3_q_5",	"q_19_friend_3_q_6",	"q_19_friend_3_q_7",
                                             "q_19_friend_4_q_1",	"q_19_friend_4_q_2",	"q_19_friend_4_q_3",	"q_19_friend_4_q_4",	"q_19_friend_4_q_5", 	"q_19_friend_4_q_6", 	"q_19_friend_4_q_7",
                                             "q_19_friend_5_q_1",	"q_19_friend_5_q_2"	, "q_19_friend_5_q_3",	"q_19_friend_5_q_4",	"q_19_friend_5_q_5",	"q_19_friend_5_q_6",	"q_19_friend_5_q_7",
                                             "q_19_friend_6_q_1",	"q_19_friend_6_q_2",	"q_19_friend_6_q_3",	"q_19_friend_6_q_4",	"q_19_friend_6_q_5",	"q_19_friend_6_q_6",	"q_19_friend_6_q_7"
  )]
}



# rename columns
for (i in 1:6){
  colnames(raw.sch.list[[i]]) <- c("respondent_id", "respondent_sex","q_19_friend_1_hidden_id", "q_19_friend_2_hidden_id", "q_19_friend_3_hidden_id",
                                   "q_19_friend_4_hidden_id", "q_19_friend_5_hidden_id", "q_19_friend_6_hidden_id", 
                                   "friend_1_sex", "friend_1_school", "friend_1_FB", 
                                   "friend_1_talk", "friend_1_timein", "friend_1_timeout", 
                                   "friend_1_timeonline", "friend_2_sex", "friend_2_school", "friend_2_FB",
                                   "friend_2_talk", "friend_2_timein", "friend_2_timeout", "friend_2_timeonline", 
                                   "friend_3_sex", "friend_3_school", "friend_3_FB",
                                   "friend_3_talk", "friend_3_timein", "friend_3_timeout", "friend_3_timeonline",
                                   "friend_4_sex", "friend_4_school", "friend_4_FB",
                                   "friend_4_talk", "friend_4_timein", "friend_4_timeout", "friend_4_timeonline", 
                                   "friend_5_sex", "friend_5_school", "friend_5_FB",
                                   "friend_5_talk", "friend_5_timein", "friend_5_timeout", "friend_5_timeonline", 
                                   "friend_6_sex", "friend_6_school", "friend_6_FB",
                                   "friend_6_talk", "friend_6_timein", "friend_6_timeout", "friend_6_timeonline")
}                                 
colnames(raw.sch.list[[1]])                               

# Set missing values to NA
for (i in 1:6){
  is.na(raw.sch.list[[i]]) <- !raw.sch.list[[i]]
} 


raw.baseline.network.questions <- raw.sch.list

save(raw.baseline.network.questions, file = "T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline_raw_net_qs.rdata")



#############################################################################
#
#             Networks with edge attributes 
#
#
#############################################################################




####Creating nomination question attributes. 


##friend_*_sex       Are they male or female?
##friend_*_school    Are they in s4 at this school (can ignore maybe if linked via ID)?
##friend_*_FB        Are you facebook friends with them?
##friend_*_talk      Are tehy someone you would talk to about something private?
#                    Do you spend time with this person
##friend_*_timein       in school
##friend_*_timeout      outside school
##friend_*_timeonline   online

control.edge.att.edgelist <- list()
control.edge.att.network  <- list()

for (i in 1:6){
  colnames(raw.control.network.questions[[i]]) <- c("respondent_id", "respondent_sex",
                                                    "friend_1_hidden_id", "friend_2_hidden_id", "friend_3_hidden_id",
                                                    "friend_4_hidden_id", "friend_5_hidden_id", "friend_6_hidden_id", 
                                                    "friend_1_sex", "friend_1_school", "friend_1_FB", 
                                                    "friend_1_talk", "friend_1_timein", "friend_1_timeout", 
                                                    "friend_1_timeonline", "friend_2_sex", "friend_2_school", "friend_2_FB",
                                                    "friend_2_talk", "friend_2_timein", "friend_2_timeout", "friend_2_timeonline", 
                                                    "friend_3_sex", "friend_3_school", "friend_3_FB",
                                                    "friend_3_talk", "friend_3_timein", "friend_3_timeout", "friend_3_timeonline",
                                                    "friend_4_sex", "friend_4_school", "friend_4_FB",
                                                    "friend_4_talk", "friend_4_timein", "friend_4_timeout", "friend_4_timeonline", 
                                                    "friend_5_sex", "friend_5_school", "friend_5_FB",
                                                    "friend_5_talk", "friend_5_timein", "friend_5_timeout", "friend_5_timeonline", 
                                                    "friend_6_sex", "friend_6_school", "friend_6_FB",
                                                    "friend_6_talk", "friend_6_timein", "friend_6_timeout", "friend_6_timeonline")
                                 
###add school id var
raw.control.network.questions[[i]]$respondent_school <- i
df <- raw.control.network.questions[[i]]
###Melt data into  edgelist based on edges only
edge.df <- df[,c("respondent_id","friend_1_hidden_id",  "friend_2_hidden_id", 
                 "friend_3_hidden_id","friend_4_hidden_id","friend_5_hidden_id", "friend_6_hidden_id")]

edge.el <- melt(edge.df, id.vars = "respondent_id", variable.name = "friend.order")

colnames(edge.el) <- c("respondent_id","friend.order","to.id")

###Melt data into attributes based on first attr
fsex.df <- df[,c("respondent_id","friend_1_sex",  "friend_2_sex", 
                 "friend_3_sex","friend_4_sex","friend_5_sex", "friend_6_sex")]

colnames(fsex.df)
fsex.el <- melt(fsex.df, id.vars = "respondent_id", variable.name = "friend.order")

##trim out the friend.order column
colnames(fsex.el) <- c("respondent_id","friend.order","fsex")

###Melt data into attributes based on second attr
fschool.df <- df[,c("respondent_id","friend_1_school",  "friend_2_school", 
                    "friend_3_school","friend_4_school","friend_5_school", "friend_6_school")]

colnames(fschool.df)
fschool.el <- melt(fschool.df, id.vars = "respondent_id", variable.name = "friend.order")
colnames(fschool.el) <- c("respondent_id","friend.order","fschool")

###Melt data into attributes based on third attr
fFB.df <- df[,c("respondent_id","friend_1_FB",  "friend_2_FB", 
                "friend_3_FB","friend_4_FB","friend_5_FB", "friend_6_FB")]

colnames(fFB.df)
fFB.el <- melt(fFB.df, id.vars = "respondent_id", variable.name = "friend.order")
colnames(fFB.el) <- c("respondent_id","friend.order","fFB")

###Melt data into attributes based on fourth attr
ftalk.df <- df[,c("respondent_id","friend_1_talk",  "friend_2_talk", 
                  "friend_3_talk","friend_4_talk","friend_5_talk", "friend_6_talk")]
colnames(ftalk.df)
ftalk.el <- melt(ftalk.df, id.vars = "respondent_id", variable.name = "friend.order")
colnames(ftalk.el) <- c("respondent_id","friend.order","ftalk")

###Melt data into attributes based on fifth attr
ftimein.df <- df[,c("respondent_id","friend_1_timein",  "friend_2_timein", 
                    "friend_3_timein","friend_4_timein","friend_5_timein", "friend_6_timein")]

colnames(ftimein.df)
ftimein.el <- melt(ftimein.df, id.vars = "respondent_id", variable.name = "friend.order")
colnames(ftimein.el) <- c("respondent_id","friend.order","ftimein")

###Melt data into attributes based on sixth attr
ftimeout.df <- df[,c("respondent_id","friend_1_timeout",  "friend_2_timeout", 
                     "friend_3_timeout","friend_4_timeout","friend_5_timeout", "friend_6_timeout")]

colnames(ftimeout.df)
ftimeout.el <- melt(ftimeout.df, id.vars = "respondent_id", variable.name = "friend.order")
colnames(ftimeout.el) <- c("respondent_id","friend.order","ftimeout")

###Melt data into attributes based on seventh attr
ftimeonline.df <- df[,c("respondent_id","friend_1_timeonline",  "friend_2_timeonline", 
                        "friend_3_timeonline","friend_4_timeonline","friend_5_timeonline", "friend_6_timeonline")]

colnames(ftimeonline.df)
ftimeonline.el <- melt(ftimeonline.df, id.vars = "respondent_id", variable.name = "friend.order")
colnames(ftimeonline.el) <- c("respondent_id","friend.order","ftimeonline")

##Second attr
head(edge.el)
head(edge.df)
head(fsex.df)
head(fsex.el)

###Check the lists

dim(fsex.el      )
dim(fschool.el)
dim(fFB.el)
dim(ftalk.el)
dim(ftimein.el)
dim(ftimeout.el)
dim(ftimeonline.el)

##All the same dimensions
head(edge.el      )
head(fsex.el      )
head(fschool.el)
head(fFB.el)
head(ftalk.el)
head(ftimein.el)
tail(edge.el      )
tail(ftimeout.el)
tail(ftimeonline.el)

###ID orders are consistent. Ok to merge columns on 
attributes.edgelist <- bind_cols(edge.el    ,   
                                 fsex.el    ,  
                                 fschool.el ,
                                 fFB.el     ,
                                 ftalk.el   ,  
                                 ftimein.el ,
                                 ftimeout.el,
                                 ftimeonline.el)

colnames(attributes.edgelist)
head(attributes.edgelist)
#View(attributes.edgelist)

colnames(attributes.edgelist)[1] <- "respondent_id"
colnames(attributes.edgelist)[2] <- "friend.order"

attributes.edgelist <- select(attributes.edgelist,
                              respondent_id,  friend.order ,  to.id,
                              fsex,fschool,fFB,ftalk,ftimein,
                              ftimeout,ftimeonline)

##Reorder columns
attributes.edgelist <- attributes.edgelist[,c("respondent_id" ,"to.id"      ,
                                              "friend.order"  ,"fsex"       ,
                                              "fschool"       ,"fFB"        ,
                                              "ftalk"         ,"ftimein"    ,
                                              "ftimeout"      ,"ftimeonline")] 

###Drop any edges to outside school friends

table(attributes.edgelist$to.id, useNA = "always")

attributes.edgelist <- attributes.edgelist[!is.na(attributes.edgelist$to.id),]

#str(attributes.edgelist)
#summary(attributes.edgelist)

##if self loops exist
if(sum(attributes.edgelist$respondent_id == attributes.edgelist$to.id)>0)
   ##remove self loops  
   attributes.edgelist <- attributes.edgelist[-which(attributes.edgelist$respondent_id == attributes.edgelist$to.id),]

  
##remove multiples - drop the second mention of the same alter by an ego
attributes.edgelist <- attributes.edgelist[!duplicated(attributes.edgelist[,1:2]),]


###The network package will create an edge attribute from a matrix. 
att.net <-network(attributes.edgelist,matrix.type='edgelist',ignore.eval=FALSE, loops = F, multiple = F)


control.edge.att.edgelist[[i]] <- attributes.edgelist 
control.edge.att.network[[i]]  <- att.net

}


#plot(control.edge.att.network[[1]])
#plot(control.edge.att.network[[2]])
#plot(control.edge.att.network[[3]])
#plot(control.edge.att.network[[4]])
#plot(control.edge.att.network[[5]])
#plot(control.edge.att.network[[6]])

save(control.edge.att.network, file = "T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control_edge_att_networks.rdata")





##########################Baseline nets

baseline.edge.att.edgelist <- list()
baseline.edge.att.network  <- list()

for (i in 1:6){
  colnames(raw.baseline.network.questions[[i]]) <- c("respondent_id", "respondent_sex",
                                                    "friend_1_hidden_id", "friend_2_hidden_id", "friend_3_hidden_id",
                                                    "friend_4_hidden_id", "friend_5_hidden_id", "friend_6_hidden_id", 
                                                    "friend_1_sex", "friend_1_school", "friend_1_FB", 
                                                    "friend_1_talk", "friend_1_timein", "friend_1_timeout", 
                                                    "friend_1_timeonline", "friend_2_sex", "friend_2_school", "friend_2_FB",
                                                    "friend_2_talk", "friend_2_timein", "friend_2_timeout", "friend_2_timeonline", 
                                                    "friend_3_sex", "friend_3_school", "friend_3_FB",
                                                    "friend_3_talk", "friend_3_timein", "friend_3_timeout", "friend_3_timeonline",
                                                    "friend_4_sex", "friend_4_school", "friend_4_FB",
                                                    "friend_4_talk", "friend_4_timein", "friend_4_timeout", "friend_4_timeonline", 
                                                    "friend_5_sex", "friend_5_school", "friend_5_FB",
                                                    "friend_5_talk", "friend_5_timein", "friend_5_timeout", "friend_5_timeonline", 
                                                    "friend_6_sex", "friend_6_school", "friend_6_FB",
                                                    "friend_6_talk", "friend_6_timein", "friend_6_timeout", "friend_6_timeonline")
  
  ###add school id var
  raw.baseline.network.questions[[i]]$respondent_school <- i
  
  df <- raw.baseline.network.questions[[i]]
  
  ###Melt data into  edgelist based on edges only
  edge.df <- df[,c("respondent_id","friend_1_hidden_id",  "friend_2_hidden_id", 
                   "friend_3_hidden_id","friend_4_hidden_id","friend_5_hidden_id", "friend_6_hidden_id")]
  
  edge.el <- melt(edge.df, id.vars = "respondent_id", variable.name = "friend.order")
  
  colnames(edge.el) <- c("respondent_id","friend.order","to.id")
  
  ###Melt data into attributes based on first attr
  fsex.df <- df[,c("respondent_id","friend_1_sex",  "friend_2_sex", 
                   "friend_3_sex","friend_4_sex","friend_5_sex", "friend_6_sex")]
  
  colnames(fsex.df)
  fsex.el <- melt(fsex.df, id.vars = "respondent_id", variable.name = "friend.order")
  
  ##trim out the friend.order column
  colnames(fsex.el) <- c("respondent_id","friend.order","fsex")
  
  ###Melt data into attributes based on second attr
  fschool.df <- df[,c("respondent_id","friend_1_school",  "friend_2_school", 
                      "friend_3_school","friend_4_school","friend_5_school", "friend_6_school")]
  
  colnames(fschool.df)
  fschool.el <- melt(fschool.df, id.vars = "respondent_id", variable.name = "friend.order")
  colnames(fschool.el) <- c("respondent_id","friend.order","fschool")
  
  ###Melt data into attributes based on third attr
  fFB.df <- df[,c("respondent_id","friend_1_FB",  "friend_2_FB", 
                  "friend_3_FB","friend_4_FB","friend_5_FB", "friend_6_FB")]
  
  colnames(fFB.df)
  fFB.el <- melt(fFB.df, id.vars = "respondent_id", variable.name = "friend.order")
  colnames(fFB.el) <- c("respondent_id","friend.order","fFB")
  
  ###Melt data into attributes based on fourth attr
  ftalk.df <- df[,c("respondent_id","friend_1_talk",  "friend_2_talk", 
                    "friend_3_talk","friend_4_talk","friend_5_talk", "friend_6_talk")]
  colnames(ftalk.df)
  ftalk.el <- melt(ftalk.df, id.vars = "respondent_id", variable.name = "friend.order")
  colnames(ftalk.el) <- c("respondent_id","friend.order","ftalk")
  
  ###Melt data into attributes based on fifth attr
  ftimein.df <- df[,c("respondent_id","friend_1_timein",  "friend_2_timein", 
                      "friend_3_timein","friend_4_timein","friend_5_timein", "friend_6_timein")]
  
  colnames(ftimein.df)
  ftimein.el <- melt(ftimein.df, id.vars = "respondent_id", variable.name = "friend.order")
  colnames(ftimein.el) <- c("respondent_id","friend.order","ftimein")
  
  ###Melt data into attributes based on sixth attr
  ftimeout.df <- df[,c("respondent_id","friend_1_timeout",  "friend_2_timeout", 
                       "friend_3_timeout","friend_4_timeout","friend_5_timeout", "friend_6_timeout")]
  
  colnames(ftimeout.df)
  ftimeout.el <- melt(ftimeout.df, id.vars = "respondent_id", variable.name = "friend.order")
  colnames(ftimeout.el) <- c("respondent_id","friend.order","ftimeout")
  
  ###Melt data into attributes based on seventh attr
  ftimeonline.df <- df[,c("respondent_id","friend_1_timeonline",  "friend_2_timeonline", 
                          "friend_3_timeonline","friend_4_timeonline","friend_5_timeonline", "friend_6_timeonline")]
  
  colnames(ftimeonline.df)
  ftimeonline.el <- melt(ftimeonline.df, id.vars = "respondent_id", variable.name = "friend.order")
  colnames(ftimeonline.el) <- c("respondent_id","friend.order","ftimeonline")
  
  ##Second attr
  head(edge.el)
  head(edge.df)
  head(fsex.df)
  head(fsex.el)
  
  ###Check the lists
  
  dim(fsex.el      )
  dim(fschool.el)
  dim(fFB.el)
  dim(ftalk.el)
  dim(ftimein.el)
  dim(ftimeout.el)
  dim(ftimeonline.el)
  
  ##All the same dimensions
  head(edge.el      )
  head(fsex.el      )
  head(fschool.el)
  head(fFB.el)
  head(ftalk.el)
  head(ftimein.el)
  tail(edge.el      )
  tail(ftimeout.el)
  tail(ftimeonline.el)
  
  ###ID orders are consistent. Ok to merge columns on 
  
  attributes.edgelist <- bind_cols(edge.el    ,   
                                   fsex.el    ,  
                                   fschool.el ,
                                   fFB.el     ,
                                   ftalk.el   ,  
                                   ftimein.el ,
                                   ftimeout.el,
                                   ftimeonline.el)
  
  colnames(attributes.edgelist)
  head(attributes.edgelist)
  
  
  colnames(attributes.edgelist)[1] <- "respondent_id"
  colnames(attributes.edgelist)[2] <- "friend.order"
  
  ##All looks fine, strip out the redundant variables
  
  attributes.edgelist <- select(attributes.edgelist,
                                respondent_id,  friend.order ,  to.id,
                                fsex,fschool,fFB,ftalk,ftimein,
                                ftimeout,ftimeonline)
  
  ##Reorder columns
  attributes.edgelist <- attributes.edgelist[,c("respondent_id" ,"to.id"      ,
                                                "friend.order"  ,"fsex"       ,
                                                "fschool"       ,"fFB"        ,
                                                "ftalk"         ,"ftimein"    ,
                                                "ftimeout"      ,"ftimeonline")] 
  
  ###Drop any edges to outside school friends
  
  table(attributes.edgelist$to.id, useNA = "always")
  
  attributes.edgelist <- attributes.edgelist[!is.na(attributes.edgelist$to.id),]

  ##if self loops exist
  if(sum(attributes.edgelist$respondent_id == attributes.edgelist$to.id)>0)
    ##remove self loops  
    attributes.edgelist <- attributes.edgelist[-which(attributes.edgelist$respondent_id == attributes.edgelist$to.id),]

  
    ##remove multiples - drop the second mention of the same alter by an ego

  attributes.edgelist <- attributes.edgelist[!duplicated(attributes.edgelist[,1:2]),]
  

  ###The network package will create an edge attribute from a matrix. 
  att.net <-network(attributes.edgelist,matrix.type='edgelist',ignore.eval=FALSE, loops = F, multiple = F)
  
  
  baseline.edge.att.edgelist[[i]] <- attributes.edgelist 
  baseline.edge.att.network[[i]]  <- att.net
  
}


save(baseline.edge.att.network, file = "T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline_edge_att_networks.rdata")
