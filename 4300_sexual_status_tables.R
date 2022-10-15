rm(list = ls())
#################
#               #
#      Name     #
#               #
#################

# 2000 atribute files

# Mark McCann developed this script
#     


#############
#  Purpose  #
#############

# This file produces a table describing variation between sexual activity groups

##############
#            #
#    Notes   #
#            #
##############

#Reviewer comment from Journal of Social and Personal Relationships 2021

#"the results discuss the three sexual activity groups and then primarily
#   focus on friendships ties, but we do not learn much about group differences.
#    Did the groups themselves differ on knowledge, norms or confidence?"





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

library("plyr")
library("dplyr")
library("psych")


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


#load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline_attributes_with_raw.rdata")
#load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control_attributes_with_raw.rdata")

load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/baseline_attributes.rdata")
load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/control_attributes.rdata")

colnames(baseline.attributes)
colnames(control.attributes)

full.df <- bind_rows(baseline.attributes, control.attributes)

dim(full.df)

colnames(full.df)


table(full.df$sex3.var, useNA = "ifany")

###Create table to hold summary results
desctable <- data.frame(matrix(NA, nc = 4, nr = 9))

colnames(desctable) <- c("Inactive",
                         "Active no oral/vaginal",
                         "Oral/vaginal",
                         "Total")

rownames(desctable) <- c("Gender (proportions)",
                         "Boy",
                         "Girl",
                         "Trans",
                         "missing",
                         "Antecedents mean (sd)",
                         "Knowledge",
                         "Norms",
                         "Confidence")


###Fill in gender and proportions
val <- table(full.df$gender,full.df$sex3.var, useNA = "ifany" )
prop <- round(prop.table(val, margin = 2), 2)
gendtab <- matrix(paste0(val," (",prop,")") , nr=4 ,nc=4)

desctable[2:5,1] <- gendtab[,2]
desctable[2:5,2] <- gendtab[,3]
desctable[2:5,3] <- gendtab[,1]

##Fill in knowledge mean and sd

x <- describeBy(full.df$know.var, full.df$sex3.var)

desctable[7,1] <- paste0(round(x[[2]]["mean"],2), " (",round(x[[2]]["sd"],2),")")
desctable[7,2] <- paste0(round(x[[3]]["mean"],2), " (",round(x[[3]]["sd"],2),")")
desctable[7,3] <- paste0(round(x[[1]]["mean"],2), " (",round(x[[1]]["sd"],2),")")


##Fill in norms mean and sd

x <- describeBy(full.df$att.var, full.df$sex3.var)

desctable[8,1] <- paste0(round(x[[2]]["mean"],2), " (",round(x[[2]]["sd"],2),")")
desctable[8,2] <- paste0(round(x[[3]]["mean"],2), " (",round(x[[3]]["sd"],2),")")
desctable[8,3] <- paste0(round(x[[1]]["mean"],2), " (",round(x[[1]]["sd"],2),")")

##Fill in confidence mean and sd

x <- describeBy(full.df$conf.var, full.df$sex3.var)

desctable[9,1] <- paste0(round(x[[2]]["mean"],2), " (",round(x[[2]]["sd"],2),")")
desctable[9,2] <- paste0(round(x[[3]]["mean"],2), " (",round(x[[3]]["sd"],2),")")
desctable[9,3] <- paste0(round(x[[1]]["mean"],2), " (",round(x[[1]]["sd"],2),")")

##Fill in the totals column
x <- table(full.df$gender, useNA = "ifany")
gendtot <- paste0(x," (",round(prop.table(x),2),")")

desctable[2:5,4] <- gendtot

desctable[7,4] <- paste0(round(mean(full.df$know.var, na.rm = T),2)," (",
round(sd(full.df$know.var, na.rm = T),2),")")

desctable[8,4] <- paste0(round(mean(full.df$att.var, na.rm = T),2)," (",
                         round(sd(full.df$att.var, na.rm = T),2),")")

desctable[9,4] <- paste0(round(mean(full.df$conf.var, na.rm = T),2)," (",
                         round(sd(full.df$conf.var, na.rm = T),2),")")


#####Fill in cross,group tests


fisher.test(table(full.df$gender,full.df$sex3.var), workspace = 20000000)

a <- oneway.test(know.var ~ sex3.var, data = full.df)
b <- oneway.test(att.var  ~ sex3.var, data = full.df)
c <- oneway.test(conf.var ~ sex3.var, data = full.df)

setwd("T:/projects/stash_trial/09 STASH SNA/DisseminationAndImpact/Manuscripts_Papers/Soc Sex Dev Paper")

write.csv(c(a,b,c),"Antecedent by sex status tests.csv")


require(ggplot2)

full.df$sex3.plot.var <- factor(full.df$sex3.var, levels = c("None","Some experience","Experience"))

know.violin <- ggplot(full.df, aes(x= sex3.plot.var, fill = sex3.plot.var , y = know.var)) +
  geom_violin() +
  
know.violin

norm.violin <- ggplot(full.df, aes(x= sex3.plot.var, fill = sex3.plot.var , y = att.var)) +
  geom_violin()
norm.violin

conf.violin <- ggplot(full.df, aes(x= sex3.plot.var, fill = sex3.plot.var , y = conf.var)) +
  geom_violin()
conf.violin


#setwd("T:/projects/stash_trial/09 STASH SNA/DisseminationAndImpact/Manuscripts_Papers/Control Schools Paper/May 2020 update")

setwd("T:/projects/stash_trial/09 STASH SNA/DisseminationAndImpact/Manuscripts_Papers/Soc Sex Dev Paper")
pdf("Knowledge by sex status violin.pdf")
know.violin
dev.off()

pdf("Norms by sex status violin.pdf")
norm.violin
dev.off()

pdf("Confidence by sex status violin.pdf")
conf.violin
dev.off()





setwd("T:/projects/stash_trial/09 STASH SNA/DisseminationAndImpact/Manuscripts_Papers/Soc Sex Dev Paper")
write.csv(desctable, file = "sex status tables.csv")
