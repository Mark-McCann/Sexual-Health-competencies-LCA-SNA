rm(list = ls())
#################
#               #
#      Name     #
#               #
#################

#     Mark McCann developed the script 

#############
#  Purpose  #
#############

#Plots of metafor results

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
  pdf(paste0(variable," forest 10 networks main model.pdf"))
  
  forest(metareg, 
         main = paste0("Tie probability by difference in ",variable),
         xlab = "Odds ratio: forming a tie",
         transf = exp,
         refline = 1)
  dev.off()
  return(metareg)  
}
####################################################

################################################################
################################################################
########    Combined Plots for Article 
################################################################
################################################################


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



###########################
##  Three sex var plots  ##
###########################

###########################
#Create a 10 * 3 table, 12 schools in rows, coefficient, SE, and baseline/control in columns


inact.coef.table <- matrix(NA, nr=10, nc = 3) 
for (i in 1:length(indata)){
  #Fill the table with varname row 1st col coef  
  inact.coef.table[i,1] <- indata[[i]]$pool.est[which(indata[[i]]$variable=="nodematch.sex.var.1")]
  #Fill the table with varname row 2nd col SE  
  inact.coef.table[i,2] <- indata[[i]]$pool.se[which(indata[[i]]$variable=="nodematch.sex.var.1")]
}

mid.coef.table <- matrix(NA, nr=10, nc = 3) 
for (i in 1:length(indata)){
  #Fill the table with varname row 1st col coef  
  mid.coef.table[i,1] <- indata[[i]]$pool.est[which(indata[[i]]$variable=="nodematch.sex.var.2")]
  #Fill the table with varname row 2nd col SE  
  mid.coef.table[i,2] <- indata[[i]]$pool.se[which(indata[[i]]$variable=="nodematch.sex.var.2")]
}

orvag.coef.table <- matrix(NA, nr=10, nc = 3) 
for (i in 1:length(indata)){
  #Fill the table with varname row 1st col coef  
  orvag.coef.table[i,1] <- indata[[i]]$pool.est[which(indata[[i]]$variable=="nodematch.sex.var.3")]
  #Fill the table with varname row 2nd col SE  
  orvag.coef.table[i,2] <- indata[[i]]$pool.se[which(indata[[i]]$variable=="nodematch.sex.var.3")]
}


#Run meta analysis
inact.meta <- rma(yi=inact.coef.table[,1],
                  sei=inact.coef.table[,2],
                  slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline"
                         , "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control") 
                  , method = "REML", level = 95)


mid.meta <- rma(yi=mid.coef.table[,1],
                sei=mid.coef.table[,2],
                slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline"
                , "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control") ,
                method = "REML", level = 95)


orvag.meta <- rma(yi=orvag.coef.table[,1],
                  sei=orvag.coef.table[,2],
                  slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline"
                       , "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control") ,
                  method = "REML", level = 95)


full.coef.table <- rbind(inact.coef.table,
                         mid.coef.table,
                         orvag.coef.table)

full.coef.table[,3] <- 1:30

all.meta <- rma(yi   =full.coef.table[,1],
                sei=full.coef.table[,2],
                slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline",
                       "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control",
                       "Sch 1 baseline ", "Sch 2 baseline ", "Sch 3 baseline ", "Sch 5 baseline ", "Sch 6 baseline ",
                       "Sch 1 control ", "Sch 2 control ","Sch 3 control ","Sch 5 control ","Sch 6 control ",
                       "Sch 1 baseline  ", "Sch 2 baseline  ", "Sch 3 baseline  ", "Sch 5 baseline  ", "Sch 6 baseline  ",
                       "Sch 1 control  ", "Sch 2 control  ","Sch 3 control  ","Sch 5 control  ","Sch 6 control  ")
                ,
                method = "REML", level = 95)


setwd("T:/projects/stash_trial/09 STASH SNA/DisseminationAndImpact/Manuscripts_Papers/Soc Sex Dev Paper/")

# bmp("Sexual Activity combined Forest plot.bmp", width = 1200 , height = 1200)
# 
# forest(all.meta,  main = "",
#        xlab = "Odds ratio: forming a tie",
#        transf = exp,
#        refline = 1,
#        rows=c(2:11,14:23,26:35), # Which rows to put estimates - leave space for the subgroup diamonds and titles
#        top = 1, # How many blank rows at top of plot for a header
#        cex = 1.25, 
#        ylim = c(1,37),
#        xlim = c(-2,6),  # Limit of whole plot
#        alim = c(-0.5,4.5), # Limit of forest axes
#        at = c(0,0.5,1,2,3.25,4), # Points on forest x axis
#        addfit = FALSE)
# 
# ### switch to bold font
# par(font=2)
# 
# text(-2, c(36), pos=4, font = 2, cex=1.25, c("Oral/Vaginal"))
# addpoly(orvag.meta, row= 25 , cex=1.25,  mlab="", transf = exp)
# text(-2, 25, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(orvag.meta$I2, digits=1, format="f"), "%"))
# 
# par(font=2)
# text(-2, c(24), pos=4, cex=1.25, c("Active no sex"))
# addpoly(mid.meta  , row= 13, cex=1.25,  mlab="", transf = exp)
# text(-2, 13, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(mid.meta$I2, digits=1, format="f"), "%"))
# 
# par(font=2)
# text(-2, c(12), pos=4, cex=1.25, c("Sexually inactive"))
# addpoly(inact.meta, row= 1, cex=1.25,  mlab="", transf = exp)
# text(-2, 1, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(inact.meta$I2, digits=1, format="f"), "%"))
# dev.off()
# 
# ###########################


tiff("Figure 1 Sexual Activity combined Forest plot.tif", width = 1200 , height = 1200)

forest(all.meta,  main = "",
       xlab = "Odds ratio of forming a tie: above 1 favours homophily",
       transf = exp,
       refline = 1,
       rows=c(2:11,14:23,26:35), # Which rows to put estimates - leave space for the subgroup diamonds and titles
       top = 1, # How many blank rows at top of plot for a header
       cex = 1.25, 
       ylim = c(1,37),
       xlim = c(-2,6),  # Limit of whole plot
       alim = c(-0.5,4.5), # Limit of forest axes
       at = c(0,0.5,1,2,3.25,4), # Points on forest x axis
       addfit = FALSE)

### switch to bold font
par(font=2)

text(-2, c(36), pos=4, font = 2, cex=1.25, c("Oral/Vaginal"))
addpoly(orvag.meta, row= 25 , cex=1.25,  mlab="", transf = exp)
text(-2, 25, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(orvag.meta$I2, digits=1, format="f"), "%"))

par(font=2)
text(-2, c(24), pos=4, cex=1.25, c("Active no sex"))
addpoly(mid.meta  , row= 13, cex=1.25,  mlab="", transf = exp)
text(-2, 13, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(mid.meta$I2, digits=1, format="f"), "%"))

par(font=2)
text(-2, c(12), pos=4, cex=1.25, c("Sexually inactive"))
addpoly(inact.meta, row= 1, cex=1.25,  mlab="", transf = exp)
text(-2, 1, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(inact.meta$I2, digits=1, format="f"), "%"))
dev.off()

###########################

#############################
#### Know att conf plots ####
#############################


###########################
#Create a 10 * 3 table, 12 schools in rows, coefficient, SE, and baseline/control in columns

know.coef.table <- matrix(NA, nr=10, nc = 3) 
for (i in 1:length(indata)){
  #Fill the table with varname row 1st col coef  
  know.coef.table[i,1] <- indata[[i]]$pool.est[which(indata[[i]]$variable=="absdiff.std.know")]
  #Fill the table with varname row 2nd col SE  
  know.coef.table[i,2] <- indata[[i]]$pool.se[which(indata[[i]]$variable=="absdiff.std.know")]
}

att.coef.table <- matrix(NA, nr=10, nc = 3) 
for (i in 1:length(indata)){
  #Fill the table with varname row 1st col coef  
  att.coef.table[i,1] <- indata[[i]]$pool.est[which(indata[[i]]$variable=="absdiff.std.att")]
  #Fill the table with varname row 2nd col SE  
  att.coef.table[i,2] <- indata[[i]]$pool.se[which(indata[[i]]$variable=="absdiff.std.att")]
}

conf.coef.table <- matrix(NA, nr=10, nc = 3) 
for (i in 1:length(indata)){
  #Fill the table with varname row 1st col coef  
  conf.coef.table[i,1] <- indata[[i]]$pool.est[which(indata[[i]]$variable=="absdiff.std.conf")]
  #Fill the table with varname row 2nd col SE  
  conf.coef.table[i,2] <- indata[[i]]$pool.se[which(indata[[i]]$variable=="absdiff.std.conf")]
}


#Run meta analysis
know.meta <- rma(yi=know.coef.table[,1],
                 sei=know.coef.table[,2],
                 slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline"
                        , "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control") 
                 , method = "REML", level = 95)


att.meta <- rma(yi=att.coef.table[,1],
                sei=att.coef.table[,2],
                #                 slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline"
                #                         , "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control") 
                method = "REML", level = 95)


conf.meta <- rma(yi=conf.coef.table[,1],
                 sei=conf.coef.table[,2],
                 #                  slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline"
                 #                       , "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control") 
                 method = "REML", level = 95)


full.coef.table <- rbind(know.coef.table,
                         att.coef.table,
                         conf.coef.table)

full.coef.table[,3] <- 1:30

all.meta <- rma(yi   =full.coef.table[,1],
                sei=full.coef.table[,2],
                slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline",
                       "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control",
                       "Sch 1 baseline ", "Sch 2 baseline ", "Sch 3 baseline ", "Sch 5 baseline ", "Sch 6 baseline ",
                       "Sch 1 control ", "Sch 2 control ","Sch 3 control ","Sch 5 control ","Sch 6 control ",
                       "Sch 1 baseline  ", "Sch 2 baseline  ", "Sch 3 baseline  ", "Sch 5 baseline  ", "Sch 6 baseline  ",
                       "Sch 1 control  ", "Sch 2 control  ","Sch 3 control  ","Sch 5 control  ","Sch 6 control  ")
                ,
                method = "REML", level = 95)



# bmp("Know Att Conf combined Forest plot.bmp", width = 1200 , height = 1200)
# 
# forest(all.meta,  main = "",
#        xlab = "Odds ratio: forming a tie",
#        transf = exp,
#        refline = 1,
#        rows=c(2:11,14:23,26:35), # Which rows to put estimates - leave space for the subgroup diamonds and titles
#        top = 1, # How many blank rows at top of plot for a header
#        cex = 1.25, 
#        ylim = c(1,37),
#        xlim = c(0.5,1.5),  # Limit of whole plot
#        #       alim = c(-0.5,4.5), # Limit of forest axes
#        #       at = c(0,0.5,1,2,3.25,4), # Points on forest x axis
#        addfit = FALSE)
# 
# ### switch to bold font
# par(font=2)
# 
# text(0.5, c(36), pos=4, font = 2, cex=1.25, c("Confidence"))
# addpoly(conf.meta, row= 25 , cex=1.25,  mlab="", transf = exp)
# text(0.5, 25, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(conf.meta$I2, digits=1, format="f"), "%"))
# 
# par(font=2)
# text(0.5, c(24), pos=4, cex=1.25, c("Norms"))
# addpoly(att.meta  , row= 13, cex=1.25,  mlab="", transf = exp)
# text(0.5, 13, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(att.meta$I2, digits=1, format="f"), "%"))
# 
# par(font=2)
# text(0.5, c(12), pos=4, cex=1.25, c("Knowledge"))
# addpoly(know.meta, row= 1, cex=1.25,  mlab="", transf = exp)
# text(0.5, 1, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(know.meta$I2, digits=1, format="f"), "%"))
# dev.off()
# 
# ###########################


tiff("Figure 2 Know Att Conf combined Forest plot.tif", width = 1200 , height = 1200)

forest(all.meta,  main = "",
       xlab = "Odds ratio of forming a tie: below 1 favours homophily",
       transf = exp,
       refline = 1,
       rows=c(2:11,14:23,26:35), # Which rows to put estimates - leave space for the subgroup diamonds and titles
       top = 1, # How many blank rows at top of plot for a header
       cex = 1.25, 
       ylim = c(1,37),
       xlim = c(0.5,1.5),  # Limit of whole plot
       #       alim = c(-0.5,4.5), # Limit of forest axes
       #       at = c(0,0.5,1,2,3.25,4), # Points on forest x axis
       addfit = FALSE)

### switch to bold font
par(font=2)

text(0.5, c(36), pos=4, font = 2, cex=1.25, c("Confidence"))
addpoly(conf.meta, row= 25 , cex=1.25,  mlab="", transf = exp)
text(0.5, 25, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(conf.meta$I2, digits=1, format="f"), "%"))

par(font=2)
text(0.5, c(24), pos=4, cex=1.25, c("Norms"))
addpoly(att.meta  , row= 13, cex=1.25,  mlab="", transf = exp)
text(0.5, 13, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(att.meta$I2, digits=1, format="f"), "%"))

par(font=2)
text(0.5, c(12), pos=4, cex=1.25, c("Knowledge"))
addpoly(know.meta, row= 1, cex=1.25,  mlab="", transf = exp)
text(0.5, 1, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(know.meta$I2, digits=1, format="f"), "%"))
dev.off()

###########################
