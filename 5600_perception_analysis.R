rm(list = ls())

#################
#               #
#      Name     #
#               #
#################
# Mark McCann created the file

#############
#  Purpose  #
#############


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
library(dplyr)
library(sna)
library(network)
library(miceadds)
library(ggplot2)
library(purrr)
library(psych)

#########################
#                       #
#     Load functions    #
#                       #
#########################


pool.regs <-
  function(input.results = NULL) {
    leng <- length(input.results)
    #Two tables for coefs and se
    mice.coef.table <-
      matrix(0,
             nc = leng + 1,
             nr = length(input.results[[1]]$coefficients))
    mice.se.table <- mice.coef.table
    #Table for results of rubin's rules pool
    mice.pooled.est     <-
      data.frame(matrix(
        NA,
        nr = length(input.results[[1]]$coefficients),
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
    for (x in 1:length(input.results[[1]]$coef)) {
      mice.pooled.est[x, 1] <-
        names(input.results[[1]]$coefficients)[x]
      #    mice.coef.table[x,1] <- names(input.results[[1]][[1]]$coefficients)[x]
    }
    rownames(mice.coef.table) <-
      names(input.results[[1]]$coefficients)
    rownames(mice.se.table)   <-
      names(input.results[[1]]$coefficients)
    
    for (i in names(input.results[[1]]$coefficients)) {
      for (iterpos in 1:20) {
        col.id <- iterpos + 1
        mice.coef.table[i, col.id] <-
          input.results[[iterpos]]$coefficients[i]
        mice.se.table[i, col.id] <-
          coef(summary(input.results[[iterpos]]))[, "Std. Error"][i]
      }
    }
    mice.coef.table[, 2:20] <- as.numeric(mice.coef.table[, 2:20])
    
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


#########################
#                       #
#  Main body of script  #
#                       #
#########################

load("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/ergm_data_control_imputed_5501.rdata")

##Table for sex perception and neighbourhood reported behaviour 
# percep.df <- matrix( nr = 14220,nc = 8 )
# 
# colnames(percep.df) <- c("imp_ID","schoolid","vertex_id","out_neighsize","all_neighsize","out_prop","all_prop", "sex.percep.var")

##Counter to move each data point down the data frame
# counter <- 1
####For each node position in the network

percep.df.imp.list <- list()

for (impid in c(1:20)) {
  ##Table for sex perception and neighbourhood reported behaviour
  percep.df <- matrix(nr = 735, nc = 9)
  colnames(percep.df) <-
    c(
      "imp_ID",
      "schoolid",
      "vertex_id",
      "sex.active",
      "out_neighsize",
      "all_neighsize",
      "out_prop",
      "all_prop",
      "sex.percep.var"
    )
  counter <- 1
  for (schoolid in c(1:3, 5:6)) {
    for (vertexid in 1:length(get.vertex.attribute(ergm.data.control.imputed[[impid]][[schoolid]], 'sex.percep.var'))) {
      #Find the neighbourhood i.e. nodes connected to that node
      #Outgoing only refers only to those reported as friends. Limited to six so less variability
      out.neigh <-
        get.neighborhood(ergm.data.control.imputed[[impid]][[schoolid]], vertexid, "out")
      
      #Combined is ingoing nominations as well, may be less visible to the ego but higher number so more variability.
      #    Variability may be a drawback as indegree may affect the proportions active
      #    But on the other hand, those that have more interaction partners have different social sampling algorithms.
      all.neigh <-
        get.neighborhood(ergm.data.control.imputed[[impid]][[schoolid]], vertexid, "combined")
      ###Find sexually active status of the neighbourhood
      out.neigh.sex.status <-
        get.vertex.attribute(ergm.data.control.imputed[[impid]][[schoolid]], 'sex.active')[out.neigh]
      all.neigh.sex.status <-
        get.vertex.attribute(ergm.data.control.imputed[[impid]][[schoolid]], 'sex.active')[all.neigh]
      #compute proportion active
      out.neigh.prop.active <-
        sum(out.neigh.sex.status) / length(out.neigh.sex.status)
      all.neigh.prop.active <-
        sum(all.neigh.sex.status) / length(all.neigh.sex.status)

      percep.df[counter, "imp_ID"]           <- impid
      percep.df[counter, "schoolid"]         <- schoolid
      percep.df[counter, "vertex_id"]        <- vertexid
      percep.df[counter, "sex.active"]        <-
        get.vertex.attribute(ergm.data.control.imputed[[impid]][[schoolid]], 'sex.active')[vertexid]
      percep.df[counter, "out_neighsize"]    <-
        length(out.neigh.sex.status)
      percep.df[counter, "all_neighsize"]    <-
        length(all.neigh.sex.status)
      percep.df[counter, "out_prop"]         <- out.neigh.prop.active
      percep.df[counter, "all_prop"]         <- all.neigh.prop.active
      percep.df[counter, "sex.percep.var"]   <-
      get.vertex.attribute(ergm.data.control.imputed[[impid]][[schoolid]], 'sex.percep.var')[vertexid]
      counter <- counter + 1
      #####Need to add the control schools data in as well
    }}
  percep.df.imp.list[[impid]] <- data.frame(percep.df)
  
  #label vars
  percep.df.imp.list[[impid]]$sex.percep.var <-
    factor(
      percep.df.imp.list[[impid]]$sex.percep.var,
      levels = c(1, 2, 3, 4),
      labels = c("All or most", "Some", "A few", "None")
    )
}


###############
##   Plots  ###
###############

setwd("T:/projects/stash_trial/09 STASH SNA/DisseminationAndImpact/Manuscripts_Papers/Soc Sex Dev Paper")

pdf("Perception Violin Plots.pdf")
for (i in 1:20){
all.prop.plot <-
  ggplot(data = percep.df.imp.list[[i]],
         aes(x = sex.percep.var, y = all_prop, fill = sex.percep.var)) +
  geom_violin() +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none", plot.title = element_text(hjust= 0.5)) +
  labs(y = "Proportion of sexually active in friendship network (all nominations)",
       x = "How many of your friends are having sex?",
       title= paste0("Percentage of sexually active peers and perceived peer sexual activity \n Imputation",i,""))
  
  print(all.prop.plot)
}
dev.off()

# out.prop.plot <-
#   ggplot(data = percep.df.imp.list[[1]],
#          aes(x = sex.percep.var, y = out_prop, fill = sex.percep.var)) +
#   geom_violin() +
#   scale_fill_brewer(palette = "Set1") +
#   theme(legend.position = "none") +
#   labs(y = "Proportion of sexually active in friendship network (self nominated)",
#        x = "How many of your friends are having sex?")
# print(out.prop.plot)



# ggplot(data = percep.df.imp.list[[1]], aes(all_prop)) +
#   geom_density()
# 
# ggplot(data = percep.df.imp.list[[1]], aes(out_prop)) +
#   geom_density()

pdf("Perception Bar charts.pdf")
for (i in 1:20) {
  per.bars <-
    ggplot(data = percep.df.imp.list[[i]], aes(all_prop, fill = as.factor(sex.percep.var))) +
    geom_bar() +
    scale_fill_brewer(palette = "Set1") +
    theme(legend.position = "none")
  print(per.bars)
}
dev.off()

stat <- percep.df.imp.list[[1]][,c("out_neighsize","all_neighsize","out_prop","all_prop","sex.percep.var")]
describeBy(stat, group="sex.percep.var")



lm(percep.df.imp.list[[1]]$all_prop ~ percep.df.imp.list[[1]]$sex.percep.var)


percep.prop.reg <- percep.df.imp.list %>% 
  map(~ lm(all_prop ~ sex.percep.var-1 , data = .))


pooled.reg <- pool.regs(percep.prop.reg)
pooled.reg

write.csv(pooled.reg, file = "T:/projects/stash_trial/09 STASH SNA/DisseminationAndImpact/Manuscripts_Papers/Soc Sex Dev Paper/control pooled regression pcent active perceived active.csv")




