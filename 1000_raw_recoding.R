rm(list = ls())

#################
#               #
#      Name     #
#               #
#################

# 1000 atribute files

# Robertson centre for Biostatistics developed this script
# Chiara Broccatelli and Mark McCann modified 


#############
#  Purpose  #
#############

# Loads raw data and recodes variables

#########################
#                       #
#    Load packages      #
#                       #
#########################
library(zoo)


#########################
#                       #
#     Load functions    #
#                       #
#########################

## Function to apply the mappings to a column to replace numbers with text
num.text.map <- function(incol, inmap, outfact=FALSE){
  stopifnot((incol %in% inmap[,1]) | is.na(incol))
  outcol <- rep(NA, length(incol))
  
  for(inmap.loop in 1:nrow(inmap)){
    curr.inp.lev  <- inmap[inmap.loop, 1]
    curr.out.lev  <- inmap[inmap.loop, 2]
    curr.cond     <- incol == curr.inp.lev
    outcol[curr.cond] <- curr.out.lev
  }
  
  ## Define as a factor if so specified. Use (unique) order of inmap to define order of levels
  if(outfact == TRUE){
    outcol <- factor(outcol, levels=unique(inmap[, 2]))
  }              
  
  return(outcol)
}

#########################
#                       #
#  Main body of script  #
#                       #
#########################

raw.sch.listB <- list()

########################
raw.sch.listB[[2]] <- read.csv("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/baseline_Schools/REDACTED.csv", stringsAsFactors = FALSE)
raw.sch.listB[[2]]$respondent_school <- 2

raw.sch.listB[[5]] <- read.csv("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/baseline_Schools/REDACTED.csv", stringsAsFactors = FALSE)
raw.sch.listB[[5]]$respondent_school <- 5

raw.sch.listB[[3]] <- read.csv("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/baseline_Schools/REDACTED.csv", stringsAsFactors = FALSE)
raw.sch.listB[[3]]$respondent_school <- 3

raw.sch.listB[[1]] <- read.csv("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/baseline_Schools/REDACTED.csv", stringsAsFactors = FALSE)
raw.sch.listB[[1]]$respondent_school <- 1

raw.sch.listB[[4]] <- read.csv("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/baseline_Schools/REDACTED.csv", stringsAsFactors = FALSE)
raw.sch.listB[[4]]$respondent_school <- 4

raw.sch.listB[[6]] <- read.csv("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/baseline_Schools/REDACTED.csv", stringsAsFactors = FALSE)
raw.sch.listB[[6]]$respondent_school <- 6


####Control Schools
raw.sch.listC <- list()
setwd("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/Control_Schools")

raw.sch.listC[[2]] <- read.csv("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/Control_Schools/REDACTED.csv", stringsAsFactors = FALSE)
raw.sch.listC[[2]]$respondent_school <- 2

raw.sch.listC[[5]] <- read.csv("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/Control_Schools/REDACTED.csv", stringsAsFactors = FALSE)
raw.sch.listC[[5]]$respondent_school <- 5

raw.sch.listC[[3]] <- read.csv("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/Control_Schools/REDACTED.csv", stringsAsFactors = FALSE)
raw.sch.listC[[3]]$respondent_school <- 3

raw.sch.listC[[1]] <- read.csv("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/Control_Schools/REDACTED.csv", stringsAsFactors = FALSE)
raw.sch.listC[[1]]$respondent_school <- 1

raw.sch.listC[[4]] <- read.csv("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/Control_Schools/REDACTED.csv", stringsAsFactors = FALSE)
raw.sch.listC[[4]]$respondent_school <- 4

raw.sch.listC[[6]] <- read.csv("T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/Control_Schools/REDACTED.csv", stringsAsFactors = FALSE)
raw.sch.listC[[6]]$respondent_school <- 6


####Combine the files
raw.baseline <- rbind.data.frame(raw.sch.listB[[1]],raw.sch.listB[[2]],raw.sch.listB[[3]],
                                 raw.sch.listB[[4]],raw.sch.listB[[5]],raw.sch.listB[[6]])


raw.control <- rbind.data.frame(raw.sch.listC[[1]],raw.sch.listC[[2]],raw.sch.listC[[3]],
                         raw.sch.listC[[4]],raw.sch.listC[[5]],raw.sch.listC[[6]])


###############################	
#	 BASELINE DATA 		#
###############################

df <- raw.baseline

recode.vars.baseline <- function(df = NULL){

# tidy data starting with dfline
df$id <- df$respondent_id # it was df$respondent_school but I think it's incorrect
df$date_complete <- as.Date(substr(df$last_updated,1,10),format="%d/%m/%Y") 
df$school <- df$respondent_school.1
df$gender<- ifelse(df$q_1==0,NA,df$q_1)
df$gender_other <- df$q_1_other

df$mnth<- df$q_2_month
df$yr<- as.character(df$q_2_year)
df$mnth<- tolower(df$q_2_month)
df$mnth <- as.numeric(df$mnth)
df$mnth <- ifelse(df$mnth==0,NA,df$mnth)
df$yr <- ifelse(df$yr=="0",NA,df$yr)


df$mnthyr<-paste(df$mnth,"-",df$yr,sep="")
df$mnthyr<-as.yearmon(df$mnthyr,"%m-%Y")

df$postcode <- ifelse(df$q_3==0,NA,
                        ifelse(df$q_3==1,1,2))

df$postcode_simd <- ifelse(df$q_3_simd_2016_d==0,NA,df$q_3_simd_2016_d)
df$askschool <- df$q_3_ask_school
df$askschool <- ifelse(df$askschool==0,NA,df$askschool)				
df$reside <- ifelse(df$q_4==0,NA,df$q_4)
df$fsm <- ifelse(df$q_5==0,NA,df$q_5)
df$ethnic <- ifelse(df$q_6==0,NA,df$q_6)

df$exam_nat4 <- as.numeric(as.character(df$q_7_nat_4))
df$exam_nat4[is.na(df$exam_nat4)]<- 0
df$exam_nat4<- ifelse(df$exam_nat4>10,NA,df$exam_nat4)

df$exam_nat5 <- as.numeric(as.character(df$q_7_nat_5))
df$exam_nat5[is.na(df$exam_nat5)]<- 0
df$exam_nat5<- ifelse(df$exam_nat5>10,NA,df$exam_nat5)


df$exam_other <- as.numeric(as.character(df$q_7_other_number))
df$exam_other[is.na(df$exam_other)]<- 0
df$exam_other<- ifelse(df$exam_other>10,NA,df$exam_other)


df$other_examtext <- df$q_7_other_text
df$other_examtext[is.na(df$q_7_other_text)]<- NA

for (i in 1:nrow(df)){
  df$exam[i] <- sum(df$exam_nat4[i], df$exam_nat5[i], df$exam_other[i],na.rm = TRUE) }
df$exam<- ifelse(df$exam<0 | df$exam>10,NA,df$exam)

df$leave <- ifelse(df$q_8==0,NA,df$q_8)
df$relig <- ifelse(df$q_9==0,NA,df$q_9)

df$EWopt<- ifelse(df$q_10_a==0,NA,df$q_10_a)
df$EWuse<- ifelse(df$q_10_b==0,NA,df$q_10_b)
df$EWrlx<- ifelse(df$q_10_c==0,NA,df$q_10_c)
df$EWprob<- ifelse(df$q_10_d==0,NA,df$q_10_d)
df$EWthink<- ifelse(df$q_10_e==0,NA,df$q_10_e)
df$EWclose<- ifelse(df$q_10_f==0,NA,df$q_10_f)
df$EWmind<- ifelse(df$q_10_g==0,NA,df$q_10_g)

df$SWEMWBS <- (as.numeric(as.character(df$q_10_a)) + as.numeric(as.character(df$q_10_b))
                 + as.numeric(as.character(df$q_10_c)) + as.numeric(as.character(df$q_10_d)) + as.numeric(as.character(df$q_10_e)) 
                 + as.numeric(as.character(df$q_10_f)) + as.numeric(as.character(df$q_10_g)))
x <- df$SWEMWBS
x <- ifelse(x < 7, NA,
            ifelse(x == 7, 7.00,
                   ifelse(x == 8, 9.51,
                          ifelse(
                            x == 9, 11.25,
                            ifelse(x == 10, 12.40,
                                   ifelse(
                                     x == 11, 13.33,
                                     ifelse(x == 12, 14.08,
                                            ifelse(
                                              x == 13, 14.75,
                                              ifelse(x ==
                                                       14, 15.32,
                                                     ifelse(
                                                       x == 15, 15.84,
                                                       ifelse(x ==
                                                                16, 16.36,
                                                              ifelse(
                                                                x == 17, 16.88,
                                                                ifelse(x ==
                                                                         18, 17.43,
                                                                       ifelse(
                                                                         x == 19, 17.98,
                                                                         ifelse(x ==
                                                                                  20, 18.59,
                                                                                ifelse(
                                                                                  x == 21, 19.25,
                                                                                  ifelse(x ==
                                                                                           22, 19.98,
                                                                                         ifelse(
                                                                                           x == 23, 20.73,
                                                                                           ifelse(x ==
                                                                                                    24, 21.54,
                                                                                                  ifelse(
                                                                                                    x == 25, 22.35,
                                                                                                    ifelse(x ==
                                                                                                             26, 23.21,
                                                                                                           ifelse(
                                                                                                             x == 27, 24.11,
                                                                                                             ifelse(x ==
                                                                                                                      28, 25.03,
                                                                                                                    ifelse(
                                                                                                                      x == 29, 26.02,
                                                                                                                      ifelse(x ==
                                                                                                                               30, 27.03,
                                                                                                                             ifelse(
                                                                                                                               x == 31, 28.13,
                                                                                                                               ifelse(x ==
                                                                                                                                        32, 29.31,
                                                                                                                                      ifelse(
                                                                                                                                        x == 33, 30.70,
                                                                                                                                        ifelse(x ==
                                                                                                                                                 34, 32.55,
                                                                                                                                               ifelse(x ==
                                                                                                                                                        35, 35.00, NA))
                                                                                                                                      ))
                                                                                                                             ))
                                                                                                                    ))
                                                                                                           ))
                                                                                                  ))
                                                                                         ))
                                                                                ))
                                                                       ))
                                                              ))
                                                     ))
                                            ))
                                   ))
                          ))))
df$SWEMWBS <- x


df$peersex<- ifelse(df$q_11_a==0,NA,df$q_11_a)
df$peerlaw<- ifelse(df$q_11_b==0,NA,df$q_11_b)
df$peeralc<- ifelse(df$q_11_c==0,NA,df$q_11_c)
df$peercan<- ifelse(df$q_11_d==0,NA,df$q_11_d)

df$climfair<- ifelse(df$q_12_a==0,NA,df$q_12_a)
df$climtalk<- ifelse(df$q_12_b==0,NA,df$q_12_b)
df$climval<- ifelse(df$q_12_c==0,NA,df$q_12_c)
df$climtrust<- ifelse(df$q_12_d==0,NA,df$q_12_d)
df$climenc<- ifelse(df$q_12_e==0,NA,df$q_12_e)
df$climclose<- ifelse(df$q_12_f==0,NA,df$q_12_f)
df$climtry<- ifelse(df$q_12_g==0,NA,df$q_12_g)


df$parhome<- df$q_13_a
df$parhome[df$parhome==0] <- NA
df$pardet<- df$q_13_b
df$pardet[df$pardet==0] <- NA
df$parcheck<- df$q_13_c
df$parcheck[df$parcheck==0] <- NA

df$attract<- df$q_14
df$ident<- df$q_15

df$regford<- ifelse(df$q_16_a==0,NA,df$q_16_a)
df$regwant<- ifelse(df$q_16_b==0,NA,df$q_16_b)
df$regres<- ifelse(df$q_16_c==0,NA,df$q_16_c)

df$smimp<- ifelse(df$q_17_a==0,NA,df$q_17_a)
df$smout<- ifelse(df$q_17_b==0,NA,df$q_17_b)

df$chabod <- ifelse(df$q_18_a==0,NA,df$q_18_a)
df$charel <- ifelse(df$q_18_b==0,NA,df$q_18_b)
df$chaready <- ifelse(df$q_18_c==0,NA,df$q_18_c)
df$chasti <- ifelse(df$q_18_d==0,NA,df$q_18_d)
df$chasxt <- ifelse(df$q_18_e==0,NA,df$q_18_e)

df$melike<- ifelse(df$q_20_a==0,NA,df$q_20_a)
df$melook<- ifelse(df$q_20_b==0,NA,df$q_20_b)
df$esteem<- ifelse(df$q_20_c==0,NA,df$q_20_c)

df$sexest<- ifelse(df$q_21==0,NA,df$q_21)



df$knwcdm<- ifelse(df$q_22_a==0,NA,df$q_22_a)
df$knwsti<- ifelse(df$q_22_b==0,NA,df$q_22_b)
df$knwlaw<- ifelse(df$q_22_c==0,NA,df$q_22_c)
df$knwdoc<- ifelse(df$q_22_d==0,NA,df$q_22_d)
df$knwcum<- ifelse(df$q_22_e==0,NA,df$q_22_e)




df$opnude<- ifelse(df$q_23_a==0,NA,df$q_23_a)
df$oporn<- ifelse(df$q_23_b==0,NA,df$q_23_b)
df$opagree<- ifelse(df$q_23_c==0,NA,df$q_23_c)
df$opdrunk<- ifelse(df$q_23_d==0,NA,df$q_23_d)
df$oprisk<- ifelse(df$q_23_e==0,NA,df$q_23_e)


df$talkpar<- ifelse(df$q_24_a==0,NA,df$q_24_a)
df$talkfrend <- ifelse(df$q_24_b==0,NA,df$q_24_b)

df$conget<- ifelse(df$q_25_a==0,NA,df$q_25_a)
df$conput<- ifelse(df$q_25_b==0,NA,df$q_25_b)
df$conref<- ifelse(df$q_25_c==0,NA,df$q_25_c)


df$kiss<- ifelse(df$q_26_a==0,NA,df$q_26_a)
df$gent<- ifelse(df$q_26_b==0,NA,df$q_26_b)
df$mstbte<- ifelse(df$q_26_c==0,NA,df$q_26_c)

df$oralyes<- df$q_27
df$oralyes[df$oralyes==0]<-NA

df$oralwhy<- df$q_28
df$oralwhy[df$oralwhy==0] <- NA
df$oralwhyother<- df$q_28_other
df$oralwhyother[df$oralwhyother==""] <-NA
df$oralwhyother <- factor(df$oralwhyother)


df$oralwhn<- df$q_29
df$oralwhn[df$oralwhn==0] <- NA
df$oral1st<- df$q_30
df$oral1st[df$Oral1st==0] <- NA


df$oralgen<- df$q_31
df$oralgen[df$oralgen==0] <- NA

df$oralstcdm<- df$q_32
df$oralstcdm[df$oralstcdm==0]<-NA

df$oralcdmwhy<- df$q_33
df$oralcdmwhy[df$oralcdmwhy==0]<-NA
#df$oralcdmwhyother<- df$q_33_other
#df$oralcdmwhyother[df$oralcdmwhyother==""] <-NA
#df$oralcdmwhyother<-factor(df$oralcdmwhyother)

df$oralstpart<- df$q_34
df$oralstpart[df$oralstpart==0]<-NA

df$intyes<- df$q_35
df$intyes[df$intyes==0]<-NA

df$intwhy<- df$q_36
df$intwhy[df$intwhy==0]<-NA
df$intwhyother <- df$q_36_other
df$intwhyother[df$intwhyother==""] <-NA
df$intwhyother<-factor(df$intwhyother)


df$intwhn<- df$q_37
df$intwhn[df$intwhn==0]<-NA

df$int1st<- df$q_38
df$int1st[df$int1st==0]<-NA

df$int3mcfreq<- df$q_39
df$int3mcfreq[df$int3mcfreq==0]<-NA

df$intlastcdm<- df$q_40
df$intlastcdm[df$intlastcdm==0]<-NA

df$intcdmwhy<- df$q_41
df$intcdmwhy[df$intcdmwhy==0]<-NA
df$intcdmwhyother <- df$q_41_other

df$intlstwill<- df$q_42

df$intlstwill[df$intlstwill==0] <- NA

df$intlast_regret <- df$q_43
#df$intlast_regret[df$intlast_regret==0] <- NA

#df$intlast_drunk <- df$q_44
#df$intlast_drunk[df$intlast_drunk==0] <- NA

df$intlastgend<- df$q_45
df$intlastgend[df$intlastgend==0]<-NA

#df$CHUwor<- df$q_46_1
#df$CHUwor[df$CHUwor==0]<-NA
#df$CHUsad<- df$q_46_2
#df$CHUsad[df$CHUsad==0]<-NA
#df$CHUpain<- df$q_46_3
#df$CHUpain[df$CHUpain==0]<-NA
#df$CHUtire<- df$q_46_4
#df$CHUtire[df$CHUtire==0]<-NA
#df$CHUanoy<- df$q_46_5
#df$CHUanoy[df$CHUanoy==0]<-NA
#df$CHUwork<- df$q_46_6
#df$CHUwork[df$CHUwork==0]<-NA
#df$CHUslp<- df$q_46_7
#df$CHUslp[df$CHUslp==0]<-NA
#df$CHUrout<- df$q_46_8
#df$CHUrout[df$CHUrout==0]<-NA
#df$CHUact<- df$q_46_9
#df$CHUact[df$CHUact==0]<-NA

#df$cdmYN<- ifelse(df$q_47_a==0,NA,df$q_47_a)
#df$cdmwher <- df$q_47_a_where
#df$cdmwher[df$cdmwher ==""] <-NA
#df$cdmwher[df$cdmwher ==" "] <-NA
#df$cdmwher <- factor(df$cdmwher)

#df$advYN<- ifelse(df$q_47_b==0,NA,df$q_47_b)
#df$advwher <- df$q_47_b_where
#df$advwher [df$advwher ==""] <-NA
#df$advwher [df$advwher ==" "] <-NA
#df$advwher [df$advwher =="\""] <-NA
#df$advwher <- factor(df$advwher )


df$relyes<- df$q_48
df$relyes[df$relyes==0]<-NA

df$rellng<- df$q_49
df$rellng[df$rellng==0]<-NA

#df$rqfeel<- ifelse(df$q_50_a==0,NA,df$q_50_a)
#df$rqhap<- ifelse(df$q_50_b==0,NA,df$q_50_b)
#df$rqtalk<- ifelse(df$q_50_c==0,NA,df$q_50_c)
#df$rqres<- ifelse(df$q_50_d==0,NA,df$q_50_d)
#df$rqang<- ifelse(df$q_50_e==0,NA,df$q_50_e)
#df$rqpers<- ifelse(df$q_50_f==0,NA,df$q_50_f)
#df$rqjeal<- ifelse(df$q_50_g==0,NA,df$q_50_g)

df$relgen<- df$q_51
df$relgen[df$relgen==0]<-NA

df$relS4<- df$q_52
df$relS4[df$relS4==0]<-NA

df$prellng<- df$q_53
df$prellng[df$prellng==0]<-NA

#df$prqfeel<- ifelse(df$q_54_a==0,NA,df$q_54_a)
#df$prqhap<- ifelse(df$q_54_b==0,NA,df$q_54_b)
#df$pretalk<- ifelse(df$q_54_c==0,NA,df$q_54_c)
#df$prqres<- ifelse(df$q_54_d==0,NA,df$q_54_d)
#df$prqang<- ifelse(df$q_54_e==0,NA,df$q_54_e)
#df$prqpers<- ifelse(df$q_54_f==0,NA,df$q_54_f)
#df$prqjeal<- ifelse(df$q_54_g==0,NA,df$q_54_g)

df$prelgen<- df$q_55
df$prelgen[df$prelgen==0]<-NA

#df$prelS4<- df$q_56
#df$prelS4[df$prelS4==0]<-NA

#df$releasy<- df$q_57
#df$releasy[df$releasy==0]<-NA

df$webinfo<- ifelse(df$q_58_a==0,NA,df$q_58_a)
df$webimag<- ifelse(df$q_58_b==0,NA,df$q_58_b)
df$webndsend<- ifelse(df$q_58_c==0,NA,df$q_58_c)
df$webndget<- ifelse(df$q_58_d==0,NA,df$q_58_d)
#df$webndask<- ifelse(df$q_58_e==0,NA,df$q_58_e)
#df$webndfor<- ifelse(df$q_58_f==0,NA,df$q_58_f)

#df$distress<- df$q_59
#df$distress[df$distress==0]<-NA

df$linkyes<- df$q_60
df$linkyes[df$linkyes==0]<-NA

df$viewtxt<-df$q_61
df$viewtxt[df$viewtxt==""] <-NA
df$viewtxt <- factor(df$viewtxt)


#####################################
# 	CHARACTER df		#
#####################################
# updated 3/7/18 - new label for 3rd category
df$gender_t<- factor(df$gender,levels=1:3,labels=c("Male","Female","Trans/Non-binary/Other"))

df$postcode_t<- factor(ifelse(is.na(df$postcode),NA,
                                ifelse(df$postcode==1,"Yes","No")))


df$reside_t<- factor(df$reside, levels=1:6,
                       labels=c("House/Flat from council","House/flat from someone else","House/flat owned by family","Care/Foster","Other","Dont know"))
df$fsm_t <- factor(ifelse(df$fsm==1,"Yes",
                            ifelse(df$fsm==2,"No",NA)))
df$ethnic_t<- factor(df$ethnic, levels=1:5,
                       labels=c("White Scottish/British","White but not Scottish/British","Asian","African/Caribbean/Black","Other/Mixed"))

df$leave_t<- factor(df$leave, levels=1:5,
                      labels=c("End S4","Christmas S5","End S5","End S6","Dont know"))

df$relig_t<- factor(df$relig,levels=1:4,labels=c("Very important","Quite Important","Not very important","Not at all important"))


map<- cbind(1:5,c("None of the time","Rarely","Some of the time","Often","All of the time"))
df$EWopt_t  <- num.text.map(incol=df$EWopt, inmap=map, outfact=T)
df$EWuse_t <- num.text.map(incol=df$EWuse, inmap=map, outfact=T)
df$EWrlx_t <- num.text.map(incol=df$EWrlx, inmap=map, outfact=T)
df$EWprob_t<- num.text.map(incol=df$EWprob, inmap=map, outfact=T)
df$EWthink_t <- num.text.map(incol=df$EWthink, inmap=map, outfact=T)
df$EWclose_t <- num.text.map(incol=df$EWclose, inmap=map, outfact=T)
df$EWmind_t <- num.text.map(incol=df$EWmind, inmap=map, outfact=T)



map<- cbind(1:4,c("All or most","Some","A few","None"))
df$peersex_t  <- num.text.map(incol=df$peersex, inmap=map, outfact=T)
df$peerlaw_t <- num.text.map(incol=df$peerlaw, inmap=map, outfact=T)
df$peeralc_t <- num.text.map(incol=df$peeralc, inmap=map, outfact=T)
df$peercan_t<- num.text.map(incol=df$peercan, inmap=map, outfact=T)


map<- cbind(1:4,c("Totally agree","Agree a bit","Don't really agree","Totally Disagree"))
df$climfair_t  <- num.text.map(incol=df$climfair, inmap=map, outfact=T)
df$climtalk_t <- num.text.map(incol=df$climtalk, inmap=map, outfact=T)
df$climval_t <- num.text.map(incol=df$climval, inmap=map, outfact=T)
df$climtrust_t<- num.text.map(incol=df$climtrust, inmap=map, outfact=T)
df$climenc_t <- num.text.map(incol=df$climenc, inmap=map, outfact=T)
df$climclose_t <- num.text.map(incol=df$climclose, inmap=map, outfact=T)
df$climtry_t<- num.text.map(incol=df$climtry, inmap=map, outfact=T)



map<- cbind(1:4,c("Never or almost never","Sometimes","Often","Always or almost always"))
df$parhome_t  <- num.text.map(incol=df$parhome, inmap=map, outfact=T)
df$pardet_t <- num.text.map(incol=df$pardet, inmap=map, outfact=T)
df$parcheck_t <- num.text.map(incol=df$parcheck, inmap=map, outfact=T)



df$attract_t<- factor(df$attract,levels=1:6,
                        labels=c("Only girls","More often girls, atleast once boy","Equal","More often boys,atleast once girls","Only boys","Never to anyone"))

df$ident_t<- factor(df$ident,levels=1:5,labels=c("Heterosexual/Straight","Gay or lesbian","Bisexual","Other","Rather not say"))


map<- cbind(1:5,c("Not at all true","Not very true","Neither true or untrue","Somewhat true","Really true"))
df$regford_t <- num.text.map(incol=df$regford, inmap=map, outfact=T)
df$regwant_t <- num.text.map(incol=df$regwant, inmap=map, outfact=T)
df$regres_t<- num.text.map(incol=df$regres, inmap=map, outfact=T)


map<- cbind(1:5,c("Strongly Agree","Agree","Neither agree not disagree","Disagree","Strongly disagree"))
df$smimp_t <- num.text.map(incol=df$smimp, inmap=map, outfact=T)
df$smout_t <- num.text.map(incol=df$smout, inmap=map, outfact=T)

map<- cbind(1:2,c("Yes","No"))
df$chabod_t <- num.text.map(incol=df$chabod , inmap=map, outfact=T)
df$charel_t<- num.text.map(incol=df$charel, inmap=map, outfact=T)
df$chaready_t <- num.text.map(incol=df$chaready , inmap=map, outfact=T)
df$chasti_t <- num.text.map(incol=df$chasti , inmap=map, outfact=T)
df$chasxt_t <- num.text.map(incol=df$chasxt , inmap=map, outfact=T)

map<- cbind(1:5,c("Strongly Agree","Agree","Unsure","Disagree","Strongly Disagree"))
df$melike_t <- num.text.map(incol=df$melike, inmap=map, outfact=T)
df$melook_t <- num.text.map(incol=df$melook, inmap=map, outfact=T)
df$esteem_t<- num.text.map(incol=df$esteem, inmap=map, outfact=T)


df$sexest_t<- factor(df$sexest,levels=1:5,
                       labels=c("None or a few","About a third","Half of them","Two thirds of them","Most or all of them"))

map<- cbind(1:3,c("True","False","Don't know"))
df$knwcdm_t  <- num.text.map(incol=df$knwcdm, inmap=map, outfact=T)
df$knwsti_t<- num.text.map(incol=df$knwsti, inmap=map, outfact=T)
df$knwlaw_t<- num.text.map(incol=df$knwlaw, inmap=map, outfact=T)
df$knwdoc_t<- num.text.map(incol=df$knwdoc, inmap=map, outfact=T)
df$knwcum_t <- num.text.map(incol=df$knwcum, inmap=map, outfact=T)

map<- cbind(1:5,c("Strongly agree","Agree","Neither agree or disagree","Disagree","Strongly disagree"))
df$opnude_t<- num.text.map(incol=df$opnude, inmap=map, outfact=T)
df$oporn_t<- num.text.map(incol=df$oporn, inmap=map, outfact=T)
df$opagree_t<- num.text.map(incol=df$opagree, inmap=map, outfact=T)
df$opdrunk_t<- num.text.map(incol=df$opdrunk, inmap=map, outfact=T)
df$oprisk_t<- num.text.map(incol=df$oprisk, inmap=map, outfact=T)

map<- cbind(1:4,c("Didnt talk","Easy","Varies","Difficult"))
df$talkpar_t <- num.text.map(incol=df$talkpar, inmap=map, outfact=T)
df$talkfrend_t<- num.text.map(incol=df$talkfrend, inmap=map, outfact=T)

map<- cbind(1:5,c("Very confident","Quite confident","Unsure","Not very confident","Not at all confident"))
df$conget_t<- num.text.map(incol=df$conget, inmap=map, outfact=T)
df$conput_t<- num.text.map(incol=df$conput, inmap=map, outfact=T)
df$conref_t <- num.text.map(incol=df$conref, inmap=map, outfact=T)

map<- cbind(1:3,c("Never","Yes, last 6 months","Yes, more than 6 months"))
df$kiss_t <- num.text.map(incol=df$kiss, inmap=map, outfact=T)
df$gent_t<- num.text.map(incol=df$gent, inmap=map, outfact=T)
df$mstbte_t<- num.text.map(incol=df$mstbte, inmap=map, outfact=T)

df$oralyes_t<- factor(df$oralyes,levels=1:2, labels=c("Yes","No"))

df$oralwhy_t<- factor(df$oralwhy,levels=1:9,
                        labels=c("Dont want to/Not interested","Not had opportunity","Waiting for steady relationship","Waiting until older","Worried about risks of STIs/pregnancy","Worried about risk to reputation","Beliefs/values","Nervous/Scared","Other"))

df$oralwhn_t<- factor(df$oralwhn,levels=1:2,labels=c("Last 6 months","More than 6 months"))
df$oral1st_t<- factor(df$oral1st,levels=1:2,labels=c("Yes","No"))

df$oralgen_t<- factor(df$oralgen,levels=1:3,labels=c("Male","Female","Trans/non-binary"))

df$oralstcdm_t<- factor(df$oralstcdm,levels=1:2,labels=c("Yes","No"))

df$oralcdmwhy_t<- factor(df$oralcdmwhy,levels=1:7,
                           labels=c("Got carried away and forgot","Did not have one to hand","I/my partner did not think it was necessary","I/my partner did not know how to use one","I/my partner did not want to use one","We had one but it broke","Other"))

df$oralstpart_t<- factor(df$oralstpart,levels=1:3,labels=c("Male","Female","Non-binary"))

df$intyes_t<- factor(df$intyes,levels=1:2,labels=c("Yes","No"))
df$intwhy_t<- factor(df$intwhy,levels=1:9,
                       labels=c("Dont want to/ not interested","Not had opportunity","Waiting for steady relationship","Waiting until older","Worried about risks of STIs/pregnancy","Worried about risk to reputation","Beliefs/values","Nervous/Scared","Other"))

df$intwhn_t<- factor(df$intwhn,levels=1:2,labels=c("Last 6 months","More than 6 months"))

df$int1st_t<- factor(df$int1st,levels=1:2,labels=c("Yes","No"))

df$int3mcfreq_t<- factor(df$int3mcfreq,levels=1:4,
                           labels=c("Always","Often","Sometimes","Never"))

df$intlastcdm_t<- factor(df$intlastcdm,levels=1:2,labels=c("Yes","No"))

df$intcdmwhy_t<- factor(df$intcdmwhy,levels=1:8,
                          labels=c("Got carried away and forgot","Did not have one to hand","I/my partner did not think it was necessary","I/my partner did not know how to use one","I/my partner did not want to use one","We had one but it broke","Both got tested for STIs & had another form of contraception","Other"))

df$intlstwill_t<- factor(df$intlstwill,levels=1:3,labels=c("Equally willing","I was more willing","They were more willing"))

# problems


#df$intlast_regret_t <- factor(df$intlast_regret , levels=1:4, labels=c("Not at all","A bit","Quite a lot","Very much"))

#df$intlast_drunk_t <- factor(df$intlast_drunk , levels=1:4, labels=c("No","Yes, a little bit","Yes, I was quite drunk","Very much"))

df$intlastgend_t<- factor(df$intlastgend,levels=1:3,labels=c("Male","Female","Non-binary/trans"))

#df$CHUwor_t<- factor(df$CHUwor,levels=1:5,labels=c("Dont feel worried","Little worried","Bit worried","Quite worried","Very worried"))
#df$CHUsad_t<- factor(df$CHUsad,levels=1:5,labels=c("Dont feel sad","Little sad","Bit sad","Quite sad","Very sad"))
#df$CHUpain_t<- factor(df$CHUpain,levels=1:5,labels=c("No pain","Little pain","Bit of pain","Quite a lot of pain","A lot of pain"))
#df$CHUtire_t<- factor(df$CHUtire,levels=1:5,labels=c("Dont feel tired","Little tired","Bit tired","Quite tired","Very tired"))
#df$CHUanoy_t<- factor(df$CHUanoy,levels=1:5,labels=c("Not annoyed","Little annoyed","Bit annoyed","Quite annoyed","Very annoyed"))
#df$CHUwork_t<- factor(df$CHUwork,levels=1:5,labels=c("No problems","Few problems","Some problems","Many problems","Can't do"))
#df$CHUslp_t<- factor(df$CHUslp,levels=1:5,labels=c("No problems","Few problems","Some problems","Many problems","Couldn't sleep"))
#df$CHUrout_t<- factor(df$CHUrout,levels=1:5,labels=c("No problems","Few problems","Some problems","Many problems","Can't do routine"))
#df$CHUact_t<- factor(df$CHUact,levels=1:5,labels=c("Any activity","Most activities","Some activities","Few activities","No activities"))

#map<- cbind(1:2,c("No","Yes"))
#df$cdmYN_t <- num.text.map(incol=df$cdmYN, inmap=map, outfact=T)
#df$advYN_t <- num.text.map(incol=df$advYN, inmap=map, outfact=T)

df$relyes_t<- factor(df$relyes,levels=1:3,labels=c("No, not in last 6mnths","Used to, not now","Yes, have one now"))

df$rellng_t<- factor(df$rellng,levels=1:5,
                       labels=c("Less than a month","Between one & three months","Between three & six months","Between six months & a year","Over a year"))

map<- cbind(1:5,c("All the time","Often","Sometimes","Not often","Never"))
#df$rqfeel_t  <- num.text.map(incol=df$rqfeel, inmap=map, outfact=T)
#df$rqhap_t<- num.text.map(incol=df$rqhap, inmap=map, outfact=T)
#df$rqtalk_t<- num.text.map(incol=df$rqtalk, inmap=map, outfact=T)
#df$rqres_t<- num.text.map(incol=df$rqres, inmap=map, outfact=T)
#df$rqang_t<- num.text.map(incol=df$rqang, inmap=map, outfact=T)
#df$rqpers_t<- num.text.map(incol=df$rqpers, inmap=map, outfact=T)
#df$rqjeal_t<- num.text.map(incol=df$rqjeal, inmap=map, outfact=T)

df$relgen_t<- factor(df$relgen,levels=1:3,labels=c("Male","Female","Trans/non-binary"))

df$relS4_t<- factor(df$relS4,levels=1:2,labels=c("Yes","No"))

df$prellng_t<- factor(df$prellng,levels=1:5,
                        labels=c("Less than a month","Between 1 & 3 months","Between 3 & 6 months ","Between 6 months & a year","Over one year"))

map<- cbind(1:5,c("All the time","Often","Sometimes","Not often","Never"))
#df$prqfeel_t  <- num.text.map(incol=df$prqfeel, inmap=map, outfact=T)
#df$prqhap_t<- num.text.map(incol=df$prqhap, inmap=map, outfact=T)
#df$pretalk_t<- num.text.map(incol=df$pretalk, inmap=map, outfact=T)
#df$prqres_t<- num.text.map(incol=df$prqres, inmap=map, outfact=T)
#df$prqang_t<- num.text.map(incol=df$prqang, inmap=map, outfact=T)
#df$prqpers_t<- num.text.map(incol=df$prqpers, inmap=map, outfact=T)
#df$prqjeal_t<- num.text.map(incol=df$prqjeal, inmap=map, outfact=T)

df$prelgen_t<- factor(df$prelgen,levels=1:3,labels=c("Male","Female","Trans/non-binary"))
#df$prelS4_t<- factor(df$prelS4,levels=1:2,labels=c("Yes","No"))
#df$releasy_t<- factor(df$releasy,levels=1:4,labels=c("Not relevant","Always easy","Sometimes easy, sometimes difficult","Always difficult"))

map<- cbind(1:4,c("Never","Once or twice","3-10times",">10 times"))
#df$webinfo_t  <- num.text.map(incol=df$webinfo, inmap=map, outfact=T)
#df$webimag_t<- num.text.map(incol=df$webimag, inmap=map, outfact=T)
#df$webndsend_t<- num.text.map(incol=df$webndsend, inmap=map, outfact=T)
#df$webndget_t<- num.text.map(incol=df$webndget, inmap=map, outfact=T)
#df$webndask_t<- num.text.map(incol=df$webndask, inmap=map, outfact=T)
#df$webndfor_t<- num.text.map(incol=df$webndfor, inmap=map, outfact=T)

#df$distress_t<- factor(df$distress,levels=1:5,labels=c("Agree Strongly","Agree","Neither agree nor disagree","Disagree","Disagree strongly"))

df$linkyes_t<- factor(df$linkyes,levels=1:2,labels=c("Yes","No"))

return(df)

}



#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################


df <- raw.control
recode.vars.control <- function(df = NULL){
  
  # tidy data starting with dfline
  df$id <- df$respondent_id # it was df$respondent_school but I think it's incorrect
  df$date_complete <- as.Date(substr(df$last_updated,1,10),format="%d/%m/%Y") 
  df$school <- df$respondent_school.1
  df$gender<- ifelse(df$q_1==0,NA,df$q_1)
  df$gender_other <- df$q_1_other
  
  df$mnth<- df$q_2_month
  df$yr<- as.character(df$q_2_year)
  df$mnth<- tolower(df$q_2_month)
  df$mnth <- as.numeric(df$mnth)
  df$mnth <- ifelse(df$mnth==0,NA,df$mnth)
  df$yr <- ifelse(df$yr=="0",NA,df$yr)
  
  
  df$mnthyr<-paste(df$mnth,"-",df$yr,sep="")
  df$mnthyr<-as.yearmon(df$mnthyr,"%m-%Y")
  
  df$postcode <- ifelse(df$q_3==0,NA,
                        ifelse(df$q_3==1,1,2))
  
  df$postcode_simd <- ifelse(df$q_3_simd_2016_d==0,NA,df$q_3_simd_2016_d)
  df$askschool <- df$q_3_ask_school
  df$askschool <- ifelse(df$askschool==0,NA,df$askschool)				
  df$reside <- ifelse(df$q_4==0,NA,df$q_4)
  df$fsm <- ifelse(df$q_5==0,NA,df$q_5)
  df$ethnic <- ifelse(df$q_6==0,NA,df$q_6)
  
  df$exam_nat4 <- as.numeric(as.character(df$q_7_nat_4))
  df$exam_nat4[is.na(df$exam_nat4)]<- 0
  df$exam_nat4<- ifelse(df$exam_nat4>10,NA,df$exam_nat4)
  
  df$exam_nat5 <- as.numeric(as.character(df$q_7_nat_5))
  df$exam_nat5[is.na(df$exam_nat5)]<- 0
  df$exam_nat5<- ifelse(df$exam_nat5>10,NA,df$exam_nat5)
  
  
  df$exam_other <- as.numeric(as.character(df$q_7_other_number))
  df$exam_other[is.na(df$exam_other)]<- 0
  df$exam_other<- ifelse(df$exam_other>10,NA,df$exam_other)
  
  
  df$other_examtext <- df$q_7_other_text
  df$other_examtext[is.na(df$q_7_other_text)]<- NA
  
  for (i in 1:nrow(df)){
    df$exam[i] <- sum(df$exam_nat4[i], df$exam_nat5[i], df$exam_other[i],na.rm = TRUE) }
  df$exam<- ifelse(df$exam<0 | df$exam>10,NA,df$exam)
  
  df$leave <- ifelse(df$q_8==0,NA,df$q_8)
  df$relig <- ifelse(df$q_9==0,NA,df$q_9)
  
  df$EWopt<- ifelse(df$q_10_a==0,NA,df$q_10_a)
  df$EWuse<- ifelse(df$q_10_b==0,NA,df$q_10_b)
  df$EWrlx<- ifelse(df$q_10_c==0,NA,df$q_10_c)
  df$EWprob<- ifelse(df$q_10_d==0,NA,df$q_10_d)
  df$EWthink<- ifelse(df$q_10_e==0,NA,df$q_10_e)
  df$EWclose<- ifelse(df$q_10_f==0,NA,df$q_10_f)
  df$EWmind<- ifelse(df$q_10_g==0,NA,df$q_10_g)
  
  df$SWEMWBS <- (as.numeric(as.character(df$q_10_a)) + as.numeric(as.character(df$q_10_b))
                 + as.numeric(as.character(df$q_10_c)) + as.numeric(as.character(df$q_10_d)) + as.numeric(as.character(df$q_10_e)) 
                 + as.numeric(as.character(df$q_10_f)) + as.numeric(as.character(df$q_10_g)))
  x <- df$SWEMWBS
  x <- ifelse(x < 7, NA,
              ifelse(x==7,7.00,
                     ifelse(x==8,9.51,
                            ifelse(x==9,11.25,
                                   ifelse(x==10,12.40,
                                          ifelse(x==11,13.33,
                                                 ifelse(x==12,14.08,
                                                        ifelse(x==13,14.75,
                                                               ifelse(x==14,15.32,				
                                                                      ifelse(x==15,15.84,
                                                                             ifelse(x==16,16.36,
                                                                                    ifelse(x==17,16.88,
                                                                                           ifelse(x==18,17.43,
                                                                                                  ifelse(x==19,17.98,
                                                                                                         ifelse(x==20,18.59,
                                                                                                                ifelse(x==21,19.25,
                                                                                                                       ifelse(x==22,19.98,
                                                                                                                              ifelse(x==23,20.73,
                                                                                                                                     ifelse(x==24,21.54,
                                                                                                                                            ifelse(x==25,22.35,
                                                                                                                                                   ifelse(x==26,23.21,
                                                                                                                                                          ifelse(x==27,24.11,
                                                                                                                                                                 ifelse(x==28,25.03,
                                                                                                                                                                        ifelse(x==29,26.02,
                                                                                                                                                                               ifelse(x==30,27.03,
                                                                                                                                                                                      ifelse(x==31,28.13,
                                                                                                                                                                                             ifelse(x==32,29.31,
                                                                                                                                                                                                    ifelse(x==33,30.70,
                                                                                                                                                                                                           ifelse(x==34,32.55,
                                                                                                                                                                                                                  ifelse(x==35,35.00,NA))))))))))))))))))))))))))))))
  df$SWEMWBS <- x
  
  
  df$peersex<- ifelse(df$q_11_a==0,NA,df$q_11_a)
  df$peerlaw<- ifelse(df$q_11_b==0,NA,df$q_11_b)
  df$peeralc<- ifelse(df$q_11_c==0,NA,df$q_11_c)
  df$peercan<- ifelse(df$q_11_d==0,NA,df$q_11_d)
  
  df$climfair<- ifelse(df$q_12_a==0,NA,df$q_12_a)
  df$climtalk<- ifelse(df$q_12_b==0,NA,df$q_12_b)
  df$climval<- ifelse(df$q_12_c==0,NA,df$q_12_c)
  df$climtrust<- ifelse(df$q_12_d==0,NA,df$q_12_d)
  df$climenc<- ifelse(df$q_12_e==0,NA,df$q_12_e)
  df$climclose<- ifelse(df$q_12_f==0,NA,df$q_12_f)
  df$climtry<- ifelse(df$q_12_g==0,NA,df$q_12_g)
  
  
  df$parhome<- df$q_13_a
  df$parhome[df$parhome==0] <- NA
  df$pardet<- df$q_13_b
  df$pardet[df$pardet==0] <- NA
  df$parcheck<- df$q_13_c
  df$parcheck[df$parcheck==0] <- NA
  
  df$attract<- df$q_14
  df$ident<- df$q_15
  
  df$regford<- ifelse(df$q_16_a==0,NA,df$q_16_a)
  df$regwant<- ifelse(df$q_16_b==0,NA,df$q_16_b)
  df$regres<- ifelse(df$q_16_c==0,NA,df$q_16_c)
  
  df$smimp<- ifelse(df$q_17_a==0,NA,df$q_17_a)
  df$smout<- ifelse(df$q_17_b==0,NA,df$q_17_b)
  
  df$chabod <- ifelse(df$q_18_a==0,NA,df$q_18_a)
  df$charel <- ifelse(df$q_18_b==0,NA,df$q_18_b)
  df$chaready <- ifelse(df$q_18_e==0,NA,df$q_18_e)
  df$chasti <- ifelse(df$q_18_g==0,NA,df$q_18_g)
  df$chasxt <- ifelse(df$q_18_i==0,NA,df$q_18_i)
  
  df$melike<- ifelse(df$q_20_a==0,NA,df$q_20_a)
  df$melook<- ifelse(df$q_20_b==0,NA,df$q_20_b)
  df$esteem<- ifelse(df$q_20_c==0,NA,df$q_20_c)
  
  df$sexest<- ifelse(df$q_21==0,NA,df$q_21)
  
  df$knwcdm<- ifelse(df$q_22_a==0,NA,df$q_22_a)
  df$knwsti<- ifelse(df$q_22_c==0,NA,df$q_22_c)
  df$knwlaw<- ifelse(df$q_22_d==0,NA,df$q_22_d)
  df$knwdoc<- ifelse(df$q_22_e==0,NA,df$q_22_e)
  df$knwcum<- ifelse(df$q_22_g==0,NA,df$q_22_g)
  
  
  df$opnude <- ifelse(df$q_23_b==0,NA,df$q_23_b)
  df$oporn  <- ifelse(df$q_23_e==0,NA,df$q_23_e)
  df$opagree<- ifelse(df$q_23_i==0,NA,df$q_23_i)
  df$opdrunk<- ifelse(df$q_23_k==0,NA,df$q_23_k)
  df$oprisk <- ifelse(df$q_23_l==0,NA,df$q_23_l)
  
  
  df$talkpar<- ifelse(df$q_24_a==0,NA,df$q_24_a)
  df$talkfrend <- ifelse(df$q_24_b==0,NA,df$q_24_b)
  
  df$conget<- ifelse(df$q_25_d==0,NA,df$q_25_d)
  df$conput<- ifelse(df$q_25_e==0,NA,df$q_25_e)
  df$conref<- ifelse(df$q_25_f==0,NA,df$q_25_f)
  
  
  df$kiss<- ifelse(df$q_26_a==0,NA,df$q_26_a)
  df$gent<- ifelse(df$q_26_b==0,NA,df$q_26_b)
  df$mstbte<- ifelse(df$q_26_c==0,NA,df$q_26_c)
  
  df$oralyes<- df$q_27
  df$oralyes[df$oralyes==0]<-NA
  
  df$oralwhy<- df$q_28
  df$oralwhy[df$oralwhy==0] <- NA
  df$oralwhyother<- df$q_28_other
  df$oralwhyother[df$oralwhyother==""] <-NA
  df$oralwhyother <- factor(df$oralwhyother)
  
  
  df$oralwhn<- df$q_29
  df$oralwhn[df$oralwhn==0] <- NA
  df$oral1st<- df$q_30
  df$oral1st[df$Oral1st==0] <- NA
  
  
  df$oralgen<- df$q_31
  df$oralgen[df$oralgen==0] <- NA
  
  df$oralstcdm<- df$q_32
  df$oralstcdm[df$oralstcdm==0]<-NA
  
  df$oralcdmwhy<- df$q_33
  df$oralcdmwhy[df$oralcdmwhy==0]<-NA
  #df$oralcdmwhyother<- df$q_33_other
  #df$oralcdmwhyother[df$oralcdmwhyother==""] <-NA
  #df$oralcdmwhyother<-factor(df$oralcdmwhyother)
  
  df$oralstpart<- df$q_34
  df$oralstpart[df$oralstpart==0]<-NA
  
  df$intyes<- df$q_35
  df$intyes[df$intyes==0]<-NA
  
  df$intwhy<- df$q_36
  df$intwhy[df$intwhy==0]<-NA
  df$intwhyother <- df$q_36_other
  df$intwhyother[df$intwhyother==""] <-NA
  df$intwhyother<-factor(df$intwhyother)
  
  
  df$intwhn<- df$q_37
  df$intwhn[df$intwhn==0]<-NA
  
  df$int1st<- df$q_38
  df$int1st[df$int1st==0]<-NA
  
  df$int3mcfreq<- df$q_39
  df$int3mcfreq[df$int3mcfreq==0]<-NA
  
  df$intlastcdm<- df$q_40
  df$intlastcdm[df$intlastcdm==0]<-NA
  
  df$intcdmwhy<- df$q_41
  df$intcdmwhy[df$intcdmwhy==0]<-NA
  df$intcdmwhyother <- df$q_41_other
  
  df$intlstwill<- df$q_42
  
  df$intlstwill[df$intlstwill==0] <- NA
  
  df$intlast_regret <- df$q_43
  #df$intlast_regret[df$intlast_regret==0] <- NA
  
  #df$intlast_drunk <- df$q_44
  #df$intlast_drunk[df$intlast_drunk==0] <- NA
  
  df$intlastgend<- df$q_45
  df$intlastgend[df$intlastgend==0]<-NA
  
  #df$CHUwor<- df$q_46_1
  #df$CHUwor[df$CHUwor==0]<-NA
  #df$CHUsad<- df$q_46_2
  #df$CHUsad[df$CHUsad==0]<-NA
  #df$CHUpain<- df$q_46_3
  #df$CHUpain[df$CHUpain==0]<-NA
  #df$CHUtire<- df$q_46_4
  #df$CHUtire[df$CHUtire==0]<-NA
  #df$CHUanoy<- df$q_46_5
  #df$CHUanoy[df$CHUanoy==0]<-NA
  #df$CHUwork<- df$q_46_6
  #df$CHUwork[df$CHUwork==0]<-NA
  #df$CHUslp<- df$q_46_7
  #df$CHUslp[df$CHUslp==0]<-NA
  #df$CHUrout<- df$q_46_8
  #df$CHUrout[df$CHUrout==0]<-NA
  #df$CHUact<- df$q_46_9
  #df$CHUact[df$CHUact==0]<-NA
  
  #df$cdmYN<- ifelse(df$q_47_a==0,NA,df$q_47_a)
  #df$cdmwher <- df$q_47_a_where
  #df$cdmwher[df$cdmwher ==""] <-NA
  #df$cdmwher[df$cdmwher ==" "] <-NA
  #df$cdmwher <- factor(df$cdmwher)
  
  #df$advYN<- ifelse(df$q_47_b==0,NA,df$q_47_b)
  #df$advwher <- df$q_47_b_where
  #df$advwher [df$advwher ==""] <-NA
  #df$advwher [df$advwher ==" "] <-NA
  #df$advwher [df$advwher =="\""] <-NA
  #df$advwher <- factor(df$advwher )
  
  
  df$relyes<- df$q_48
  df$relyes[df$relyes==0]<-NA
  
  df$rellng<- df$q_49
  df$rellng[df$rellng==0]<-NA
  
  #df$rqfeel<- ifelse(df$q_50_a==0,NA,df$q_50_a)
  #df$rqhap<- ifelse(df$q_50_b==0,NA,df$q_50_b)
  #df$rqtalk<- ifelse(df$q_50_c==0,NA,df$q_50_c)
  #df$rqres<- ifelse(df$q_50_d==0,NA,df$q_50_d)
  #df$rqang<- ifelse(df$q_50_e==0,NA,df$q_50_e)
  #df$rqpers<- ifelse(df$q_50_f==0,NA,df$q_50_f)
  #df$rqjeal<- ifelse(df$q_50_g==0,NA,df$q_50_g)
  
  df$relgen<- df$q_51
  df$relgen[df$relgen==0]<-NA
  
  df$relS4<- df$q_52
  df$relS4[df$relS4==0]<-NA
  
  df$prellng<- df$q_53
  df$prellng[df$prellng==0]<-NA
  
  #df$prqfeel<- ifelse(df$q_54_a==0,NA,df$q_54_a)
  #df$prqhap<- ifelse(df$q_54_b==0,NA,df$q_54_b)
  #df$pretalk<- ifelse(df$q_54_c==0,NA,df$q_54_c)
  #df$prqres<- ifelse(df$q_54_d==0,NA,df$q_54_d)
  #df$prqang<- ifelse(df$q_54_e==0,NA,df$q_54_e)
  #df$prqpers<- ifelse(df$q_54_f==0,NA,df$q_54_f)
  #df$prqjeal<- ifelse(df$q_54_g==0,NA,df$q_54_g)
  
  df$prelgen<- df$q_55
  df$prelgen[df$prelgen==0]<-NA
  
  #df$prelS4<- df$q_56
  #df$prelS4[df$prelS4==0]<-NA
  
  #df$releasy<- df$q_57
  #df$releasy[df$releasy==0]<-NA
  
  df$webinfo<- ifelse(df$q_58_a==0,NA,df$q_58_a)
  df$webimag<- ifelse(df$q_58_b==0,NA,df$q_58_b)
  df$webndsend<- ifelse(df$q_58_c==0,NA,df$q_58_c)
  df$webndget<- ifelse(df$q_58_d==0,NA,df$q_58_d)
  #df$webndask<- ifelse(df$q_58_e==0,NA,df$q_58_e)
  #df$webndfor<- ifelse(df$q_58_f==0,NA,df$q_58_f)
  
  #df$distress<- df$q_59
  #df$distress[df$distress==0]<-NA
  
  df$linkyes<- df$q_60
  df$linkyes[df$linkyes==0]<-NA
  
  df$viewtxt<-df$q_61
  df$viewtxt[df$viewtxt==""] <-NA
  df$viewtxt <- factor(df$viewtxt)
  
  
  #####################################
  # 	CHARACTER df		#
  #####################################
  # updated 3/7/18 - new label for 3rd category
  df$gender_t<- factor(df$gender,levels=1:3,labels=c("Male","Female","Trans/Non-binary/Other"))
  
  df$postcode_t<- factor(ifelse(is.na(df$postcode),NA,
                                ifelse(df$postcode==1,"Yes","No")))
  
  
  df$reside_t<- factor(df$reside, levels=1:6,
                       labels=c("House/Flat from council","House/flat from someone else","House/flat owned by family","Care/Foster","Other","Dont know"))
  df$fsm_t <- factor(ifelse(df$fsm==1,"Yes",
                            ifelse(df$fsm==2,"No",NA)))
  df$ethnic_t<- factor(df$ethnic, levels=1:5,
                       labels=c("White Scottish/British","White but not Scottish/British","Asian","African/Caribbean/Black","Other/Mixed"))
  
  df$leave_t<- factor(df$leave, levels=1:5,
                      labels=c("End S4","Christmas S5","End S5","End S6","Dont know"))
  
  df$relig_t<- factor(df$relig,levels=1:4,labels=c("Very important","Quite Important","Not very important","Not at all important"))
  
  
  map<- cbind(1:5,c("None of the time","Rarely","Some of the time","Often","All of the time"))
  df$EWopt_t  <- num.text.map(incol=df$EWopt, inmap=map, outfact=T)
  df$EWuse_t <- num.text.map(incol=df$EWuse, inmap=map, outfact=T)
  df$EWrlx_t <- num.text.map(incol=df$EWrlx, inmap=map, outfact=T)
  df$EWprob_t<- num.text.map(incol=df$EWprob, inmap=map, outfact=T)
  df$EWthink_t <- num.text.map(incol=df$EWthink, inmap=map, outfact=T)
  df$EWclose_t <- num.text.map(incol=df$EWclose, inmap=map, outfact=T)
  df$EWmind_t <- num.text.map(incol=df$EWmind, inmap=map, outfact=T)
  
  
  
  map<- cbind(1:4,c("All or most","Some","A few","None"))
  df$peersex_t  <- num.text.map(incol=df$peersex, inmap=map, outfact=T)
  df$peerlaw_t <- num.text.map(incol=df$peerlaw, inmap=map, outfact=T)
  df$peeralc_t <- num.text.map(incol=df$peeralc, inmap=map, outfact=T)
  df$peercan_t<- num.text.map(incol=df$peercan, inmap=map, outfact=T)
  
  
  map<- cbind(1:4,c("Totally agree","Agree a bit","Don't really agree","Totally Disagree"))
  df$climfair_t  <- num.text.map(incol=df$climfair, inmap=map, outfact=T)
  df$climtalk_t <- num.text.map(incol=df$climtalk, inmap=map, outfact=T)
  df$climval_t <- num.text.map(incol=df$climval, inmap=map, outfact=T)
  df$climtrust_t<- num.text.map(incol=df$climtrust, inmap=map, outfact=T)
  df$climenc_t <- num.text.map(incol=df$climenc, inmap=map, outfact=T)
  df$climclose_t <- num.text.map(incol=df$climclose, inmap=map, outfact=T)
  df$climtry_t<- num.text.map(incol=df$climtry, inmap=map, outfact=T)
  
  
  
  map<- cbind(1:4,c("Never or almost never","Sometimes","Often","Always or almost always"))
  df$parhome_t  <- num.text.map(incol=df$parhome, inmap=map, outfact=T)
  df$pardet_t <- num.text.map(incol=df$pardet, inmap=map, outfact=T)
  df$parcheck_t <- num.text.map(incol=df$parcheck, inmap=map, outfact=T)
  
  
  
  df$attract_t<- factor(df$attract,levels=1:6,
                        labels=c("Only girls","More often girls, atleast once boy","Equal","More often boys,atleast once girls","Only boys","Never to anyone"))
  
  df$ident_t<- factor(df$ident,levels=1:5,labels=c("Heterosexual/Straight","Gay or lesbian","Bisexual","Other","Rather not say"))
  
  
  map<- cbind(1:5,c("Not at all true","Not very true","Neither true or untrue","Somewhat true","Really true"))
  df$regford_t <- num.text.map(incol=df$regford, inmap=map, outfact=T)
  df$regwant_t <- num.text.map(incol=df$regwant, inmap=map, outfact=T)
  df$regres_t<- num.text.map(incol=df$regres, inmap=map, outfact=T)
  
  
  map<- cbind(1:5,c("Strongly Agree","Agree","Neither agree not disagree","Disagree","Strongly disagree"))
  df$smimp_t <- num.text.map(incol=df$smimp, inmap=map, outfact=T)
  df$smout_t <- num.text.map(incol=df$smout, inmap=map, outfact=T)
  
  map<- cbind(1:2,c("Yes","No"))
  df$chabod_t <- num.text.map(incol=df$chabod , inmap=map, outfact=T)
  df$charel_t<- num.text.map(incol=df$charel, inmap=map, outfact=T)
  df$chaready_t <- num.text.map(incol=df$chaready , inmap=map, outfact=T)
  df$chasti_t <- num.text.map(incol=df$chasti , inmap=map, outfact=T)
  df$chasxt_t <- num.text.map(incol=df$chasxt , inmap=map, outfact=T)
  
  map<- cbind(1:5,c("Strongly Agree","Agree","Unsure","Disagree","Strongly Disagree"))
  df$melike_t <- num.text.map(incol=df$melike, inmap=map, outfact=T)
  df$melook_t <- num.text.map(incol=df$melook, inmap=map, outfact=T)
  df$esteem_t<- num.text.map(incol=df$esteem, inmap=map, outfact=T)
  
  
  df$sexest_t<- factor(df$sexest,levels=1:5,
                       labels=c("None or a few","About a third","Half of them","Two thirds of them","Most or all of them"))
  
  map<- cbind(1:3,c("True","False","Don't know"))
  df$knwcdm_t  <- num.text.map(incol=df$knwcdm, inmap=map, outfact=T)
  df$knwsti_t<- num.text.map(incol=df$knwsti, inmap=map, outfact=T)
  df$knwlaw_t<- num.text.map(incol=df$knwlaw, inmap=map, outfact=T)
  df$knwdoc_t<- num.text.map(incol=df$knwdoc, inmap=map, outfact=T)
  df$knwcum_t <- num.text.map(incol=df$knwcum, inmap=map, outfact=T)
  
  map<- cbind(1:5,c("Strongly agree","Agree","Neither agree or disagree","Disagree","Strongly disagree"))
  df$opnude_t<- num.text.map(incol=df$opnude, inmap=map, outfact=T)
  df$oporn_t<- num.text.map(incol=df$oporn, inmap=map, outfact=T)
  df$opagree_t<- num.text.map(incol=df$opagree, inmap=map, outfact=T)
  df$opdrunk_t<- num.text.map(incol=df$opdrunk, inmap=map, outfact=T)
  df$oprisk_t<- num.text.map(incol=df$oprisk, inmap=map, outfact=T)
  
  map<- cbind(1:4,c("Didnt talk","Easy","Varies","Difficult"))
  df$talkpar_t <- num.text.map(incol=df$talkpar, inmap=map, outfact=T)
  df$talkfrend_t<- num.text.map(incol=df$talkfrend, inmap=map, outfact=T)
  
  map<- cbind(1:5,c("Very confident","Quite confident","Unsure","Not very confident","Not at all confident"))
  df$conget_t<- num.text.map(incol=df$conget, inmap=map, outfact=T)
  df$conput_t<- num.text.map(incol=df$conput, inmap=map, outfact=T)
  df$conref_t <- num.text.map(incol=df$conref, inmap=map, outfact=T)
  
  map<- cbind(1:3,c("Never","Yes, last 6 months","Yes, more than 6 months"))
  df$kiss_t <- num.text.map(incol=df$kiss, inmap=map, outfact=T)
  df$gent_t<- num.text.map(incol=df$gent, inmap=map, outfact=T)
  df$mstbte_t<- num.text.map(incol=df$mstbte, inmap=map, outfact=T)
  
  df$oralyes_t<- factor(df$oralyes,levels=1:2, labels=c("Yes","No"))
  
  df$oralwhy_t<- factor(df$oralwhy,levels=1:9,
                        labels=c("Dont want to/Not interested","Not had opportunity","Waiting for steady relationship","Waiting until older","Worried about risks of STIs/pregnancy","Worried about risk to reputation","Beliefs/values","Nervous/Scared","Other"))
  
  df$oralwhn_t<- factor(df$oralwhn,levels=1:2,labels=c("Last 6 months","More than 6 months"))
  df$oral1st_t<- factor(df$oral1st,levels=1:2,labels=c("Yes","No"))
  
  df$oralgen_t<- factor(df$oralgen,levels=1:3,labels=c("Male","Female","Trans/non-binary"))
  
  df$oralstcdm_t<- factor(df$oralstcdm,levels=1:2,labels=c("Yes","No"))
  
  df$oralcdmwhy_t<- factor(df$oralcdmwhy,levels=1:7,
                           labels=c("Got carried away and forgot","Did not have one to hand","I/my partner did not think it was necessary","I/my partner did not know how to use one","I/my partner did not want to use one","We had one but it broke","Other"))
  
  df$oralstpart_t<- factor(df$oralstpart,levels=1:3,labels=c("Male","Female","Non-binary"))
  
  df$intyes_t<- factor(df$intyes,levels=1:2,labels=c("Yes","No"))
  df$intwhy_t<- factor(df$intwhy,levels=1:9,
                       labels=c("Dont want to/ not interested","Not had opportunity","Waiting for steady relationship","Waiting until older","Worried about risks of STIs/pregnancy","Worried about risk to reputation","Beliefs/values","Nervous/Scared","Other"))
  
  df$intwhn_t<- factor(df$intwhn,levels=1:2,labels=c("Last 6 months","More than 6 months"))
  
  df$int1st_t<- factor(df$int1st,levels=1:2,labels=c("Yes","No"))
  
  df$int3mcfreq_t<- factor(df$int3mcfreq,levels=1:4,
                           labels=c("Always","Often","Sometimes","Never"))
  
  df$intlastcdm_t<- factor(df$intlastcdm,levels=1:2,labels=c("Yes","No"))
  
  df$intcdmwhy_t<- factor(df$intcdmwhy,levels=1:8,
                          labels=c("Got carried away and forgot","Did not have one to hand","I/my partner did not think it was necessary","I/my partner did not know how to use one","I/my partner did not want to use one","We had one but it broke","Both got tested for STIs & had another form of contraception","Other"))
  
  df$intlstwill_t<- factor(df$intlstwill,levels=1:3,labels=c("Equally willing","I was more willing","They were more willing"))
  
  # problems
  
  
  #df$intlast_regret_t <- factor(df$intlast_regret , levels=1:4, labels=c("Not at all","A bit","Quite a lot","Very much"))
  
  #df$intlast_drunk_t <- factor(df$intlast_drunk , levels=1:4, labels=c("No","Yes, a little bit","Yes, I was quite drunk","Very much"))
  
  df$intlastgend_t<- factor(df$intlastgend,levels=1:3,labels=c("Male","Female","Non-binary/trans"))
  
  #df$CHUwor_t<- factor(df$CHUwor,levels=1:5,labels=c("Dont feel worried","Little worried","Bit worried","Quite worried","Very worried"))
  #df$CHUsad_t<- factor(df$CHUsad,levels=1:5,labels=c("Dont feel sad","Little sad","Bit sad","Quite sad","Very sad"))
  #df$CHUpain_t<- factor(df$CHUpain,levels=1:5,labels=c("No pain","Little pain","Bit of pain","Quite a lot of pain","A lot of pain"))
  #df$CHUtire_t<- factor(df$CHUtire,levels=1:5,labels=c("Dont feel tired","Little tired","Bit tired","Quite tired","Very tired"))
  #df$CHUanoy_t<- factor(df$CHUanoy,levels=1:5,labels=c("Not annoyed","Little annoyed","Bit annoyed","Quite annoyed","Very annoyed"))
  #df$CHUwork_t<- factor(df$CHUwork,levels=1:5,labels=c("No problems","Few problems","Some problems","Many problems","Can't do"))
  #df$CHUslp_t<- factor(df$CHUslp,levels=1:5,labels=c("No problems","Few problems","Some problems","Many problems","Couldn't sleep"))
  #df$CHUrout_t<- factor(df$CHUrout,levels=1:5,labels=c("No problems","Few problems","Some problems","Many problems","Can't do routine"))
  #df$CHUact_t<- factor(df$CHUact,levels=1:5,labels=c("Any activity","Most activities","Some activities","Few activities","No activities"))
  
  #map<- cbind(1:2,c("No","Yes"))
  #df$cdmYN_t <- num.text.map(incol=df$cdmYN, inmap=map, outfact=T)
  #df$advYN_t <- num.text.map(incol=df$advYN, inmap=map, outfact=T)
  
  df$relyes_t<- factor(df$relyes,levels=1:3,labels=c("No, not in last 6mnths","Used to, not now","Yes, have one now"))
  
  df$rellng_t<- factor(df$rellng,levels=1:5,
                       labels=c("Less than a month","Between one & three months","Between three & six months","Between six months & a year","Over a year"))
  
  map<- cbind(1:5,c("All the time","Often","Sometimes","Not often","Never"))
  #df$rqfeel_t  <- num.text.map(incol=df$rqfeel, inmap=map, outfact=T)
  #df$rqhap_t<- num.text.map(incol=df$rqhap, inmap=map, outfact=T)
  #df$rqtalk_t<- num.text.map(incol=df$rqtalk, inmap=map, outfact=T)
  #df$rqres_t<- num.text.map(incol=df$rqres, inmap=map, outfact=T)
  #df$rqang_t<- num.text.map(incol=df$rqang, inmap=map, outfact=T)
  #df$rqpers_t<- num.text.map(incol=df$rqpers, inmap=map, outfact=T)
  #df$rqjeal_t<- num.text.map(incol=df$rqjeal, inmap=map, outfact=T)
  
  df$relgen_t<- factor(df$relgen,levels=1:3,labels=c("Male","Female","Trans/non-binary"))
  
  df$relS4_t<- factor(df$relS4,levels=1:2,labels=c("Yes","No"))
  
  df$prellng_t<- factor(df$prellng,levels=1:5,
                        labels=c("Less than a month","Between 1 & 3 months","Between 3 & 6 months ","Between 6 months & a year","Over one year"))
  
  map<- cbind(1:5,c("All the time","Often","Sometimes","Not often","Never"))
  #df$prqfeel_t  <- num.text.map(incol=df$prqfeel, inmap=map, outfact=T)
  #df$prqhap_t<- num.text.map(incol=df$prqhap, inmap=map, outfact=T)
  #df$pretalk_t<- num.text.map(incol=df$pretalk, inmap=map, outfact=T)
  #df$prqres_t<- num.text.map(incol=df$prqres, inmap=map, outfact=T)
  #df$prqang_t<- num.text.map(incol=df$prqang, inmap=map, outfact=T)
  #df$prqpers_t<- num.text.map(incol=df$prqpers, inmap=map, outfact=T)
  #df$prqjeal_t<- num.text.map(incol=df$prqjeal, inmap=map, outfact=T)
  
  df$prelgen_t<- factor(df$prelgen,levels=1:3,labels=c("Male","Female","Trans/non-binary"))
  #df$prelS4_t<- factor(df$prelS4,levels=1:2,labels=c("Yes","No"))
  #df$releasy_t<- factor(df$releasy,levels=1:4,labels=c("Not relevant","Always easy","Sometimes easy, sometimes difficult","Always difficult"))
  
  map<- cbind(1:4,c("Never","Once or twice","3-10times",">10 times"))
  #df$webinfo_t  <- num.text.map(incol=df$webinfo, inmap=map, outfact=T)
  #df$webimag_t<- num.text.map(incol=df$webimag, inmap=map, outfact=T)
  #df$webndsend_t<- num.text.map(incol=df$webndsend, inmap=map, outfact=T)
  #df$webndget_t<- num.text.map(incol=df$webndget, inmap=map, outfact=T)
  #df$webndask_t<- num.text.map(incol=df$webndask, inmap=map, outfact=T)
  #df$webndfor_t<- num.text.map(incol=df$webndfor, inmap=map, outfact=T)
  
  #df$distress_t<- factor(df$distress,levels=1:5,labels=c("Agree Strongly","Agree","Neither agree nor disagree","Disagree","Disagree strongly"))
  
  df$linkyes_t<- factor(df$linkyes,levels=1:2,labels=c("Yes","No"))
  
  return(df)
  
}


recoded.baseline <- recode.vars.baseline(raw.baseline)
recoded.control   <- recode.vars.control(raw.control)


#################################################
#################################################
### Add peer sex perception to control schools ##
#################################################
#################################################

###Code up the variable
raw.control$peersex <- ifelse(df$q_11_a==0,NA,df$q_11_a)
#Was no need to reorder. Just rename to suit the other scripts 

raw.control$sex_percep <- raw.control$peersex 
###Append on the new var. Too lazy to use a join
recoded.control <- cbind(recoded.control, raw.control$sex_percep)

table(recoded.control$sex_percep)

names(recoded.control)[462] <- "sex_percep"

save(recoded.baseline, file = "T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/recoded_baseline.rdata")
save(recoded.control, file = "T:/projects/stash_trial/09 STASH SNA/Data/AnonymisedData/working data/recoded_control.rdata")




