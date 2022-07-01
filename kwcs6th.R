# -------------------------------------------------------------------------
# Title: Tensor-based clustering of survey respondents, binary variables, 
# and Likert scale variables
# Authors: Juna Goo (junagoo@boisestate.edu), Seulbi Shin
# -------------------------------------------------------------------------

# To download KWCS 6th data:
# English version:
# https://www.kosha.or.kr/eoshri/resources/KWCSDownload.do
# Korean version:
# https://www.kosha.or.kr/oshri/researchField/downWorkingEnvironmentSurvey.do

library(haven)
kwcs6th <- read_dta("kwcs6th_eng_220210.dta") # kwcs6th_eng_220210.dta can be found at kwcs6th_eng_STATA.zip 

# subset the data with employees only
head(kwcs6th$emp_type)
table(kwcs6th$emp_type, exclude = T) 
kwcs6th <- kwcs6th[kwcs6th$emp_type == 3,]

# subset the data by age < 65
# In Korea, people are automatically one year old at birth.

kwcs6th <- kwcs6th[which(kwcs6th$AGE <= 65),] 
summary(kwcs6th$AGE)


# -------------------------------------------------------------------------
# Binary variables
# -------------------------------------------------------------------------
# regular (full-time employee 1 otherwise 0)
# A worker with no limitation on the period of employment or who has at least a one year term for the employment contract

head(kwcs6th$emp_stat)
kwcs6th$regular <- ifelse(kwcs6th$emp_stat == 1, 1, 
                          ifelse(kwcs6th$emp_stat %in% c(2,3), 0, NA))

table(kwcs6th$regular, exclude = T)

# age (>=40 1, otherwise 0)
kwcs6th$age <- ifelse(kwcs6th$AGE >= 40, 1,0)

# gender (male 1 female 0)
head(kwcs6th$TSEX)
table(kwcs6th$TSEX, exclude = T)
kwcs6th$gender <- ifelse(kwcs6th$TSEX ==2,0,1)

# education (Community college or above 1 otherwise 0) 
head(kwcs6th$edu)
table(kwcs6th$edu, exclude = T)
kwcs6th$education<- ifelse(kwcs6th$edu %in% c(5,6,7), 1, 
                           ifelse(kwcs6th$edu %in% c(1,2,3,4), 0,NA))

# work.life.balance (very well, well 1 otherwise 0)
head(kwcs6th$wbalance)
table(kwcs6th$wbalance, exclude = T)
kwcs6th$work.life.balance <- ifelse(kwcs6th$wbalance %in%c(1,2), 1,
                                    ifelse(kwcs6th$wbalance %in% c(3,4),0,NA))

# shift (yes 1 no 0)
head(kwcs6th$wtime_length5)
table(kwcs6th$wtime_length5, exclude = T)
kwcs6th$shift<-ifelse(kwcs6th$wtime_length5 == 1, 1,
                      ifelse(kwcs6th$wtime_length5 == 2,0, NA))

# discrimination 
head(kwcs6th$disc1)
table(kwcs6th$disc1, exclude = T)

kwcs6th$disc1.1<-ifelse(kwcs6th$disc1 == 1, 1,
                      ifelse(kwcs6th$disc1 == 2,0, NA))
table(kwcs6th$disc1.1, exclude = T)

kwcs6th$disc2.1<-ifelse(kwcs6th$disc2 == 1, 1,
                        ifelse(kwcs6th$disc2 == 2,0, NA))

kwcs6th$disc3.1<-ifelse(kwcs6th$disc3 == 1, 1,
                        ifelse(kwcs6th$disc3 == 2,0, NA))

kwcs6th$disc4.1<-ifelse(kwcs6th$disc4 == 1, 1,
                        ifelse(kwcs6th$disc4 == 2,0, NA))

kwcs6th$disc5.1<-ifelse(kwcs6th$disc5 == 1, 1,
                        ifelse(kwcs6th$disc5 == 2,0, NA))

kwcs6th$disc6.1<-ifelse(kwcs6th$disc6 == 1, 1,
                        ifelse(kwcs6th$disc6 == 2,0, NA))

kwcs6th$disc7.1<-ifelse(kwcs6th$disc7 == 1, 1,
                        ifelse(kwcs6th$disc7 == 2,0, NA))

kwcs6th$disc8.1<-ifelse(kwcs6th$disc8 == 1, 1,
                        ifelse(kwcs6th$disc8 == 2,0, NA))

kwcs6th$disc9.1<-ifelse(kwcs6th$disc9 == 1, 1,
                        ifelse(kwcs6th$disc9 == 2,0, NA))

kwcs6th$disc10.1<-ifelse(kwcs6th$disc10 == 1, 1,
                        ifelse(kwcs6th$disc10 == 2,0, NA))

kwcs6th$disc11.1<-ifelse(kwcs6th$disc11 == 1, 1,
                         ifelse(kwcs6th$disc11 == 2,0, NA))


# check observations with all NAs for the discrimination question
kwcs6th$is.na.disc <- apply(kwcs6th[,c("disc1.1","disc2.1","disc3.1",
                                       "disc4.1","disc5.1","disc6.1",
                                       "disc7.1","disc8.1","disc9.1",
                                       "disc10.1","disc11.1")], 1, 
                            function(x) sum(is.na(x)))

kwcs6th[which(kwcs6th$is.na.disc == 11),]

# check
kwcs6th[kwcs6th$id == "26087",c("disc1.1","disc2.1","disc3.1",
                                "disc4.1","disc5.1","disc6.1",
                                "disc7.1","disc8.1","disc9.1",
                                "disc10.1","disc11.1")]

kwcs6th[which(kwcs6th$is.na.disc == 10),]

# check
kwcs6th[kwcs6th$id == "50435",c("disc1.1","disc2.1","disc3.1",
                                "disc4.1","disc5.1","disc6.1",
                                "disc7.1","disc8.1","disc9.1",
                                "disc10.1","disc11.1")]

# remove observations with all NAs for the discrimination question
kwcs6th <- kwcs6th[which(kwcs6th$is.na.disc != 11),]

kwcs6th$discrimination <- apply(kwcs6th[,c("disc1.1","disc2.1","disc3.1",
                                           "disc4.1","disc5.1","disc6.1",
                                           "disc7.1","disc8.1","disc9.1",
                                           "disc10.1","disc11.1")], 1, function(x) sum(x, na.rm = T))

# at least one discrimination, 1 otherwise 0
kwcs6th$discrimination2 <- ifelse(kwcs6th$discrimination >= 1,1,0)

# violence 
head(kwcs6th$asb5)
table(kwcs6th$asb5, exclude = T)

kwcs6th$asb5.1<-ifelse(kwcs6th$asb5 == 1, 1,
                        ifelse(kwcs6th$asb5 == 2,0, NA))
table(kwcs6th$asb5.1, exclude = T)

kwcs6th$asb6.1<-ifelse(kwcs6th$asb6 == 1, 1,
                       ifelse(kwcs6th$asb6 == 2,0, NA))

kwcs6th$asb7.1<-ifelse(kwcs6th$asb7 == 1, 1,
                       ifelse(kwcs6th$asb7 == 2,0, NA))
# check observations with all NAs for the discrimination question
kwcs6th$is.na.asb <- apply(kwcs6th[,c("asb5.1","asb6.1","asb7.1")], 1, 
                            function(x) sum(is.na(x)))

# remove observations with all NAs for the violence question
kwcs6th <- kwcs6th[which(kwcs6th$is.na.asb != 3),]

kwcs6th$violence <- apply(kwcs6th[,c("asb5.1","asb6.1","asb7.1")], 1,
                                function(x) sum(x, na.rm = T))

# at least one violence, 1 otherwise 0
kwcs6th$violence2 <- ifelse(kwcs6th$violence >= 1,1,0)

# at least one of discrimination or violence
kwcs6th$discrimination.violence <- ifelse(kwcs6th$discrimination2 == 1 |
                                            kwcs6th$violence2 == 1,1,0)

# work.sick (yes 1 no/I was not sick 0)
head(kwcs6th$heal_wsick1)
table(kwcs6th$heal_wsick1, exclude = T)

kwcs6th$work.sick <- ifelse(kwcs6th$heal_wsick1==1,1,
                            ifelse(kwcs6th$heal_wsick1 %in% c(2,7),0,NA))

# observations that worked less than 36 hours per week: part time
# observations that worked >= 36 hours per week: full time

head(kwcs6th$emp_fptime)
table(kwcs6th$emp_fptime, exclude = T)

kwcs6th <- kwcs6th[kwcs6th$emp_fptime %in% c(1,2), ]

kwcs6th$wtime_week2 <- ifelse(is.na(kwcs6th$wtime_week) == F, kwcs6th$wtime_week,
                              kwcs6th$wtime_month/4)

table(kwcs6th[kwcs6th$emp_fptime == 1, ]$wtime_week2, exclude = T)
table(kwcs6th[kwcs6th$emp_fptime == 2, ]$wtime_week2, exclude = T)

kwcs6th[which(kwcs6th$wtime_week2 == 144),] # outlier: 144 hours per week 
kwcs6th[which(kwcs6th$wtime_week2 == 144),]$wtime_week2 <- NA

table(kwcs6th$wtime_week2, exclude = T)
kwcs6th <- kwcs6th[is.na(kwcs6th$wtime_week2) == F, ]

# full.time (full-time & >= 36 hours per week 1 part-time & < 36 hours per week 0) 
head(kwcs6th$emp_fptime)

kwcs6th$full.time <- ifelse(kwcs6th$emp_fptime == 1 & kwcs6th$wtime_week2 >= 36, 1,
                            ifelse(kwcs6th$emp_fptime == 2 & kwcs6th$wtime_week2 < 36, 0, NA))

table(kwcs6th$full.time, exclude = T)

# income (>= 3 million won 1 otherwise 0)
head(kwcs6th$TEARNING)
table(kwcs6th$TEARNING, exclude = T)
kwcs6th$income <- ifelse(kwcs6th$TEARNING == 5,NA,
                         ifelse(kwcs6th$TEARNING %in% c(1,2),0,1))

# work.period (>= 1 year 1 otherwise 0)
head(kwcs6th$TWDURATION)
table(kwcs6th$TWDURATION, exclude = T)

kwcs6th$work.period <- ifelse(kwcs6th$TWDURATION == 7, NA,
                            ifelse(kwcs6th$TWDURATION == 1,0,1))


# expense.income (very easily, easily, fairly easily 1 otherwise 0)
head(kwcs6th$income_bal)
table(kwcs6th$income_bal, exclude = T)

kwcs6th$expense.income <- ifelse(kwcs6th$income_bal %in% c(8,9),NA,
                                 ifelse(kwcs6th$income_bal %in% c(1,2,3),1,0))

# skill (My present skills correspond well with my duties or
# I have the skills to cope with more demanding duties 1
# I need further training to cope well with my duties 0)

head(kwcs6th$skillmat)
table(kwcs6th$skillmat, exclude = T)

kwcs6th$skill <- ifelse(kwcs6th$skillmat %in% c(8,9),NA,
                      ifelse(kwcs6th$skillmat %in% c(2,3),1,0))

# work.at.risk (yes 1 no 0)
head(kwcs6th$heal_risk)
table(kwcs6th$heal_risk, exclude = T)

kwcs6th$work.at.risk <- ifelse(kwcs6th$heal_risk %in% c(8,9),NA,
                             ifelse(kwcs6th$heal_risk == 1,1,0))


# -------------------------------------------------------------------------
# Likert scale variables
# -------------------------------------------------------------------------
head(kwcs6th$wstat1)
table(kwcs6th$wstat1, exclude = T)

kwcs6th$well.paid <- ifelse(kwcs6th$wstat1 %in% c(7,8,9), NA,
                            kwcs6th$wstat1)

head(kwcs6th$wstat2)
table(kwcs6th$wstat2, exclude = T)
kwcs6th$career.prospects <- ifelse(kwcs6th$wstat2 %in% c(7,8,9), NA,
                                   kwcs6th$wstat2)

head(kwcs6th$wstat3)
table(kwcs6th$wstat3, exclude = T)

kwcs6th$recognition <- ifelse(kwcs6th$wstat3 %in% c(7,8,9), NA, 
                              kwcs6th$wstat3)

head(kwcs6th$wstat4)
table(kwcs6th$wstat4, exclude = T)

kwcs6th$competition <- ifelse(kwcs6th$wstat4 %in% c(7,8,9), NA, 
                              kwcs6th$wstat4)

head(kwcs6th$wstat5)
table(kwcs6th$wstat5, exclude = T)

kwcs6th$motivation <- ifelse(kwcs6th$wstat5 %in% c(7,8,9), NA, 
                             kwcs6th$wstat5)

head(kwcs6th$wstat6)
table(kwcs6th$wstat6, exclude = T)

kwcs6th$lose.job <- ifelse(kwcs6th$wstat6 %in% c(7,8,9), NA, 
                           kwcs6th$wstat6)

head(kwcs6th$wstat7)
table(kwcs6th$wstat7, exclude = T)

kwcs6th$new.job <- ifelse(kwcs6th$wstat7 %in% c(7,8,9), NA, 
                          kwcs6th$wstat7)

head(kwcs6th$comp_ass1)
table(kwcs6th$comp_ass1, exclude = T)

kwcs6th$appreciation <- ifelse(kwcs6th$comp_ass1 %in% c(7,8,9), NA,
                               kwcs6th$comp_ass1)

head(kwcs6th$comp_ass2)
table(kwcs6th$comp_ass2, exclude = T)

kwcs6th$management.trust <- ifelse(kwcs6th$comp_ass2 %in% c(7,8,9), NA,
                                   kwcs6th$comp_ass2)

head(kwcs6th$comp_ass3)
table(kwcs6th$comp_ass3, exclude = T)

kwcs6th$conflict.fair <- ifelse(kwcs6th$comp_ass3 %in% c(7,8,9), NA,
                                kwcs6th$comp_ass3)
head(kwcs6th$comp_ass4)
table(kwcs6th$comp_ass4, exclude = T)

kwcs6th$work.fair <- ifelse(kwcs6th$comp_ass4 %in% c(7,8,9), NA,
                            kwcs6th$comp_ass4)

head(kwcs6th$comp_ass5)
table(kwcs6th$comp_ass5, exclude = T)

kwcs6th$cooperation <- ifelse(kwcs6th$comp_ass5 %in% c(7,8,9), NA,
                              kwcs6th$comp_ass5)

head(kwcs6th$comp_ass6)
table(kwcs6th$comp_ass6, exclude = T)

kwcs6th$employee.trust <- ifelse(kwcs6th$comp_ass6 %in% c(7,8,9), NA,
                                 kwcs6th$comp_ass6)

# whole.satisfaction (very satisfied, satisfied 1 not very satisfied not at all satisfied 0)

head(kwcs6th$satisfaction)
table(kwcs6th$satisfaction, exclude = T)
     
kwcs6th$whole.satisfaction <- ifelse(kwcs6th$satisfaction %in% c(8,9), NA,
                                     ifelse(kwcs6th$satisfaction %in% c(1,2),1,0))


colnames(kwcs6th)

kwcs6th$ID <- paste0("id:",kwcs6th$id)

# Binary variables

binary <- c("gender","age","education","skill",
            "work.period","regular","full.time","shift",
            "income","expense.income","work.life.balance",
            "work.sick","work.at.risk","discrimination.violence")

# Likert scale variables

likert <- c("well.paid","career.prospects","recognition",
            "competition","motivation","lose.job","new.job",
            "appreciation","management.trust","conflict.fair",
            "work.fair","cooperation","employee.trust")

subkwcs <- kwcs6th[,c("ID","whole.satisfaction",binary,likert)]
colSums(is.na(subkwcs))

subkwcs <- subkwcs[complete.cases(subkwcs),]

subkwcs <- as.data.frame(subkwcs)

for (i in 1:length(binary)){
  subkwcs[,binary[i]] <- factor(subkwcs[,binary[i]])
}

for (i in 1:length(likert)){
  subkwcs[,likert[i]] <- factor(subkwcs[,likert[i]])
  levels(subkwcs[,likert[i]]) <- c("SA","A","N","D","SD")
}

subkwcs$whole.satisfaction <- factor(subkwcs$whole.satisfaction)

str(subkwcs)

# save the final data set for the analysis
save(subkwcs, file = "Final_kwcs6th.RData")


# remove all except subkwcs data set, binary and likert variables 
rm(list=setdiff(ls(), c("subkwcs","binary","likert")))

# -------------------------------------------------------------------------
# Table 4
# -------------------------------------------------------------------------
mat <- matrix(NA, nrow = length(binary), ncol = 3)
colnames(mat) <- c("Satisfied", "Not-Satisfied","pvlaue")
rownames(mat) <- binary

for (i in 1:length(binary)){
  x <- table(subkwcs[,binary[i]], subkwcs$whole.satisfaction)
  y <- as.numeric(table(subkwcs$whole.satisfaction))
  mat[i,1] <- round(x[2,2]/y[2], digits = 3)*100
  mat[i,2] <- round(x[2,1]/y[1], digits = 3)*100
  mat[i,3] <- round(chisq.test(subkwcs[,binary[i]],subkwcs$whole.satisfaction)$p.value, digits = 4)
}

mat
table(subkwcs$whole.satisfaction)

# -------------------------------------------------------------------------
# Likert scale variables p-value (not included in the manuscript)
# -------------------------------------------------------------------------
mat <- matrix(NA, nrow = length(likert), ncol = 11)
colnames(mat) <- c("Satisfied-SA", "Satisfied-A", "Satisfied-N", 
                   "Satisfied-D", "Satisfied-SD", 
                   "Not-Satisfied-SA", "Not-Satisfied-A", "Not-Satisfied-N", 
                   "Not-Satisfied-D", "Not-Satisfied-SD", "pvlaue")
rownames(mat) <- likert

for (i in 1:length(likert)){
  x <- table(subkwcs[,likert[i]], subkwcs$whole.satisfaction)
  y <- as.numeric(table(subkwcs$whole.satisfaction))
  mat[i,1:5] <- round(x[,2]/y[2], digits = 3)*100
  mat[i,6:10] <- round(x[,1]/y[1], digits = 3)*100
  mat[i,11] <- round(chisq.test(subkwcs[,likert[i]],subkwcs$whole.satisfaction)$p.value, digits = 4)
}



mat

# -------------------------------------------------------------------------
# Figure 1 
# -------------------------------------------------------------------------

library(ggplot2)
library(ggpubr)

data <- subkwcs[,c("whole.satisfaction",likert)]

data$whole.satisfaction <- ifelse(data$whole.satisfaction == 0,"Not Satisfied","Satisfied")
str(data)

theme_set(theme_classic())

ps1=ggplot(data, aes(fill=well.paid, x=whole.satisfaction)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle(label="Considering all my efforts \nand achievements in my job, \nI feel I get paid appropriately.")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 10))+
  scale_x_discrete(limits=c("Satisfied","Not Satisfied"))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

ps2=ggplot(data, aes(fill=career.prospects, x=whole.satisfaction)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("My job offers good prospects \nfor career advancement.\n")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 10))+
  scale_x_discrete(limits=c("Satisfied","Not Satisfied"))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

ps3=ggplot(data, aes(fill=recognition, x=whole.satisfaction)) + 
  geom_bar(position="fill",colour="black")+  
  ggtitle("I receive the recognition \nI deserve for my work.\n")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 10))+
  scale_x_discrete(limits=c("Satisfied","Not Satisfied"))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

ps4=ggplot(data, aes(fill=competition, x=whole.satisfaction)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("I feel I strongly compete with \nothers in and for work.\n")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 10))+
  scale_x_discrete(limits=c("Satisfied","Not Satisfied"))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

ps5=ggplot(data, aes(fill=motivation, x=whole.satisfaction)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("The organisation I work for \nmotivates me to give my best \njob performance.")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 10))+
  scale_x_discrete(limits=c("Satisfied","Not Satisfied"))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

ps6=ggplot(data, aes(fill=new.job, x=whole.satisfaction)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("I might lose my job \nin the next 6 months.\n")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 10))+
  scale_x_discrete(limits=c("Satisfied","Not Satisfied"))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

ps7=ggplot(data, aes(fill=new.job, x=whole.satisfaction)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("If I were to lose or quit my current\njob, it would be easy for me\nto find a job of similar salary.")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 10))+
  scale_x_discrete(limits=c("Satisfied","Not Satisfied"))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

pw1=ggplot(data, aes(fill=appreciation, x=whole.satisfaction)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("Employees are appreciated \nwhen they have done a good \njob.")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 10))+
  scale_x_discrete(limits=c("Satisfied","Not Satisfied"))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

pw2=ggplot(data, aes(fill=management.trust, x=whole.satisfaction)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("The management trusts \nthe employees to do their \nwork well.")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 10))+
  scale_x_discrete(limits=c("Satisfied","Not Satisfied"))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

pw3=ggplot(data, aes(fill=conflict.fair, x=whole.satisfaction)) + 
  geom_bar(position="fill",colour="black")+  
  ggtitle("Conflicts are resolved in a fair\nway.\n")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 10))+
  scale_x_discrete(limits=c("Satisfied","Not Satisfied"))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

pw4=ggplot(data, aes(fill=work.fair, x=whole.satisfaction)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("The work is distributed fairly.\n\n")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 10))+
  scale_x_discrete(limits=c("Satisfied","Not Satisfied"))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

pw5=ggplot(data, aes(fill=cooperation, x=whole.satisfaction)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("There is good cooperation \nbetween you and your colleagues.\n")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 10))+
  scale_x_discrete(limits=c("Satisfied","Not Satisfied"))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

pw6=ggplot(data, aes(fill=employee.trust, x=whole.satisfaction)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("In general, employees trust \nmanagement.\n")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 10))+
  scale_x_discrete(limits=c("Satisfied","Not Satisfied"))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

ggarrange(ps1,ps2,ps3,ps4,ps5,ps6,ps7,pw1,pw2,pw3,pw4,pw5,pw6, nrow=5, ncol=3, common.legend=TRUE, legend="bottom", labels="AUTO")


# -------------------------------------------------------------------------
# Tensor representation
# -------------------------------------------------------------------------
library(rTensor)

nosatis <- subkwcs[subkwcs$whole.satisfaction == 0,]
satis <- subkwcs[subkwcs$whole.satisfaction == 1,]

nosatis$whole.satisfaction <- NULL
satis$whole.satisfaction <- NULL

# -------------------------------------------------------------------------
# Whole satisfaction == "1" (Satisfied group)
# -------------------------------------------------------------------------

n <- nrow(satis)
satis.m.array <- array(NA, dim = c(n,length(binary), length(likert)))

dat <- satis

for (j in 1:length(binary)){
  for (k in 1:length(likert)){
    tt <- table(dat[,binary[j]],dat[,likert[k]])
    
    for (i in 1:n){
      a <- dat[i,binary[j]]
      b <- dat[i,likert[k]]
      
      if (b == "SA"){
        satis.m.array[i,j,k] <- tt[a,"SA"]/n + tt[a,"A"]/n
      }else if (b == "SD"){
        satis.m.array[i,j,k] <- tt[a,"SD"]/n + tt[a,"D"]/n
      }else{
        satis.m.array[i,j,k] <- tt[a,b]/n
      }
    }
  }
}

satis.m.tnsr <- as.tensor(satis.m.array)
set.seed(2022)
satis.tnsr <- cp(satis.m.tnsr, num_components = 2, max_iter = 1000) # cp decomposition

# -------------------------------------------------------------------------
# Whole satisfaction == "0" (Not Satisfied group)
# -------------------------------------------------------------------------

n <- nrow(nosatis)
nosatis.m.array <- array(NA, dim = c(n,length(binary), length(likert)))

dat <- nosatis

for (j in 1:length(binary)){
  for (k in 1:length(likert)){
    tt <- table(dat[,binary[j]],dat[,likert[k]])
    
    for (i in 1:n){
      a <- dat[i,binary[j]]
      b <- dat[i,likert[k]]
      
      if (b == "SA"){
        nosatis.m.array[i,j,k] <- tt[a,"SA"]/n + tt[a,"A"]/n
      }else if (b == "SD"){
        nosatis.m.array[i,j,k] <- tt[a,"SD"]/n + tt[a,"D"]/n
      }else{
        nosatis.m.array[i,j,k] <- tt[a,b]/n
      }
    }
  }
}

nosatis.m.tnsr <- as.tensor(nosatis.m.array) 
set.seed(2022)
nosatis.tnsr <- cp(nosatis.m.tnsr, num_components = 2, max_iter = 1000) # cp decomposition

# -------------------------------------------------------------------------
# Whole satisfaction == "1" (Satisfied group)
# mode 1: survey respondents

satis.u1 <- as.data.frame(satis.tnsr$U[[1]])
rownames(satis.u1) <- satis$ID

set.seed(2022)
km <- kmeans(satis.u1,3)
km
c1 <- km$cluster

s1 <- satis[satis$ID %in% names(c1[c1 == 1]),]
s2 <- satis[satis$ID %in% names(c1[c1 == 2]),]
s3 <- satis[satis$ID %in% names(c1[c1 == 3]),]

s.binary <- matrix(NA, nrow = length(binary), ncol = 7)
colnames(s.binary) <- c("binary", "s1_0", "s1_1", "s2_0", "s2_1", "s3_0", "s3_1")
s.binary[,1] <- binary

for (i in 1:length(binary)){
  s.binary[i,2:3] <- round(table(s1[,binary[i]])/nrow(s1), digits = 3)
  s.binary[i,4:5] <- round(table(s2[,binary[i]])/nrow(s2), digits = 3)
  s.binary[i,6:7] <- round(table(s3[,binary[i]])/nrow(s3), digits = 3)
}

s.binary <- as.data.frame(s.binary)

for (i in 2:7){
  s.binary[,i] <- as.numeric(s.binary[,i])
}

s.binary$binary <- as.factor(s.binary$binary)
s.binary$binary <- factor(s.binary$binary, levels=rev(binary))

str(s.binary)

library(reshape2)

z1 <- melt(s.binary, id = "binary")
z1$code <- substring(z1$variable, 4)
z1$code <- as.factor(z1$code)
z1$variable <- substr(z1$variable, 1, 2)
z1$variable <- as.factor(z1$variable)

str(z1)

g1 <-  ggplot(z1, aes(x = value, y = binary)) + 
  geom_point(aes(fill = variable, shape = variable), size = 1.5) +
  facet_wrap(~ code, ncol = 5) + 
  scale_shape_manual(values = c(21,22,24)) +
  scale_fill_manual(values = c("#ffc425","#00aedb","#d11141")) +
  xlab("") + ylab("") + 
  scale_x_continuous(breaks=c(0, 0.5, 1), limits=c(0, 1)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(legend.title = element_blank(),
        plot.margin = unit(c(0,0.5,-0.5,0), "cm")) 
g1

s.likert <- matrix(NA, nrow = length(likert), ncol = 16)
colnames(s.likert) <- c("likert", paste0("s1_",c("SA","A","N","D","SD")),
                        paste0("s2_",c("SA","A","N","D","SD")),
                        paste0("s3_",c("SA","A","N","D","SD")))

s.likert[,1] <- likert

for (i in 1:length(likert)){
  s.likert[i,2:6] <- round(table(s1[,likert[i]])/nrow(s1), digits = 3)
  s.likert[i,7:11] <- round(table(s2[,likert[i]])/nrow(s2), digits = 3)
  s.likert[i,12:16] <- round(table(s3[,likert[i]])/nrow(s3), digits = 3)
}


s.likert <- as.data.frame(s.likert)
s.likert

for (i in 2:16){
  s.likert[,i] <- as.numeric(s.likert[,i])
}

s.likert$likert <- as.factor(s.likert$likert)
s.likert$likert <- factor(s.likert$likert, levels=rev(likert))

z2 <- melt(s.likert, id = "likert")
z2$code <- substring(z2$variable, 4)
z2$code <- factor(z2$code, levels = c("SA","A","N","D","SD"))
z2$variable <- substr(z2$variable, 1, 2)
z2$variable <- as.factor(z2$variable)

str(z2)

g2 <- ggplot(z2, aes(x = value, y = likert)) + 
  geom_point(aes(fill = variable, shape = variable), size = 1.5) +
  facet_wrap(~ code, ncol = 5) + 
  scale_shape_manual(values = c(21,22,24)) +
  scale_fill_manual(values = c("#ffc425","#00aedb","#d11141")) +
  xlab("") + ylab("") + 
  scale_x_continuous(breaks=c(0, 0.5, 1), limits=c(0, 1)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(legend.title = element_blank(),
        plot.margin = unit(c(0,0.5,-0.5,0), "cm")) 

g2


# -------------------------------------------------------------------------
# Figure 2 A,B 
# -------------------------------------------------------------------------

ggarrange(g1, g2, labels = c("A","B"), common.legend = T,
          legend = "bottom",   ncol = 2, widths = c(1,1.5)) 

# -------------------------------------------------------------------------
# Whole satisfaction == "0" (Not Satisfied group)
# mode 1: survey respondents

nosatis.u1 <- as.data.frame(nosatis.tnsr$U[[1]])
rownames(nosatis.u1) <- nosatis$ID

set.seed(2022)
km <- kmeans(nosatis.u1,3)
km
c1 <- km$cluster

ns1 <- nosatis[nosatis$ID %in% names(c1[c1 == 1]),]
ns2 <- nosatis[nosatis$ID %in% names(c1[c1 == 2]),]
ns3 <- nosatis[nosatis$ID %in% names(c1[c1 == 3]),]

ns.binary <- matrix(NA, nrow = length(binary), ncol = 7)
colnames(ns.binary) <- c("binary", "ns1_0", "ns1_1", "ns2_0", "ns2_1", "ns3_0", "ns3_1")
ns.binary[,1] <- binary

for (i in 1:length(binary)){
  ns.binary[i,2:3] <- round(table(ns1[,binary[i]])/nrow(ns1), digits = 3)
  ns.binary[i,4:5] <- round(table(ns2[,binary[i]])/nrow(ns2), digits = 3)
  ns.binary[i,6:7] <- round(table(ns3[,binary[i]])/nrow(ns3), digits = 3)
}

ns.binary <- as.data.frame(ns.binary)

for (i in 2:7){
  ns.binary[,i] <- as.numeric(ns.binary[,i])
}

ns.binary$binary <- as.factor(ns.binary$binary)
ns.binary$binary <- factor(ns.binary$binary, levels=rev(binary))
str(ns.binary)

z3 <- melt(ns.binary, id = "binary")
z3$code <- substring(z3$variable, 5)
z3$code <- as.factor(z3$code)
z3$variable <- substr(z3$variable, 1, 3)
z3$variable <- as.factor(z3$variable)

str(z3)


g1 <- ggplot(z3, aes(x = value, y = binary)) + 
  geom_point(aes(fill = variable, shape = variable), size = 1.5) +
  facet_wrap(~ code, ncol = 5) + 
  scale_shape_manual(values = c(21,22,24)) +
  scale_fill_manual(values = c("#000000","#00b159","#f37735")) +
  xlab("") + ylab("") + 
  scale_x_continuous(breaks=c(0, 0.5, 1), limits=c(0, 1)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(legend.title = element_blank(),
        plot.margin = unit(c(0,0.5,-0.5,0), "cm")) 
g1

ns.likert <- matrix(NA, nrow = length(likert), ncol = 16)
colnames(ns.likert) <- c("likert", paste0("ns1_",c("SA","A","N","D","SD")),
                        paste0("ns2_",c("SA","A","N","D","SD")),
                        paste0("ns3_",c("SA","A","N","D","SD")))

ns.likert[,1] <- likert

for (i in 1:length(likert)){
  ns.likert[i,2:6] <- round(table(ns1[,likert[i]])/nrow(ns1), digits = 3)
  ns.likert[i,7:11] <- round(table(ns2[,likert[i]])/nrow(ns2), digits = 3)
  ns.likert[i,12:16] <- round(table(ns3[,likert[i]])/nrow(ns3), digits = 3)
}


ns.likert <- as.data.frame(ns.likert)
ns.likert

for (i in 2:16){
  ns.likert[,i] <- as.numeric(ns.likert[,i])
}

ns.likert$likert <- as.factor(ns.likert$likert)
ns.likert$likert <- factor(ns.likert$likert, levels=rev(likert))

str(ns.likert)

z4 <- melt(ns.likert, id = "likert")
z4$code <- substring(z4$variable, 5)
z4$code <- factor(z4$code, levels = c("SA","A","N","D","SD"))
z4$variable <- substr(z4$variable, 1, 3)
z4$variable <- as.factor(z4$variable)

str(z4)

g2 <- ggplot(z4, aes(x = value, y = likert)) + 
  geom_point(aes(fill = variable, shape = variable), size = 1.5) +
  facet_wrap(~ code, ncol = 5) + 
  scale_shape_manual(values = c(21,22,24)) +
  scale_fill_manual(values = c("#000000","#00b159","#f37735")) +
  xlab("") + ylab("") + 
  scale_x_continuous(breaks=c(0, 0.5, 1), limits=c(0, 1)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(legend.title = element_blank(),
        plot.margin = unit(c(0,0.5,-0.5,0), "cm")) 

g2

# -------------------------------------------------------------------------
# Figure 2 C,D 
# -------------------------------------------------------------------------

ggarrange(g1, g2, labels = c("C","D"), common.legend = T,
          legend = "bottom",   ncol = 2, widths = c(1,1.5)) 


# -------------------------------------------------------------------------
# Whole satisfaction == "1" (Satisfied group)
# mode 2: binary variables

satis.u2 <- as.data.frame(satis.tnsr$U[[2]])
rownames(satis.u2) <- binary

set.seed(2022)
km <- kmeans(satis.u2,3)
km

dt <- satis.u2
dt$km.cluster <- as.factor(km$cluster)
dt[dt$km.cluster==1,]
dt[dt$km.cluster==2,]
dt[dt$km.cluster==3,]


library(ggrepel)

g1 <- ggplot(data = dt,aes(x = V1, y = V2, color = km.cluster, 
                           shape = km.cluster,
                           label = rownames(dt))) +
  geom_point() +
  geom_text_repel(max.overlaps = 100, size = 3) +
  scale_color_brewer(palette = "Set1") +
  ggtitle("SSB/SST=94.4%") +
  xlab("b1") + ylab("b2") +
  theme_bw() +
  theme(legend.position = "none")

g1


# mode 3: Likert scale variables

satis.u3 <- as.data.frame(satis.tnsr$U[[3]])
rownames(satis.u3) <- likert

set.seed(2022)
km <- kmeans(satis.u3,3)
km
dt2 <- satis.u3
dt2$km.cluster <- as.factor(km$cluster)

dt2[dt2$km.cluster==1,]
dt2[dt2$km.cluster==2,]
dt2[dt2$km.cluster==3,]


#set.seed(2022)
#km0 <- kmeans(satis.u3,2)
#km0 # 56.8 %


g2 <- ggplot(data = dt2, aes(x = V1, y = V2, color = km.cluster, 
                             shape = km.cluster,
                             label = rownames(dt2))) +
  geom_point() +
  geom_text_repel(max.overlaps = 100, size = 3) +
  scale_color_brewer(palette = "Set1") +
  ggtitle("SSB/SST=90%") +
  xlab("c1") + ylab("c2") +
  theme_bw() +
  theme(legend.position = "none")

g2


# -------------------------------------------------------------------------
# Figure 3 A,B
# -------------------------------------------------------------------------
ggarrange(g1, g2, labels = c("A","B"), ncol = 2) 


# -------------------------------------------------------------------------
# Whole satisfaction == "0" (Not Satisfied group)
# mode 2: binary variables

nosatis.u2 <- as.data.frame(nosatis.tnsr$U[[2]])
rownames(nosatis.u2) <- binary

set.seed(2022)
km <- kmeans(nosatis.u2,3)
km
dt <- nosatis.u2
dt$km.cluster <- as.factor(km$cluster)

dt[dt$km.cluster==1,]
dt[dt$km.cluster==2,]
dt[dt$km.cluster==3,]


g1 <- ggplot(data = dt,aes(x = V1, y = V2, color = km.cluster, 
                           shape = km.cluster,
                           label = rownames(dt))) +
  geom_point() +
  geom_text_repel(max.overlaps = 100, size = 3) +
  scale_color_brewer(palette = "Set1") +
  ggtitle("SSB/SST=90.5%") +
  xlab("b1") + ylab("b2") +
  theme_bw() +
  theme(legend.position = "none")

g1

# mode 3: Likert scale variables

nosatis.u3 <- as.data.frame(nosatis.tnsr$U[[3]])
rownames(nosatis.u3) <- likert

set.seed(2022)
km <- kmeans(nosatis.u3,3)
km
dt2 <- nosatis.u3
dt2$km.cluster <- as.factor(km$cluster)

dt2[dt2$km.cluster==1,]
dt2[dt2$km.cluster==2,]
dt2[dt2$km.cluster==3,]

#set.seed(2022)
#km0 <- kmeans(nosatis.u3,2)
#km0 # 76.7 %

g2 <- ggplot(data = dt2, aes(x = V1, y = V2, color = km.cluster, 
                             shape = km.cluster,
                             label = rownames(dt2))) +
  geom_point() +
  geom_text_repel(max.overlaps = 100, size = 3) +
  scale_color_brewer(palette = "Set1") +
  ggtitle("SSB/SST=85.2%") +
  xlab("c1") + ylab("c2") +
  theme_bw() +
  theme(legend.position = "none")

g2


# -------------------------------------------------------------------------
# Figure 3 C,D
# -------------------------------------------------------------------------
ggarrange(g1, g2, labels = c("C","D"), ncol = 2) 
