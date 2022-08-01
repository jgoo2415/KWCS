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

kwcs6th<-read_dta("kwcs6th_eng_220210.dta") 
# kwcs6th_eng_220210.dta can be found at kwcs6th_eng_STATA.zip 

# subset the data with employees only
head(kwcs6th$emp_type)
table(kwcs6th$emp_type, exclude = T) 
kwcs6th<-kwcs6th[kwcs6th$emp_type==3,]

# subset the data by age < 65

kwcs6th<-kwcs6th[kwcs6th$AGE<65,]
summary(kwcs6th$AGE)

# -------------------------------------------------------------------------
# Binary variables
# -------------------------------------------------------------------------
# gender (male 1 female 0)
head(kwcs6th$TSEX)
table(kwcs6th$TSEX, exclude = T)
kwcs6th$gender<-ifelse(kwcs6th$TSEX==2,0,1)

# education (Community college or above 1 Otherwise 0) 
head(kwcs6th$edu)
table(kwcs6th$edu, exclude = T)
kwcs6th$education<-ifelse(kwcs6th$edu %in% c(5,6,7),1, 
                          ifelse(kwcs6th$edu %in% c(1,2,3,4),0,NA))

table(kwcs6th$education, exclude = T)

# regular (yes 1 no 0)
# A worker with no limitation on the period of employment or who has at least a one year term for the employment contract

head(kwcs6th$emp_stat)
kwcs6th$regular<-ifelse(kwcs6th$emp_stat == 1, 1, 
                        ifelse(kwcs6th$emp_stat %in% c(2,3), 0, NA))

table(kwcs6th$regular, exclude = T)

# observations that worked less than 36 hours per week: part time
# observations that worked >= 36 hours per week: full time

head(kwcs6th$emp_fptime)
table(kwcs6th$emp_fptime, exclude = T)
kwcs6th$emp_fptime2<-ifelse(kwcs6th$emp_fptime %in% c(8,9),NA,
                            ifelse(kwcs6th$emp_fptime==2,0,kwcs6th$emp_fptime))
table(kwcs6th$emp_fptime2, exclude = T)
kwcs6th$wtime_week2<-ifelse(is.na(kwcs6th$wtime_week)==F, kwcs6th$wtime_week,
                            kwcs6th$wtime_month/4)
table(kwcs6th[kwcs6th$emp_fptime2==1, ]$wtime_week2, exclude = T)
table(kwcs6th[kwcs6th$emp_fptime2==0, ]$wtime_week2, exclude = T)

kwcs6th[which(kwcs6th$wtime_week2==144),] # outlier: 144 hours per week 
kwcs6th[which(kwcs6th$wtime_week2==144),]$wtime_week2<-NA

table(kwcs6th$wtime_week2, exclude = T)
kwcs6th<-kwcs6th[is.na(kwcs6th$wtime_week2)==F, ]

# full.time (yes 1 no 0)
# (full-time & >= 36 hours per week 1 part-time & < 36 hours per week 0) 
kwcs6th$full.time<-ifelse(kwcs6th$emp_fptime2==1 & kwcs6th$wtime_week2>=36, 1,
                          ifelse(kwcs6th$emp_fptime2==0 & kwcs6th$wtime_week2<36, 0, NA))

table(kwcs6th$full.time, exclude = T)

# shift (yes 1 no 0)
head(kwcs6th$wtime_length5)
table(kwcs6th$wtime_length5, exclude = T)
kwcs6th$shift<-ifelse(kwcs6th$wtime_length5==2,0,
                      ifelse(kwcs6th$wtime_length5 %in% c(8,9),NA,
                             kwcs6th$wtime_length5))
table(kwcs6th$shift, exclude = T)

# depend.colleagues (yes 1 no 0)
head(kwcs6th$winten3_1)
table(kwcs6th$winten3_1, exclude = T)
kwcs6th$depend.colleagues<-ifelse(kwcs6th$winten3_1==2,0,
                                  ifelse(kwcs6th$winten3_1 %in% c(7,8,9),NA,
                                         kwcs6th$winten3_1))
table(kwcs6th$depend.colleagues, exclude = T)

# depend.people (yes 1 no 0)
head(kwcs6th$winten3_2)
table(kwcs6th$winten3_2, exclude = T)
kwcs6th$depend.people<-ifelse(kwcs6th$winten3_2==2,0,
                                  ifelse(kwcs6th$winten3_2 %in% c(7,8,9),NA,
                                         kwcs6th$winten3_2))
table(kwcs6th$depend.people, exclude = T)

# depend.target (yes 1 no 0)
head(kwcs6th$winten3_3)
table(kwcs6th$winten3_3, exclude = T)
kwcs6th$depend.target<-ifelse(kwcs6th$winten3_3==2,0,
                              ifelse(kwcs6th$winten3_3 %in% c(7,8,9),NA,
                                     kwcs6th$winten3_3))
table(kwcs6th$depend.target, exclude = T)

# depend.automatic (yes 1 no 0)
head(kwcs6th$winten3_4)
table(kwcs6th$winten3_4, exclude = T)
kwcs6th$depend.automatic<-ifelse(kwcs6th$winten3_4==2,0,
                              ifelse(kwcs6th$winten3_4 %in% c(7,8,9),NA,
                                     kwcs6th$winten3_4))
table(kwcs6th$depend.automatic, exclude = T)

# depend.boss (yes 1 no 0)
head(kwcs6th$winten3_5)
table(kwcs6th$winten3_5, exclude = T)
kwcs6th$depend.boss<-ifelse(kwcs6th$winten3_5==2,0,
                                 ifelse(kwcs6th$winten3_5 %in% c(7,8,9),NA,
                                        kwcs6th$winten3_5))
table(kwcs6th$depend.boss, exclude = T)

# assess.customers (yes 1 no 0)
head(kwcs6th$ass_cust1)
table(kwcs6th$ass_cust1, exclude = T)
kwcs6th$assess.customers<-ifelse(kwcs6th$ass_cust1==2,0,
                                ifelse(kwcs6th$ass_cust1 %in% c(8,9),NA,
                                       kwcs6th$ass_cust1))
table(kwcs6th$assess.customers, exclude = T)

# repetitive.task (yes 1 no 0)
kwcs6th[kwcs6th$winten1_1==1 & kwcs6th$winten1_2==0,]

head(kwcs6th$winten1_2)
table(kwcs6th$winten1_2, exclude = T)
kwcs6th$repetitive.task<-ifelse(kwcs6th$winten1_2==2,0,
                                ifelse(kwcs6th$winten1_2 %in% c(8,9),NA,
                                       kwcs6th$winten1_2))

table(kwcs6th$repetitive.task, exclude = T)  

# alternating task (yes 1 no 0)
head(kwcs6th$alter_task1)
table(kwcs6th$alter_task1, exclude = T)
kwcs6th$alternating.task<-ifelse(kwcs6th$alter_task1==2,0,
                                ifelse(kwcs6th$alter_task1 %in% c(8,9),NA,
                                       kwcs6th$alter_task1))
table(kwcs6th$alternating.task, exclude = T)

# teamwork (yes 1 no 0)
head(kwcs6th$wteam1)
table(kwcs6th$wteam1, exclude = T)
kwcs6th$teamwork<-ifelse(kwcs6th$wteam1==2,0,
                         ifelse(kwcs6th$wteam1 %in% c(8,9),NA,
                                kwcs6th$wteam1))

table(kwcs6th$teamwork, exclude = T)

# emotion.manual (yes 1 no 0)
head(kwcs6th$emo_manual)
table(kwcs6th$emo_manual, exclude = T)
kwcs6th$emotion.manual<-ifelse(kwcs6th$emo_manual==2,0,
                               ifelse(kwcs6th$emo_manual %in% c(8,9),NA,
                                      kwcs6th$emo_manual))

table(kwcs6th$emotion.manual, exclude = T)

# meet.standard (yes 1 no 0)
head(kwcs6th$condim1)
table(kwcs6th$condim1, exclude = T)
kwcs6th$meet.standard<-ifelse(kwcs6th$condim1==2,0,
                             ifelse(kwcs6th$condim1 %in% c(8,9),NA,
                                    kwcs6th$condim1))
table(kwcs6th$meet.standard, exclude = T)

# assess.quality (yes 1 no 0)
head(kwcs6th$condim2)
table(kwcs6th$condim2, exclude = T)
kwcs6th$assess.quality<-ifelse(kwcs6th$condim2==2,0,
                              ifelse(kwcs6th$condim2 %in% c(8,9),NA,
                                     kwcs6th$condim2))

table(kwcs6th$assess.quality, exclude = T)

# solve.problem (yes 1 no 0)
head(kwcs6th$condim3)
table(kwcs6th$condim3, exclude = T)
kwcs6th$solve.problem<-ifelse(kwcs6th$condim3==2,0,
                              ifelse(kwcs6th$condim3 %in% c(8,9),NA,
                                     kwcs6th$condim3))

table(kwcs6th$solve.problem, exclude = T)

# monotonous.task (yes 1 no 0)
head(kwcs6th$condim4)
table(kwcs6th$condim4, exclude = T)
kwcs6th$monotonous.task<-ifelse(kwcs6th$condim4==2,0,
                                ifelse(kwcs6th$condim4 %in% c(8,9),NA,
                                      kwcs6th$condim4))

table(kwcs6th$monotonous.task, exclude = T)

# complex.task (yes 1 no 0)
head(kwcs6th$condim5)
table(kwcs6th$condim5, exclude = T)
kwcs6th$complex.task<-ifelse(kwcs6th$condim5==2,0,
                             ifelse(kwcs6th$condim5 %in% c(8,9),NA,
                                    kwcs6th$condim5))

table(kwcs6th$complex.task, exclude = T)

# learn.new (yes 1 no 0)
head(kwcs6th$condim6)
table(kwcs6th$condim6, exclude = T)
kwcs6th$learn.new<-ifelse(kwcs6th$condim6==2,0,
                          ifelse(kwcs6th$condim6 %in% c(8,9),NA,
                                 kwcs6th$condim6))

table(kwcs6th$learn.new, exclude = T)

# change.order (yes 1 no 0)
head(kwcs6th$decla1)
table(kwcs6th$decla1, exclude = T)
kwcs6th$change.order<-ifelse(kwcs6th$decla1==2,0,
                             ifelse(kwcs6th$decla1 %in% c(8,9),NA,
                                    kwcs6th$decla1))
table(kwcs6th$change.order, exclude = T)

# change.method (yes 1 no 0)
head(kwcs6th$decla2)
table(kwcs6th$decla2, exclude = T)
kwcs6th$change.method<-ifelse(kwcs6th$decla2==2,0,
                              ifelse(kwcs6th$decla2 %in% c(8,9),NA,
                                     kwcs6th$decla2))

table(kwcs6th$change.method, exclude = T)

# change.speed (yes 1 no 0)
head(kwcs6th$decla3)
table(kwcs6th$decla3, exclude = T)
kwcs6th$change.speed<-ifelse(kwcs6th$decla3==2,0,
                            ifelse(kwcs6th$decla3 %in% c(8,9),NA,
                                   kwcs6th$decla3))

table(kwcs6th$change.speed, exclude = T)


# suggest.efficiency (yes 1 no 0)
head(kwcs6th$suggest1)
table(kwcs6th$suggest1, exclude = T)
kwcs6th$suggest.efficiency<-ifelse(kwcs6th$suggest1==2,0,
                                   ifelse(kwcs6th$suggest1 %in% c(8,9),NA,
                                          kwcs6th$suggest1))
table(kwcs6th$suggest.efficiency, exclude = T)

# suggest.quality (yes 1 no 0)
head(kwcs6th$suggest2)
table(kwcs6th$suggest2, exclude = T)
kwcs6th$suggest.quality<-ifelse(kwcs6th$suggest2==2,0,
                                ifelse(kwcs6th$suggest2 %in% c(8,9),NA,
                                       kwcs6th$suggest2))
table(kwcs6th$suggest.quality, exclude = T)

# work.at.risk (yes 1 no 0)
head(kwcs6th$heal_risk)
table(kwcs6th$heal_risk, exclude = T)

kwcs6th$work.at.risk<-ifelse(kwcs6th$heal_risk %in% c(8,9),NA,
                             ifelse(kwcs6th$heal_risk==2,0,kwcs6th$heal_risk))

table(kwcs6th$work.at.risk, exclude = T)

# discrimination (yes 1 no 0)
head(kwcs6th$disc1) # Age
table(kwcs6th$disc1, exclude = T)

head(kwcs6th$disc2) 
table(kwcs6th$disc2, exclude = T)

head(kwcs6th$disc3) 
table(kwcs6th$disc3, exclude = T)

head(kwcs6th$disc4) 
table(kwcs6th$disc4, exclude = T)

head(kwcs6th$disc5) 
table(kwcs6th$disc5, exclude = T)

head(kwcs6th$disc6) 
table(kwcs6th$disc6, exclude = T)

head(kwcs6th$disc7) 
table(kwcs6th$disc7, exclude = T)

head(kwcs6th$disc8) 
table(kwcs6th$disc8, exclude = T)

head(kwcs6th$disc9) 
table(kwcs6th$disc9, exclude = T)

head(kwcs6th$disc10) 
table(kwcs6th$disc10, exclude = T)

head(kwcs6th$disc11) 
table(kwcs6th$disc11, exclude = T)


kwcs6th$sum.discrimination<-apply(kwcs6th[,c("disc1","disc2","disc3","disc4","disc5","disc6","disc7","disc8","disc9","disc10")],1,function(x) sum(x[!is.na(x)]==1))

table(kwcs6th$sum.discrimination, exclude = T)

# at least one discrimination, 1 otherwise 0
kwcs6th$discrimination<-ifelse(kwcs6th$sum.discrimination >= 1,1,0)
table(kwcs6th$discrimination, exclude = T)

# violence (yes 1 no 0)
head(kwcs6th$asb1) # Verbal abuse
table(kwcs6th$asb1, exclude = T)

head(kwcs6th$asb2) # Unwanted sexual attention
table(kwcs6th$asb2, exclude = T)

head(kwcs6th$asb3) # Threats
table(kwcs6th$asb3, exclude = T)

head(kwcs6th$asb4) # Humiliating behavior
table(kwcs6th$asb4, exclude = T)

head(kwcs6th$asb5) # Physical violence
table(kwcs6th$asb5, exclude = T)

head(kwcs6th$asb6) # Sexual harassment
table(kwcs6th$asb6, exclude = T)

head(kwcs6th$asb7) # Bullying / harassment
table(kwcs6th$asb7, exclude = T) 

kwcs6th$sum.violence<-apply(kwcs6th[,c("asb1","asb2","asb3","asb4","asb5","asb6","asb7")],1,function(x) sum(x[!is.na(x)]==1))

table(kwcs6th$sum.violence, exclude = T)

# at least one violence, 1 otherwise 0
kwcs6th$violence<-ifelse(kwcs6th$sum.violence >= 1,1,0)
table(kwcs6th$violence, exclude = T)

# performance.income (yes 1 no 0)
head(kwcs6th$income_pos6)
table(kwcs6th$income_pos6, exclude = T)

kwcs6th$performance.income<-ifelse(kwcs6th$income_pos6==2,0,
                                   ifelse(kwcs6th$income_pos6 %in% c(8,9),NA,
                                          kwcs6th$income_pos6))
table(kwcs6th$performance.income, exclude = T)


# danger.income (yes 1 no 0)
head(kwcs6th$income_pos4)
table(kwcs6th$income_pos4, exclude = T)
kwcs6th$danger.income<-ifelse(kwcs6th$income_pos4==2,0,
                                ifelse(kwcs6th$income_pos4 %in% c(8,9),NA,
                                       kwcs6th$income_pos4))
table(kwcs6th$danger.income, exclude = T)

# bonus.income (yes 1 no 0)
head(kwcs6th$income_pos8)
table(kwcs6th$income_pos8, exclude = T)

kwcs6th$bonus.income<-ifelse(kwcs6th$income_pos8==2,0,
                             ifelse(kwcs6th$income_pos8 %in% c(8,9),NA,
                                    kwcs6th$income_pos8))
table(kwcs6th$bonus.income, exclude = T)


# -------------------------------------------------------------------------
# Likert scale variables
# -------------------------------------------------------------------------
head(kwcs6th$wstat1)
table(kwcs6th$wstat1, exclude = T)

kwcs6th$well.paid<-ifelse(kwcs6th$wstat1 %in% c(7,8,9), NA,
                          kwcs6th$wstat1)

head(kwcs6th$wstat2)
table(kwcs6th$wstat2, exclude = T)
kwcs6th$career.prospects<-ifelse(kwcs6th$wstat2 %in% c(7,8,9), NA,
                                 kwcs6th$wstat2)

head(kwcs6th$wstat3)
table(kwcs6th$wstat3, exclude = T)

kwcs6th$recognition<-ifelse(kwcs6th$wstat3 %in% c(7,8,9), NA, 
                            kwcs6th$wstat3)

head(kwcs6th$wstat4)
table(kwcs6th$wstat4, exclude = T)

kwcs6th$competition<-ifelse(kwcs6th$wstat4 %in% c(7,8,9), NA, 
                            kwcs6th$wstat4)

head(kwcs6th$wstat5)
table(kwcs6th$wstat5, exclude = T)

kwcs6th$motivation<-ifelse(kwcs6th$wstat5 %in% c(7,8,9), NA, 
                           kwcs6th$wstat5)

head(kwcs6th$wstat6)
table(kwcs6th$wstat6, exclude = T)

kwcs6th$lose.job<-ifelse(kwcs6th$wstat6 %in% c(7,8,9), NA, 
                         kwcs6th$wstat6)

head(kwcs6th$wstat7)
table(kwcs6th$wstat7, exclude = T)

kwcs6th$new.job<-ifelse(kwcs6th$wstat7 %in% c(7,8,9), NA, 
                        kwcs6th$wstat7)

head(kwcs6th$comp_ass1)
table(kwcs6th$comp_ass1, exclude = T)

kwcs6th$appreciation<-ifelse(kwcs6th$comp_ass1 %in% c(7,8,9), NA,
                             kwcs6th$comp_ass1)

head(kwcs6th$comp_ass2)
table(kwcs6th$comp_ass2, exclude = T)

kwcs6th$management.trust<-ifelse(kwcs6th$comp_ass2 %in% c(7,8,9), NA,
                                 kwcs6th$comp_ass2)

head(kwcs6th$comp_ass3)
table(kwcs6th$comp_ass3, exclude = T)

kwcs6th$conflict.fair<-ifelse(kwcs6th$comp_ass3 %in% c(7,8,9), NA,
                              kwcs6th$comp_ass3)
head(kwcs6th$comp_ass4)
table(kwcs6th$comp_ass4, exclude = T)

kwcs6th$work.fair<-ifelse(kwcs6th$comp_ass4 %in% c(7,8,9), NA,
                          kwcs6th$comp_ass4)

head(kwcs6th$comp_ass5)
table(kwcs6th$comp_ass5, exclude = T)

kwcs6th$cooperation<-ifelse(kwcs6th$comp_ass5 %in% c(7,8,9), NA,
                            kwcs6th$comp_ass5)

head(kwcs6th$comp_ass6)
table(kwcs6th$comp_ass6, exclude = T)

kwcs6th$employee.trust<-ifelse(kwcs6th$comp_ass6 %in% c(7,8,9), NA,
                               kwcs6th$comp_ass6)

colnames(kwcs6th)

kwcs6th$ID<-paste0("id:",kwcs6th$id)

# Binary variables


binary<-c("gender","education","regular","full.time","shift",
          "depend.colleagues","depend.people","depend.target",
          "depend.automatic","depend.boss",
          "meet.standard","assess.quality","solve.problem",
          "monotonous.task","complex.task","learn.new",
          "change.order","change.method","change.speed",
          "assess.customers","repetitive.task","alternating.task",
          "teamwork","emotion.manual",
          "suggest.efficiency","suggest.quality",
          "work.at.risk","discrimination","violence",
          "performance.income","danger.income","bonus.income")

# Likert scale variables

likert<-c("well.paid","career.prospects","recognition",
          "competition","motivation","lose.job","new.job",
          "appreciation","management.trust","conflict.fair",
          "work.fair","cooperation","employee.trust")

subkwcs<-kwcs6th[,c("ID",binary,likert)]
colSums(is.na(subkwcs))

subkwcs<-subkwcs[complete.cases(subkwcs),]

subkwcs<-as.data.frame(subkwcs)

for (i in 1:length(binary)){
  subkwcs[,binary[i]]<-factor(subkwcs[,binary[i]])
}

# -------------------------------------------------------------------------
# comparisons
library(ClustOfVar)
# Binary variables

source("kmeansvar2.R") # to specify rename.level = T in PCAmix
set.seed(2022)
km.b<-kmeansvar2(X.quali=subkwcs[,binary],init=3)
ref.b<-km.b$cluster

nseed<-50
bb<-list()
for (i in 1:nseed){
  set.seed(i+10000)
  km.b<-kmeansvar2(X.quali=subkwcs[,binary],init=3)
  bb[[i]]<-km.b$cluster
}

library(clevr)

eval.b<-matrix(NA,nrow=nseed,ncol=8)
colnames(eval.b)<-c("homogeneity","completeness","v_measure",
                    "rand_index","adj_rand_index","variation_info",
                    "mutual_info","fowlkes_mallows")
for (i in 1:nseed){
  eval.b[i,]<-unlist(eval_report_clusters(ref.b,bb[[i]]))
}

MCA.b<-apply(eval.b,2,function(x) mean(x))
MCA.b
# when Likert scale variables are treated as continuous variables
set.seed(2022)
km.q<-kmeansvar(X.quanti=subkwcs[,likert],init=3)
ref.q<-km.q$cluster

nseed<-50
qq.c<-list()
for (i in 1:nseed){
  set.seed(i+10000)
  km.q<-kmeansvar(X.quanti=subkwcs[,likert],init=3)
  qq.c[[i]]<-km.q$cluster
}


eval.q.c<-matrix(NA,nrow=nseed,ncol=8)
colnames(eval.q.c)<-c("homogeneity","completeness","v_measure",
                    "rand_index","adj_rand_index","variation_info",
                    "mutual_info","fowlkes_mallows")
for (i in 1:nseed){
  eval.q.c[i,]<-unlist(eval_report_clusters(ref.q,qq.c[[i]]))
}

PCA.q<-apply(eval.q.c,2,function(x) mean(x))
PCA.q

# change Likert scale variables into factor variables

for (i in 1:length(likert)){
  subkwcs[,likert[i]]<-factor(subkwcs[,likert[i]])
  levels(subkwcs[,likert[i]])<-c("SA","A","N","D","SD")
}

# when Likert scale variables are treated as qualitative variables
str(subkwcs)

set.seed(2022)
km.q<-kmeansvar2(X.quali=subkwcs[,likert],init=3)
ref.q<-km.q$cluster

nseed<-50
qq<-list()
for (i in 1:nseed){
  set.seed(i+10000)
  km.q<-kmeansvar2(X.quali=subkwcs[,likert],init=3)
  qq[[i]]<-km.q$cluster
}

eval.q<-matrix(NA,nrow=nseed,ncol=8)
colnames(eval.q)<-c("homogeneity","completeness","v_measure",
                    "rand_index","adj_rand_index","variation_info",
                    "mutual_info","fowlkes_mallows")
for (i in 1:nseed){
  eval.q[i,]<-unlist(eval_report_clusters(ref.q,qq[[i]]))
}

MCA.q<-apply(eval.q,2,function(x) mean(x))
MCA.q

# save the final data set for the analysis
saveRDS(subkwcs, file="Final_kwcs6th.rds")


# -------------------------------------------------------------------------
# Tensor representation
# -------------------------------------------------------------------------
library(multiway)

dat<-subkwcs
n<-nrow(dat)
data.m.array<-array(NA, dim = c(n,length(binary), length(likert)))


for (j in 1:length(binary)){
  for (k in 1:length(likert)){
    tt<-table(dat[,binary[j]],dat[,likert[k]])
    
    for (i in 1:n){
      a<-dat[i,binary[j]]
      b<-dat[i,likert[k]]
      
      data.m.array[i,j,k]<-tt[a,b]/n
    }
  }
}

set.seed(2022)

pfac<-list()
cc<-c()

for (i in 1:5){
  pfac[[i]]<-parafac(data.m.array,nfac=i)
  cc[i]<-corcondia(data.m.array, pfac[[i]])
}


cc


saveRDS(pfac, file="pfac.rds")
saveRDS(data.m.array, file="tensor.rds")

# -------------------------------------------------------------------------
# load data

subkwcs<-readRDS(file="Final_kwcs6th.rds")
pfac<-readRDS(file="pfac.rds")
data.m.array<-readRDS(file="tensor.rds")

# mode 1: survey respondents

data.u1<-as.data.frame(pfac[[2]]$A)
rownames(data.u1)<-dat$ID

set.seed(2022)
km<-kmeans(data.u1,4)
km

km$betweenss/km$totss
km$withinss/km$totss
km$tot.withinss/km$totss

library(factoextra)
fviz_nbclust(data.u1, kmeans, method='wss')

data.u1$km.cluster<-as.factor(km$cluster)


binary<-c("gender","education","regular","full.time","shift",
          "depend.colleagues","depend.people","depend.target",
          "depend.automatic","depend.boss",
          "meet.standard","assess.quality","solve.problem",
          "monotonous.task","complex.task","learn.new",
          "change.order","change.method","change.speed",
          "assess.customers","repetitive.task","alternating.task",
          "teamwork","emotion.manual",
          "suggest.efficiency","suggest.quality",
          "work.at.risk","discrimination","violence",
          "performance.income","danger.income","bonus.income")

# Likert scale variables

likert<-c("well.paid","career.prospects","recognition",
          "competition","motivation","lose.job","new.job",
          "appreciation","management.trust","conflict.fair",
          "work.fair","cooperation","employee.trust")

str(data.u1)
data.u1.k<-merge(dat, data.u1, by.x="ID", by.y="row.names")

# Binary variables

mat<-matrix(NA, nrow=2*length(binary), ncol=5)
rownames(mat)<-paste(rep(binary,each=2),rep(0:1,length(binary)),sep="=")
colnames(mat)<-c("total%","k=1_col%","k=2_col%","k=3_col%","k=4_col%")

for (i in 1:length(binary)){
  x<-round(table(data.u1.k[,binary[i]])/nrow(data.u1.k),digits=3)*100
  x.k<-round(prop.table(table(data.u1.k[,binary[i]],data.u1.k$km.cluster),margin = 2),digits=3)*100
  mat[(2*i-1):(2*i),]<-cbind(as.matrix(x),x.k)
}

mat
mat[seq(2,2*length(binary),by=2),]

# Likert scale variables

mat2<-matrix(NA, nrow=5*length(likert), ncol=5)
rownames(mat2)<-paste(rep(likert,each=5),rep(c("SA", "A", "N", "D", "SD"),length(likert)),sep="=")
colnames(mat2)<-c("total%","k=1_col%","k=2_col%","k=3_col%","k=4_col%")

for (i in 1:length(likert)){
  x<-round(table(data.u1.k[,likert[i]])/nrow(data.u1.k),digits=3)*100
  x.k<-round(prop.table(table(data.u1.k[,likert[i]],data.u1.k$km.cluster),margin = 2),digits=3)*100
  mat2[(5*i-4):(5*i),]<-cbind(as.matrix(x),x.k)
}

mat2


# -------------------------------------------------------------------------
# Figure 
# -------------------------------------------------------------------------

library(gridExtra)
library(ggplot2)
library(ggpubr)

theme_set(theme_classic())

data.u1.k$km.cluster1<-ifelse(data.u1.k$km.cluster==1,"cluster1",
                              ifelse(data.u1.k$km.cluster==2,"cluster2",
                                     ifelse(data.u1.k$km.cluster==3,"cluster3","cluster4")))

ps1=ggplot(data.u1.k, aes(fill=well.paid, x=km.cluster1)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle(label="Considering all my efforts and achievements in my job,\nI feel I get paid appropriately.")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 11))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))


ps2=ggplot(data.u1.k, aes(fill=career.prospects, x=km.cluster1)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("My job offers good prospects for career advancement.\n")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 11))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))


ps3=ggplot(data.u1.k, aes(fill=recognition, x=km.cluster1)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("I receive the recognition I deserve for my work.\n")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 11))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))


ps4=ggplot(data.u1.k, aes(fill=competition, x=km.cluster1)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("I feel I strongly compete with others in and for work.\n")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 11))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

ps5=ggplot(data.u1.k, aes(fill=motivation, x=km.cluster1)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("The organisation I work for motivates me to give my \nbest job performance.")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 11))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

ps6=ggplot(data.u1.k, aes(fill=lose.job, x=km.cluster1)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("I might lose my job in the next 6 months.\n")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 11))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

ps7=ggplot(data.u1.k, aes(fill=new.job, x=km.cluster1)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("If I were to lose or quit my current job, it would be easy \nfor me to find a job of similar salary.")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 11))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

pw1=ggplot(data.u1.k, aes(fill=appreciation, x=km.cluster1)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("Employees are appreciated when they have done \na good job.")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 11))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

pw2=ggplot(data.u1.k, aes(fill=management.trust, x=km.cluster1)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("The management trusts the employees to do their work\n well.")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 11))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

pw3=ggplot(data.u1.k, aes(fill=conflict.fair, x=km.cluster1)) + 
  geom_bar(position="fill",colour="black")+  
  ggtitle("Conflicts are resolved in a fair way.\n")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 11))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

pw4=ggplot(data.u1.k, aes(fill=work.fair, x=km.cluster1)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("The work is distributed fairly.\n")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 11))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

pw5=ggplot(data.u1.k, aes(fill=cooperation, x=km.cluster1)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("There is good cooperation between you and your \ncolleagues.")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 11))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

pw6=ggplot(data.u1.k, aes(fill=employee.trust, x=km.cluster1)) + 
  geom_bar(position="fill",colour="black")+
  ggtitle("In general, employees trust management.\n")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size = 11))+
  scale_fill_brewer(palette = "Blues",guide=guide_legend(title=NULL))

ggarrange(ps1,ps2,ps3,ps4,ps5,ps6,ps7,pw1,pw2,pw3,pw4,pw5,pw6, nrow=7, ncol=2, common.legend=TRUE, legend="bottom", labels = c("A","B","C","D","E","F","G","H","I","J","K","L","M"))


# -------------------------------------------------------------------------
# mode 2: binary variables

data.u2<-as.data.frame(pfac[[2]]$B)
rownames(data.u2)<-binary

set.seed(2022)
km<-kmeans(data.u2,3)
km

dt<-data.u2
dt$km.cluster<-as.factor(km$cluster)
rownames(dt[dt$km.cluster==1,])
rownames(dt[dt$km.cluster==2,])
rownames(dt[dt$km.cluster==3,])


library(ggrepel)

g2<-ggplot(data = dt,aes(x = V1, y = V2, color = km.cluster, 
                           shape = km.cluster,
                           label = rownames(dt))) +
  geom_point() +
  geom_text_repel(max.overlaps = 100, size = 3) +
  scale_color_brewer(palette = "Set1") +
  ggtitle("SSB/SST=92.9%") +
  xlab("b1") + ylab("b2") +
  theme_bw() +
  theme(legend.position = "none")

g2


# mode 3: Likert scale variables
data.u3<-as.data.frame(pfac[[2]]$C)
rownames(data.u3)<-likert

set.seed(2022)
km<-kmeans(data.u3,3)
km

dt2<-data.u3
dt2$km.cluster<-as.factor(km$cluster)

dt2[dt2$km.cluster==1,]
dt2[dt2$km.cluster==2,]
dt2[dt2$km.cluster==3,]


g3<-ggplot(data = dt2, aes(x = V1, y = V2, color = km.cluster, 
                             shape = km.cluster,
                             label = rownames(dt2))) +
  geom_point() +
  geom_text_repel(max.overlaps = 100, size = 3) +
  scale_color_brewer(palette = "Set1") +
  ggtitle("SSB/SST=93.5%") +
  xlab("c1") + ylab("c2") +
  theme_bw() +
  theme(legend.position = "none")

g3


# -------------------------------------------------------------------------
# Figure 
# -------------------------------------------------------------------------
ggarrange(g2, g3, labels = c("A","B"), ncol = 1) 
# -------------------------------------------------------------------------

# check if clustering results change greatly with different seed numbers.
# Binary variables
set.seed(2022)
km<-kmeans(data.u2,3)
ref.b.t<-km$cluster
ref.b.t

nseed<-50
bb.t<-list()
for (i in 1:nseed){
  set.seed(i+10000)
  km.b<-kmeans(data.u2,3)
  bb.t[[i]]<-km.b$cluster
}

eval.b.t<-matrix(NA,nrow=nseed,ncol=8)
colnames(eval.b.t)<-c("homogeneity","completeness","v_measure",
                    "rand_index","adj_rand_index","variation_info",
                    "mutual_info","fowlkes_mallows")
for (i in 1:nseed){
  eval.b.t[i,]<-unlist(eval_report_clusters(ref.b.t,bb.t[[i]]))
}

Tensor.b<-apply(eval.b.t,2,function(x) mean(x))
Tensor.b

# Likert scale variables

set.seed(2022)
km<-kmeans(data.u3,3)
ref.q.t<-km$cluster
ref.q.t

nseed<-50
qq.t<-list()
for (i in 1:nseed){
  set.seed(i+10000)
  km.q<-kmeans(data.u3,3)
  qq.t[[i]]<-km.q$cluster
}

eval.q.t<-matrix(NA,nrow=nseed,ncol=8)
colnames(eval.q.t)<-c("homogeneity","completeness","v_measure",
                      "rand_index","adj_rand_index","variation_info",
                      "mutual_info","fowlkes_mallows")
for (i in 1:nseed){
  eval.q.t[i,]<-unlist(eval_report_clusters(ref.q.t,qq.t[[i]]))
}

Tensor.q<-apply(eval.q.t,2,function(x) mean(x))
Tensor.q

round(t(rbind(MCA.b,Tensor.b,PCA.q,MCA.q,Tensor.q)),digits=3)

