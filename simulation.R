# -------------------------------------------------------------------------
# Title: Tensor-based clustering of survey respondents, binary variables, 
# and Likert scale variables
# Authors: Juna Goo (junagoo@boisestate.edu), Seulbi Shin
# -------------------------------------------------------------------------

source("kmeansvar2.R") # to specify rename.level = T in PCAmix

# Note: When all the variables are quantitative, PCAmix is the same as standard PCA.
# Note: When all the variables are qualitative, PCAmix is the same as standard MCA.

library(ClustOfVar)
library(multiway)
library(clevr)

ranks<-c(2,3,4,5)

result.b<-matrix(NA,8,5)
rownames(result.b)<-c("homogeneity","completeness","v_measure",
                      "rand_index","adj_rand_index","variation_info",
                      "mutual_info","fowlkes_mallows")

colnames(result.b)<-c("kmeansvar_quali","cp_R2","cp_R3","cp_R4","cp_R5")

result.q<-matrix(NA,8,6)
rownames(result.q)<-c("homogeneity","completeness","v_measure",
                      "rand_index","adj_rand_index","variation_info",
                      "mutual_info","fowlkes_mallows")
colnames(result.q)<-c("kmeansvar_quanti","kmeansvar_quali",
                      "cp_R2","cp_R3","cp_R4","cp_R5")

ans.b<-c(1,1,2,2,3,3)
names(ans.b)<-c("gender", "education","regular",
                "full.time","shift","discrimination")
ans.b

ans.q<-c(1,2,3,3,1,2,2)
names(ans.q)<-c(paste0("Y",1:7))
ans.q

eval.b<-list()
eval.q<-list()

tot<-2000

for (j in 1:5){
  eval.b[[j]]<-matrix(NA,tot,8)
}
for (j in 1:6){
  eval.q[[j]]<-matrix(NA,tot,8)
}

cc<-matrix(NA,tot,4)

n<-500
b.lst<-list(c(0,0),c(0,1),c(1,0))

for (nsim in 1:tot){
  set.seed(nsim+100000)
  
  b.group1<-rbinom(n,1,prob=0.5)  
  b.group2<-rbinom(n,1,prob=0.5)   
  b.group3<-rbinom(n,1,prob=0.5) 
  
  b.mat1<-data.frame(gender=NA,education=NA)
  
  for (i in 1:n){
    if (b.group1[i]==1){
      b.mat1[i,]<-c(1,1)
    }else{
      x<-sample(1:3,1,prob=c(1/3,1/3,1/3))
      b.mat1[i,]<-b.lst[[x]]
    }
  }
  b.mat1<-b.mat1[sample(nrow(b.mat1)),]
  
  b.mat2<-data.frame(regular=NA,full.time=NA)
  
  for (i in 1:n){
    if (b.group2[i]==1){
      b.mat2[i,]<-c(1,1)
    }else{
      x<-sample(1:3,1,prob=c(1/3,1/3,1/3))
      b.mat2[i,]<-b.lst[[x]]
    }
  }
  b.mat2<-b.mat2[sample(nrow(b.mat2)),]
  
  b.mat3<-data.frame(shift=NA,discrimination=NA)
  
  for (i in 1:n){
    if (b.group3[i]==1){
      b.mat3[i,]<-c(1,1)
    }else{
      x<-sample(1:3,1,prob=c(1/3,1/3,1/3))
      b.mat3[i,]<-b.lst[[x]]
    }
  }
  b.mat3<-b.mat3[sample(nrow(b.mat3)),]
  
  dat<-data.frame(cbind(b.mat1,b.mat2,b.mat3),
                  Y1=NA,Y2=NA,Y3=NA,Y4=NA,Y5=NA,Y6=NA,Y7=NA,
                  Y1.c=NA,Y2.c=NA,Y3.c=NA,Y4.c=NA,Y5.c=NA,Y6.c=NA,Y7.c=NA)
  
  # gender & education  <-> Y1, Y5
  
  for (i in c("Y1","Y5")){
    m<-nrow(dat[dat$gender==1 & dat$education==1,]) 
    # male & younger 
    freq<-rmultinom(1,m,prob=c(0.3,0.6,0.05,0.03,0.02))
    a<-sample(c(rep("SA",freq[1,]),
                rep("A",freq[2,]),
                rep("N",freq[3,]),
                rep("D",freq[4,]),
                rep("SD",freq[5,])))
    dat[dat$gender==1 & dat$education==1,i]<-a
  }
  
  for (i in c("Y1","Y5")){
    m<-nrow(dat[!(dat$gender==1 & dat$education==1),]) 
    # male & younger 
    freq<-rmultinom(1,m,prob=c(0.2,0.2,0.2,0.2,0.2))
    a<-sample(c(rep("SA",freq[1,]),
                rep("A",freq[2,]),
                rep("N",freq[3,]),
                rep("D",freq[4,]),
                rep("SD",freq[5,])))
    dat[!(dat$gender==1 & dat$education==1),i]<-a
  }
  
  # regular & full.time  <-> Y3, Y4
  
  for (i in c("Y3","Y4")){
    m<-nrow(dat[dat$regular==1 & dat$full.time==1,]) 
    freq<-rmultinom(1,m,prob=c(0.02,0.03,0.05,0.6,0.3))
    a<-sample(c(rep("SA",freq[1,]),
                rep("A",freq[2,]),
                rep("N",freq[3,]),
                rep("D",freq[4,]),
                rep("SD",freq[5,])))
    dat[dat$regular==1 & dat$full.time==1,i]<-a
  }
  
  for (i in c("Y3","Y4")){
    m<-nrow(dat[!(dat$regular==1 & dat$full.time==1),]) 
    freq<-rmultinom(1,m,prob=c(0.2,0.2,0.2,0.2,0.2))
    a<-sample(c(rep("SA",freq[1,]),
                rep("A",freq[2,]),
                rep("N",freq[3,]),
                rep("D",freq[4,]),
                rep("SD",freq[5,])))
    dat[!(dat$regular==1 & dat$full.time==1),i]<-a
  }
  
  
  # shift & discrimination  <-> Y2, Y6, Y7 
  
  for (i in c("Y2","Y6","Y7")){
    m<-nrow(dat[dat$shift==1 & dat$discrimination==1,]) 
    freq<-rmultinom(1,m,prob=c(0.05,0.15,0.6,0.15,0.05))
    a<-sample(c(rep("SA",freq[1,]),
                rep("A",freq[2,]),
                rep("N",freq[3,]),
                rep("D",freq[4,]),
                rep("SD",freq[5,])))
    dat[dat$shift==1 & dat$discrimination==1,i]<-a
  }
  
  for (i in c("Y2","Y6","Y7")){
    m<-nrow(dat[!(dat$shift==1 & dat$discrimination==1),]) 
    freq<-rmultinom(1,m,prob=c(0.2,0.2,0.2,0.2,0.2))
    a<-sample(c(rep("SA",freq[1,]),
                rep("A",freq[2,]),
                rep("N",freq[3,]),
                rep("D",freq[4,]),
                rep("SD",freq[5,])))
    dat[!(dat$shift==1 & dat$discrimination==1),i]<-a
  }
  

  for (i in 1:6){
    dat[,i]<-factor(dat[,i],levels=c("0","1"))
  }
  
  for (i in 7:13){
    dat[,i]<-factor(dat[,i],levels=c("SA","A","N","D","SD"))
  }
  
  rownames(dat)<-NULL
  
  for (i in 14:20){
    j<-i-7
    dat[,i]<-ifelse(dat[,j]=="SA",1,
                    ifelse(dat[,j]=="A",2,
                           ifelse(dat[,j]=="N",3,
                                  ifelse(dat[,j]=="D",4,5))))
  }
  
  # Binary variables
  km.b<-kmeansvar2(X.quali=dat[,1:6],init=3)
  eval.b[[1]][nsim,]<-unlist(eval_report_clusters(ans.b, km.b$cluster))
  
  # when Likert scale variables are treated as continuous variables
  km.q<-kmeansvar(X.quanti=dat[,14:20],init=3)
  eval.q[[1]][nsim,]<-unlist(eval_report_clusters(ans.q, km.q$cluster))
  
  # when Likert scale variables are treated as nominal variables
  km.q2<-kmeansvar2(X.quali=dat[,7:13],init=3)
  eval.q[[2]][nsim,]<-unlist(eval_report_clusters(ans.q, km.q2$cluster))
  
  m.array<-array(NA, dim=c(n,6,7))
  
  for (j in 1:6){
    for (k in 1:7){
      kk<-k+6
      tt<-table(dat[,j],dat[,kk])
      
      for (i in 1:n){
        a<-dat[i,j]
        b<-dat[i,kk]
        
        m.array[i,j,k]<-tt[a,b]/n
      }
    }
  }
  
  
  for (j in 1:4){
    set.seed(nsim+100000)
    R<-ranks[j]
    invisible({capture.output({pfac<-parafac(m.array,nfac=R)})})
    # same as cpd model
    cc[nsim,j]<-corcondia(m.array, pfac)
    
    u2<-as.data.frame(pfac$B)
    rownames(u2)<-colnames(dat)[1:6]
    km.u2<-kmeans(u2,3)
    km.cp.u2<-km.u2$cluster
    eval.b[[j+1]][nsim,]<-unlist(eval_report_clusters(ans.b, km.cp.u2))

    u3<-as.data.frame(pfac$C)
    rownames(u3)<-colnames(dat)[7:13]
    km.u3<-kmeans(u3,3)
    km.cp.u3<-km.u3$cluster
    eval.q[[j+2]][nsim,]<-unlist(eval_report_clusters(ans.q, km.cp.u3))
  } 

}

for (i in 1:5){
  result.b[,i]<-apply(eval.b[[i]],2,function(x) mean(x))
}
for (i in 1:6){
  result.q[,i]<-apply(eval.q[[i]],2,function(x) mean(x))
}

round(result.b,digit=3)
round(result.q,digit=3)

round(apply(cc,2,function(x) mean(x)),digits=2)


saveRDS(result.b,"result-b.rds")
saveRDS(result.q,"result-q.rds")
