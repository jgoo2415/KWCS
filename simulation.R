# -------------------------------------------------------------------------
# Title: Tensor-based clustering of survey respondents, binary variables, 
# and Likert scale variables
# Authors: Juna Goo (junagoo@boisestate.edu), Seulbi Shin
# -------------------------------------------------------------------------

library(rTensor)
library(nnTensor)
library(clevr)

result.b<-matrix(NA,8,8)
rownames(result.b)<-c("homogeneity","completeness","v_measure",
                      "rand_index","adj_rand_index","variation_info",
                      "mutual_info","fowlkes_mallows")
colnames(result.b)<-c("Frobenius", "KL","Pearson", 
                      "Hellinger", "Neyman","HALS","cp1","cp2")

result.q<-matrix(NA,8,8)
rownames(result.q)<-c("homogeneity","completeness","v_measure",
                      "rand_index","adj_rand_index","variation_info",
                      "mutual_info","fowlkes_mallows")
colnames(result.q)<-c("Frobenius", "KL","Pearson", 
                      "Hellinger", "Neyman","HALS","cp1","cp2")

ans.b<-c(1,1,2,2,3,3)
names(ans.b)<-c("gender", "age","work.life.balance","education",
                "income","work.period")
ans.b<-ans.b[order(names(ans.b))]
ans.b

ans.q<-c(1,2,3,3,1,2,2)
names(ans.q)<-c(paste0("Y",1:7))
ans.q<-ans.q[order(names(ans.q))]
ans.q

eval.b<-list()
eval.q<-list()

tot<-2000

for (j in 1:8){
  eval.b[[j]]<-matrix(NA,tot,8)
  eval.q[[j]]<-matrix(NA,tot,8)
}


n<-500
b.lst<-list(c(0,0),c(0,1),c(1,0))

for (nsim in 1:tot){
  set.seed(nsim+100000)
  
  b.group1<-rbinom(n,1,prob=0.5)  
  b.group2<-rbinom(n,1,prob=0.5)   
  b.group3<-rbinom(n,1,prob=0.5) 
  
  b.mat1<-data.frame(gender=NA,age=NA)
  
  for (i in 1:n){
    if (b.group1[i]==1){
      b.mat1[i,]<-c(1,1)
    }else{
      x<-sample(1:3,1,prob=c(1/3,1/3,1/3))
      b.mat1[i,]<-b.lst[[x]]
    }
  }
  b.mat2<-data.frame(work.life.balance=NA,education=NA)
  
  for (i in 1:n){
    if (b.group2[i]==1){
      b.mat2[i,]<-c(1,1)
    }else{
      x<-sample(1:3,1,prob=c(1/3,1/3,1/3))
      b.mat2[i,]<-b.lst[[x]]
    }
  }
  b.mat3<-data.frame(income=NA,work.period=NA)
  
  for (i in 1:n){
    if (b.group3[i]==1){
      b.mat3[i,]<-c(1,1)
    }else{
      x<-sample(1:3,1,prob=c(1/3,1/3,1/3))
      b.mat3[i,]<-b.lst[[x]]
    }
  }
  dat<-data.frame(cbind(b.mat1,b.mat2,b.mat3),
                  Y1=NA,Y2=NA,Y3=NA,Y4=NA,Y5=NA,Y6=NA,Y7=NA)
  
  # gender & age  <-> Y1, Y5
  
  for (i in c("Y1","Y5")){
    m<-nrow(dat[dat$gender==1 & dat$age==1,]) 
    # male & younger 
    freq<-rmultinom(1,m,prob=c(0.8,0.1,0.05,0.03,0.02))
    a<-sample(c(rep("SA",freq[1,]),
                rep("A",freq[2,]),
                rep("N",freq[3,]),
                rep("D",freq[4,]),
                rep("SD",freq[5,])))
    dat[dat$gender==1 & dat$age==1,i]<-a
  }
  
  for (i in c("Y1","Y5")){
    m<-nrow(dat[!(dat$gender==1 & dat$age==1),])
    freq<-rmultinom(1,m,prob=c(0.2,0.2,0.2,0.2,0.2)) 
    a<-sample(c(rep("SA",freq[1,]),
                rep("A",freq[2,]),
                rep("N",freq[3,]),
                rep("D",freq[4,]),
                rep("SD",freq[5,])))
    dat[!(dat$gender==1 & dat$age==1),i]<-a
  }
  
  # work.life.balance & education  <-> Y3, Y4
  
  for (i in c("Y3","Y4")){
    m<-nrow(dat[dat$work.life.balance==1 & dat$education==1,]) 
    freq<-rmultinom(1,m,prob=c(0.02,0.03,0.05,0.1,0.8))
    a<-sample(c(rep("SA",freq[1,]),
                rep("A",freq[2,]),
                rep("N",freq[3,]),
                rep("D",freq[4,]),
                rep("SD",freq[5,])))
    dat[dat$work.life.balance==1 & dat$education==1,i]<-a
  }
  
  for (i in c("Y3","Y4")){
    m<-nrow(dat[!(dat$work.life.balance==1 & dat$education==1),]) 
    freq<-rmultinom(1,m,prob=c(0.2,0.2,0.2,0.2,0.2)) 
    a<-sample(c(rep("SA",freq[1,]),
                rep("A",freq[2,]),
                rep("N",freq[3,]),
                rep("D",freq[4,]),
                rep("SD",freq[5,])))
    dat[!(dat$work.life.balance==1 & dat$education==1),i]<-a
  }
  
  # income & work.period  <-> Y2, Y6, Y7 
  
  for (i in c("Y2","Y6","Y7")){
    m<-nrow(dat[dat$income==1 & dat$work.period==1,]) 
    freq<-rmultinom(1,m,prob=c(0.05,0.1,0.7,0.1,0.05))
    a<-sample(c(rep("SA",freq[1,]),
                rep("A",freq[2,]),
                rep("N",freq[3,]),
                rep("D",freq[4,]),
                rep("SD",freq[5,])))
    dat[dat$income==1 & dat$work.period==1,i]<-a
  }
  
  for (i in c("Y2","Y6","Y7")){
    m<-nrow(dat[!(dat$income==1 & dat$work.period==1),]) 
    freq<-rmultinom(1,m,prob=c(0.2,0.2,0.2,0.2,0.2)) 
    a<-sample(c(rep("SA",freq[1,]),
                rep("A",freq[2,]),
                rep("N",freq[3,]),
                rep("D",freq[4,]),
                rep("SD",freq[5,])))
    dat[!(dat$income==1 & dat$work.period==1),i]<-a
  }
  
  for (i in 1:6){
    dat[,i]<-factor(dat[,i],levels=c("0","1"))
  }
  
  for (i in 7:13){
    dat[,i]<-factor(dat[,i],levels=c("SA","A","N","D","SD"))
  }
  
  m.array<-array(NA, dim=c(n,6,7))
  
  for (j in 1:6){
    for (k in 1:7){
      kk<-k+6
      tt<-table(dat[,j],dat[,kk])
      
      for (i in 1:n){
        a<-dat[i,j]
        b<-dat[i,kk]
        
        if (b=="SA"){
          m.array[i,j,k]<-tt[a,"SA"]/n+tt[a,"A"]/n
        }else if (b=="SD"){
          m.array[i,j,k]<-tt[a,"SD"]/n+tt[a,"D"]/n
        }else{
          m.array[i,j,k]<-tt[a,b]/n
        }
      }
    }
  }
  
  m.tnsr<-as.tensor(m.array)
  
  m.array0<-array(NA, dim=c(n,6,7))
  
  for (j in 1:6){
    for (k in 1:7){
      kk<-k+6
      tt<-table(dat[,j],dat[,kk])
      
      for (i in 1:n){
        a<-dat[i,j]
        b<-dat[i,kk]
        
        m.array0[i,j,k]<-tt[a,b]/n
      }
    }
  }
  
  m.tnsr0<-as.tensor(m.array0)
  for (j in 1:8){
    set.seed(nsim+200000)
    if (!colnames(result.b)[j]%in%c("cp1","cp2")){
      
      boolFalse<-F
      while(boolFalse==F)
      {
        tryCatch({
          tnsr<-NTF(m.tnsr,rank=2,algorithm=colnames(result.b)[j],
                    num.iter=100,init="Random")
          boolFalse<-T
        },error=function(e){
        },finally={})
      }
      
      cp.u2<-as.data.frame(t(tnsr$A[[2]]))
      rownames(cp.u2)<-colnames(dat)[1:6]
      km2<-kmeans(cp.u2,3)
      cp.u2.kmeans<-km2$cluster
      cp.u2.kmeans<-cp.u2.kmeans[order(factor(names(cp.u2.kmeans)))]
      eval.b[[j]][nsim,]<-unlist(eval_report_clusters(ans.b, cp.u2.kmeans))
      
      cp.u3<-as.data.frame(t(tnsr$A[[3]]))
      rownames(cp.u3)<-colnames(dat)[7:13]
      km3<-kmeans(cp.u3,3)
      cp.u3.kmeans<-km3$cluster
      cp.u3.kmeans<-cp.u3.kmeans[order(factor(names(cp.u3.kmeans)))]
      eval.q[[j]][nsim,]<-unlist(eval_report_clusters(ans.q, cp.u3.kmeans))
    }else if(colnames(result.b)[j]=="cp1"){
      invisible({capture.output({tnsr<-cp(m.tnsr0,num_components=2,
                                          max_iter=100)})})
      cp.u2<-as.data.frame(tnsr$U[[2]])
      rownames(cp.u2)<-colnames(dat)[1:6]
      km2<-kmeans(cp.u2,3)
      cp.u2.kmeans<-km2$cluster
      cp.u2.kmeans<-cp.u2.kmeans[order(factor(names(cp.u2.kmeans)))]
      eval.b[[j]][nsim,]<-unlist(eval_report_clusters(ans.b, cp.u2.kmeans))
      
      cp.u3<-as.data.frame(tnsr$U[[3]])
      rownames(cp.u3)<-colnames(dat)[7:13]
      km3<-kmeans(cp.u3,3)
      cp.u3.kmeans<-km3$cluster
      cp.u3.kmeans<-cp.u3.kmeans[order(factor(names(cp.u3.kmeans)))]
      eval.q[[j]][nsim,]<-unlist(eval_report_clusters(ans.q, cp.u3.kmeans))
    }else{
      invisible({capture.output({tnsr<-cp(m.tnsr,num_components=2,
                                          max_iter=100)})})
      cp.u2<-as.data.frame(tnsr$U[[2]])
      rownames(cp.u2)<-colnames(dat)[1:6]
      km2<-kmeans(cp.u2,3)
      cp.u2.kmeans<-km2$cluster
      cp.u2.kmeans<-cp.u2.kmeans[order(factor(names(cp.u2.kmeans)))]
      eval.b[[j]][nsim,]<-unlist(eval_report_clusters(ans.b, cp.u2.kmeans))
      
      cp.u3<-as.data.frame(tnsr$U[[3]])
      rownames(cp.u3)<-colnames(dat)[7:13]
      km3<-kmeans(cp.u3,3)
      cp.u3.kmeans<-km3$cluster
      cp.u3.kmeans<-cp.u3.kmeans[order(factor(names(cp.u3.kmeans)))]
      eval.q[[j]][nsim,]<-unlist(eval_report_clusters(ans.q, cp.u3.kmeans))
    }
  }
}

for (i in 1:8){
  result.b[,i]<-apply(eval.b[[i]],2,mean)
  result.q[,i]<-apply(eval.q[[i]],2,mean)
}

round(result.b,digit=3)
round(result.q,digit=3)

saveRDS(result.b,"result-b.rds")
saveRDS(result.q,"result-q.rds")
