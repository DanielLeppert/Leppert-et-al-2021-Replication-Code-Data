# This code computes the insurance contracts and quantile risk premiums.
# Please download data files and load into your RStudio workspace. 
# Running this code results in data found in output folder which can alternatively be downloaded directly
# from the main github repository. 

library(quantreg)
library(e1071)


cdd <- list()
# we analyze indices using each interpolation method
cdd[[1]] <- cdd_idw
cdd[[2]] <- cdd_ok
cdd[[3]] <- cdd_rk

pay.contract1 <- list()
pay.contract2 <- list()

eut.rev <- list()
eut.rev.contract1 <- list()
eut.rev.contract2 <- list()

wil.tes.contract1_vs_noi <- list()
wil.tes.contract2_vs_noi <- list()
wil.tes.contract2_vs_contract1 <- list()

RP_ins1_EU <- list()
RP_ins1_EU_Q1 <- list()
RP_ins1_EU_Q2 <- list()
RP_ins1_EU_Q3 <- list()
RP_ins1_EU_Q4 <- list()
RP_ins2_EU <- list()
RP_ins2_EU_Q1 <- list()
RP_ins2_EU_Q2 <- list()
RP_ins2_EU_Q3 <- list()
RP_ins2_EU_Q4 <- list()
RP_noins_EU <- list()
RP_noins_EU_Q1 <- list()
RP_noins_EU_Q2 <- list()
RP_noins_EU_Q3 <- list()
RP_noins_EU_Q4 <- list()

for (m in 1:length(cdd)) {

# Contract 1 : Interpolated indices

coverage<-1

coe.contract1 <- matrix(nrow=2, ncol=ncol(yield))

for (i in 1:ncol(yield)){
  temp.contract1<-rq(yield_detrended[,i]~cdd[[m]][,i], tau=0.3) # To try different regression quantiles, change the value of tau
  coe.contract1[,i]<-temp.contract1$coef
}

summary(coe.contract1[2,])

# strike level

str.contract1<-vector(length=ncol(yield))

for (i in 1:ncol(yield)){
  str.contract1[i]<-(coverage*mean(yield_detrended[,i])-coe.contract1[1,i])/coe.contract1[2,i]
}

# payout

pay.contract1[[m]]<-matrix(nrow=nrow(yield), ncol=ncol(yield))

for (i in 1:ncol(yield)){
  for (k in 1:nrow(yield)){
    
    if (coe.contract1[2,i]<0){
      pay.contract1[[m]][k,i]<-abs(coe.contract1[2,i])*max((cdd[[m]][k,i]-str.contract1[i]),0)}
  }
}


# contract 2 - index based on nearest neighbor station

coe.contract2 <- matrix(nrow=2, ncol=ncol(yield))
marginal_effect<-vector(length=ncol(yield))

for (i in 1:ncol(yield)){
  temp.contract2<-rq(yield_detrended[,i]~cdd_nn[,i], tau = 0.3)
  coe.contract2[,i]<-temp.contract2$coef
}

summary(coe.contract1[2,])
summary(coe.contract2[2,])
summary(marginal_effect)

cbind(coe.contract1[2,],coe.contract2[2,], marginal_effect)

# strike level - unique for each year due to kriging variance
# when I run the code there are no differences in across years

str.contract2<-vector(length=ncol(yield))

for (i in 1:ncol(yield)){
  
  str.contract2[i]<-(coverage*mean(yield_detrended[,i])-coe.contract2[1,i])/coe.contract2[2,i]
  
}

summary(str.contract1)
summary(str.contract2)

# payout

pay.contract2[[m]]<-matrix(nrow=nrow(yield), ncol=ncol(yield))

for (i in 1:ncol(yield)){
  for (k in 1:nrow(yield)){
    
    if (coe.contract2[2,i]<0){
      pay.contract2[[m]][k,i]<-abs(coe.contract2[2,i])*max((cdd_nn[k,i]-str.contract2[i]),0)}
  }
}

# premium pricing included here: 

set.seed(33)# always execute together with the below loop so that the sample() always draws the same random payouts; enables replicating the results

boots.contract1<-matrix(,ncol=ncol(yield_detrended),nrow=10000)
boots.contract2<-matrix(,ncol=ncol(yield_detrended),nrow=10000)

for (i in 1:ncol(yield_detrended)){
  for (k in 1:nrow(yield_detrended)){
    if (length(which(is.na(pay.contract1[[m]][,i])))!=nrow(yield_detrended)){
      boots.contract1[,i]<-sample(pay.contract1[[m]][!is.na(pay.contract1[[m]][,i]),i],10000, replace = T)}
    if (length(which(is.na(pay.contract2[[m]][,i])))!=nrow(yield_detrended)){
      boots.contract2[,i]<-sample(pay.contract2[[m]][!is.na(pay.contract2[[m]][,i]),i],10000, replace=T)}
  }
  
}

### Column means give insurance premium

premium.contract1<-as.vector(colMeans(boots.contract1))
premium.contract2<-as.vector(colMeans(boots.contract2))

summary(premium.contract1)
summary(premium.contract2)

# alternatively we set the premium equal to the average observed payoff

# premium.contract1<-colMeans(pay.contract1[[m]])
# premium.contract2<-colMeans(pay.contract2[[m]])

cbind(premium.contract1,premium.contract2)

################################################################################################
#####################################  EU using power Utility function #########################
################################################################################################
# we calculate the Utility of the k-th year of the i-th farm and then average over the years 


coe.ris<-c(0,1,2,3,4,5) # risk coefficients for expected utility

# uninsured revenue per farm

rev            <-  yield_detrended

# revenue after insuring via contract1

rev.ins.contract1<-  matrix(, nrow = nrow(yield_detrended), ncol = ncol(yield_detrended))

# revenue after insuring via contract2

rev.ins.contract2<-  matrix(, nrow = nrow(yield_detrended), ncol = ncol(yield_detrended))


for (i in 1:ncol(yield_detrended)){
  for(j in 1:nrow(yield_detrended)){
    
    if(!is.na(premium.contract1[i])){
      rev.ins.contract1[j,i]<- rev[j,i] - premium.contract1[i] + pay.contract1[[m]][j,i]
    }else{
      rev.ins.contract1[j,i]<- rev[j,i]
    }
    
  }
}


for (i in 1:ncol(yield_detrended)){
  for(j in 1:nrow(yield_detrended)){
    
    if(!is.na(premium.contract1[i])){
      rev.ins.contract2[j,i]<- rev[j,i] - premium.contract2[i] + pay.contract2[[m]][j,i]
    }else{
      rev.ins.contract2[j,i]<- rev[j,i]
    }
    
  }
}

# Expected Utility
#we assume different levels of relative risk aversion according to above "coe.ris" levels 

# yearly utilities for uninsured case
uti.rev              <-  array(dim=c(nrow(rev),ncol(rev),length(coe.ris)))

# yearly utility for different insured cases
uti.rev.contract1    <-  array(dim=c(nrow(rev),ncol(rev),length(coe.ris)))

# yearly utility for different insured cases
uti.rev.contract2    <-  array(dim=c(nrow(rev),ncol(rev),length(coe.ris)))


# expected utility of uninsured case
eut.rev[[m]]              <-  matrix(,ncol=ncol(rev),nrow=length(coe.ris))

# expected utility of insured cases

eut.rev.contract1[[m]]          <-  matrix(,ncol=ncol(rev),nrow=length(coe.ris))

# expected utility of insured cases

eut.rev.contract2[[m]]      <-  matrix(,ncol=ncol(rev),nrow=length(coe.ris))


for (i in 1:ncol(rev)){
  for (k in 1:nrow(rev)){
    for (j in 1:length(coe.ris)){
      if (coe.ris[j]!=1){
        uti.rev[k,i,j]              <-  ((rev[k,i])^(1-coe.ris[j]))/(1-coe.ris[j])
        
        uti.rev.contract1[k,i,j]    <-  ((rev.ins.contract1[k,i])^(1-coe.ris[j]))/(1-coe.ris[j])
        
        uti.rev.contract2[k,i,j]    <-  ((rev.ins.contract2[k,i])^(1-coe.ris[j]))/(1-coe.ris[j])
        
      } else {
        uti.rev[k,i,j]              <-  log(rev[k,i])
        
        uti.rev.contract1[k,i,j]    <-  log(rev.ins.contract1[k,i])
        
        uti.rev.contract2[k,i,j]    <-  log(rev.ins.contract2[k,i])
        
      }
      
      eut.rev[[m]][j,i]                  <-  mean(uti.rev[,i,j])
      
      eut.rev.contract1[[m]][j,i]       <-  mean(uti.rev.contract1[,i,j])
      
      eut.rev.contract2[[m]][j,i]       <-  mean(uti.rev.contract2[,i,j])
      
    }
  }
}

# now we test for statistical differences
wil.tes.contract1_vs_noi[[m]]<-vector(length=6)

for (j in 1:length(coe.ris)){
  wil.tes.contract1_vs_noi[[m]][j]<-wilcox.test(as.numeric(eut.rev.contract1[[m]][j,]), as.numeric(eut.rev[[m]][j,]), alternative="g", paired=T,na.action=na.omit)$p.value
}

wil.tes.contract2_vs_noi[[m]]<-vector(length=6)

for (j in 1:length(coe.ris)){
  wil.tes.contract2_vs_noi[[m]][j]<-wilcox.test(as.numeric(eut.rev.contract2[[m]][j,]), as.numeric(eut.rev[[m]][j,]), alternative="g", paired=T,na.action=na.omit)$p.value
}

wil.tes.contract2_vs_contract1[[m]]<-vector(length=6)

for (j in 1:length(coe.ris)){
  wil.tes.contract2_vs_contract1[[m]][j]<-wilcox.test(as.numeric(eut.rev.contract1[[m]][j,]), as.numeric(eut.rev.contract2[[m]][j,]), alternative="g", paired=T,na.action=na.omit)$p.value
}


wil.tes.contract1_vs_noi[[m]]
wil.tes.contract2_vs_noi[[m]]
wil.tes.contract2_vs_contract1[[m]]

# Quantile risk premiums: For the theoretical basis of QRP, see Chapter 2, and Kim et al. 2014

phi<-0.5


# no insurance
RP_noins_EU_Q1[[m]]<-vector(,length=ncol(rev))
RP_noins_EU_Q2[[m]]<-vector(,length=ncol(rev))
RP_noins_EU_Q3[[m]]<-vector(,length=ncol(rev))
RP_noins_EU_Q4[[m]]<-vector(,length=ncol(rev))
RP_noins_EU[[m]]<-vector(,length=ncol(rev))

for (i in 1:ncol(rev)){
  # data in quantiles
  daten_Q1<-rev[intersect(which(rev[,i] >=(quantile(rev[,i],na.rm=T)[1])) , which(rev[,i] <= quantile(rev[,i],na.rm=T)[2])),i]
  daten_Q2<-rev[intersect(which(rev[,i] > (quantile(rev[,i],na.rm=T)[2])) , which(rev[,i] <= quantile(rev[,i],na.rm=T)[3])),i]
  daten_Q3<-rev[intersect(which(rev[,i] > (quantile(rev[,i],na.rm=T)[3])) , which(rev[,i] <= quantile(rev[,i],na.rm=T)[4])),i]
  daten_Q4<-rev[intersect(which(rev[,i] > (quantile(rev[,i],na.rm=T)[4])) , which(rev[,i] <= quantile(rev[,i],na.rm=T)[5])),i]
  
  
  # Q1
  RP_noins_EU_Q1[[m]][i]<-(1/2)*(0.25-0)*
    ((phi*mean(daten_Q1)^(-phi-1))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *var(daten_Q1)+(phi/mean(rev[,i],na.rm = T))*(mean(daten_Q1)-mean(rev[,i],na.rm = T))^2)+
    (1/6)*(0.25-0)*
    (((phi^2+phi)*mean(daten_Q1)^(-phi-2))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *skewness(daten_Q1,na.rm = T)-((phi^2+phi)/mean(rev[,i],na.rm = T)^2)*(mean(daten_Q1)-mean(rev[,i],na.rm = T))^2)
  
  
  # Q2
  RP_noins_EU_Q2[[m]][i]<-(1/2)*(0.5-0.25)*
    ((phi*mean(daten_Q2)^(-phi-1))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *var(daten_Q2)+(phi/mean(rev[,i],na.rm = T))*(mean(daten_Q2)-mean(rev[,i],na.rm = T))^2)+
    (1/6)*(0.5-0.25)*
    (((phi^2+phi)*mean(daten_Q2)^(-phi-2))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *skewness(daten_Q2,na.rm = T)-((phi^2+phi)/mean(rev[,i],na.rm = T)^2)*(mean(daten_Q2)-mean(rev[,i],na.rm = T))^2)
  
  
  # Q3
  RP_noins_EU_Q3[[m]][i]<-(1/2)*(0.75-0.5)*
    ((phi*mean(daten_Q3)^(-phi-1))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *var(daten_Q3)+(phi/mean(rev[,i],na.rm = T))*(mean(daten_Q3)-mean(rev[,i],na.rm = T))^2)+
    (1/6)*(0.75-0.5)*
    (((phi^2+phi)*mean(daten_Q3)^(-phi-2))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *skewness(daten_Q3,na.rm = T)-((phi^2+phi)/mean(rev[,i],na.rm = T)^2)*(mean(daten_Q3)-mean(rev[,i],na.rm = T))^2)
  
  # Q4
  RP_noins_EU_Q4[[m]][i]<-(1/2)*(1-0.75)*
    ((phi*mean(daten_Q4)^(-phi-1))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *var(daten_Q4)+(phi/mean(rev[,i],na.rm = T))*(mean(daten_Q4)-mean(rev[,i],na.rm = T))^2)+
    (1/6)*(1-0.75)*
    (((phi^2+phi)*mean(daten_Q4)^(-phi-2))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *skewness(daten_Q4,na.rm = T)-((phi^2+phi)/mean(rev[,i],na.rm = T)^2)*(mean(daten_Q4)-mean(rev[,i],na.rm = T))^2)
  
  # RP overall 
  
  RP_noins_EU[[m]][i]<-(1/2)*(phi/mean(rev[,i],na.rm = T))*var(rev[,i],na.rm = T)+(1/6)*(-(phi^2+phi)/mean(rev[,i],na.rm = T)^2)*skewness(rev[,i],na.rm=T)
  
  remove(daten_Q1)
  remove(daten_Q2)
  remove(daten_Q3)
  remove(daten_Q4)
}



# Contract 1: Ordinary kriging (OK) index insurance
RP_ins1_EU_Q1[[m]]<-vector(,length=ncol(rev.ins.contract1))
RP_ins1_EU_Q2[[m]]<-vector(,length=ncol(rev.ins.contract1))
RP_ins1_EU_Q3[[m]]<-vector(,length=ncol(rev.ins.contract1))
RP_ins1_EU_Q4[[m]]<-vector(,length=ncol(rev.ins.contract1))
RP_ins1_EU[[m]]<-vector(,length=ncol(rev.ins.contract1))

for (i in 1:ncol(rev.ins.contract1)){
  # data in quantiles
  daten_Q1<-rev.ins.contract1[intersect(which(rev.ins.contract1[,i] >=(quantile(rev.ins.contract1[,i],na.rm=T)[1])) , which(rev.ins.contract1[,i] <= quantile(rev.ins.contract1[,i],na.rm=T)[2])),i]
  daten_Q2<-rev.ins.contract1[intersect(which(rev.ins.contract1[,i] > (quantile(rev.ins.contract1[,i],na.rm=T)[2])) , which(rev.ins.contract1[,i] <= quantile(rev.ins.contract1[,i],na.rm=T)[3])),i]
  daten_Q3<-rev.ins.contract1[intersect(which(rev.ins.contract1[,i] > (quantile(rev.ins.contract1[,i],na.rm=T)[3])) , which(rev.ins.contract1[,i] <= quantile(rev.ins.contract1[,i],na.rm=T)[4])),i]
  daten_Q4<-rev.ins.contract1[intersect(which(rev.ins.contract1[,i] > (quantile(rev.ins.contract1[,i],na.rm=T)[4])) , which(rev.ins.contract1[,i] <= quantile(rev.ins.contract1[,i],na.rm=T)[5])),i]
  
  
  # Q1
  RP_ins1_EU_Q1[[m]][i]<-(1/2)*(0.25-0)*
    ((phi*mean(daten_Q1)^(-phi-1))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *var(daten_Q1)+(phi/mean(rev.ins.contract1[,i],na.rm = T))*(mean(daten_Q1)-mean(rev.ins.contract1[,i],na.rm = T))^2)+
    (1/6)*(0.25-0)*
    (((phi^2+phi)*mean(daten_Q1)^(-phi-2))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *skewness(daten_Q1,na.rm = T)-((phi^2+phi)/mean(rev.ins.contract1[,i],na.rm = T)^2)*(mean(daten_Q1)-mean(rev.ins.contract1[,i],na.rm = T))^2)
  
  
  # Q2
  RP_ins1_EU_Q2[[m]][i]<-(1/2)*(0.5-0.25)*
    ((phi*mean(daten_Q2)^(-phi-1))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *var(daten_Q2)+(phi/mean(rev.ins.contract1[,i],na.rm = T))*(mean(daten_Q2)-mean(rev.ins.contract1[,i],na.rm = T))^2)+
    (1/6)*(0.5-0.25)*
    (((phi^2+phi)*mean(daten_Q2)^(-phi-2))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *skewness(daten_Q2,na.rm = T)-((phi^2+phi)/mean(rev.ins.contract1[,i],na.rm = T)^2)*(mean(daten_Q2)-mean(rev.ins.contract1[,i],na.rm = T))^2)
  
  
  # Q3
  RP_ins1_EU_Q3[[m]][i]<-(1/2)*(0.75-0.5)*
    ((phi*mean(daten_Q3)^(-phi-1))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *var(daten_Q3)+(phi/mean(rev.ins.contract1[,i],na.rm = T))*(mean(daten_Q3)-mean(rev.ins.contract1[,i],na.rm = T))^2)+
    (1/6)*(0.75-0.5)*
    (((phi^2+phi)*mean(daten_Q3)^(-phi-2))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *skewness(daten_Q3,na.rm = T)-((phi^2+phi)/mean(rev.ins.contract1[,i],na.rm = T)^2)*(mean(daten_Q3)-mean(rev.ins.contract1[,i],na.rm = T))^2)
  
  # Q4
  RP_ins1_EU_Q4[[m]][i]<-(1/2)*(1-0.75)*
    ((phi*mean(daten_Q4)^(-phi-1))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *var(daten_Q4)+(phi/mean(rev.ins.contract1[,i],na.rm = T))*(mean(daten_Q4)-mean(rev.ins.contract1[,i],na.rm = T))^2)+
    (1/6)*(1-0.75)*
    (((phi^2+phi)*mean(daten_Q4)^(-phi-2))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *skewness(daten_Q4,na.rm = T)-((phi^2+phi)/mean(rev.ins.contract1[,i],na.rm = T)^2)*(mean(daten_Q4)-mean(rev.ins.contract1[,i],na.rm = T))^2)
  
  # RP overall 
  
  RP_ins1_EU[[m]][i]<-(1/2)*(phi/mean(rev.ins.contract1[,i],na.rm = T))*var(rev.ins.contract1[,i],na.rm = T)+(1/6)*(-(phi^2+phi)/mean(rev.ins.contract1[,i],na.rm = T)^2)*skewness(rev.ins.contract1[,i],na.rm=T)
  remove(daten_Q1)
  remove(daten_Q2)
  remove(daten_Q3)
  remove(daten_Q4)
}




# Contract 2: Nearest-Neighbor index insurance contract
RP_ins2_EU_Q1[[m]]<-vector(,length=ncol(rev.ins.contract2))
RP_ins2_EU_Q2[[m]]<-vector(,length=ncol(rev.ins.contract2))
RP_ins2_EU_Q3[[m]]<-vector(,length=ncol(rev.ins.contract2))
RP_ins2_EU_Q4[[m]]<-vector(,length=ncol(rev.ins.contract2))
RP_ins2_EU[[m]]<-vector(,length=ncol(rev.ins.contract2))

for (i in 1:ncol(rev.ins.contract2)){
  # data in quantiles
  daten_Q1<-rev.ins.contract2[intersect(which(rev.ins.contract2[,i] >=(quantile(rev.ins.contract2[,i],na.rm=T)[1])) , which(rev.ins.contract2[,i] <= quantile(rev.ins.contract2[,i],na.rm=T)[2])),i]
  daten_Q2<-rev.ins.contract2[intersect(which(rev.ins.contract2[,i] > (quantile(rev.ins.contract2[,i],na.rm=T)[2])) , which(rev.ins.contract2[,i] <= quantile(rev.ins.contract2[,i],na.rm=T)[3])),i]
  daten_Q3<-rev.ins.contract2[intersect(which(rev.ins.contract2[,i] > (quantile(rev.ins.contract2[,i],na.rm=T)[3])) , which(rev.ins.contract2[,i] <= quantile(rev.ins.contract2[,i],na.rm=T)[4])),i]
  daten_Q4<-rev.ins.contract2[intersect(which(rev.ins.contract2[,i] > (quantile(rev.ins.contract2[,i],na.rm=T)[4])) , which(rev.ins.contract2[,i] <= quantile(rev.ins.contract2[,i],na.rm=T)[5])),i]
  
  
  # Q1
  RP_ins2_EU_Q1[[m]][i]<-(1/2)*(0.25-0)*
    ((phi*mean(daten_Q1)^(-phi-1))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *var(daten_Q1)+(phi/mean(rev.ins.contract2[,i],na.rm = T))*(mean(daten_Q1)-mean(rev.ins.contract2[,i],na.rm = T))^2)+
    (1/6)*(0.25-0)*
    (((phi^2+phi)*mean(daten_Q1)^(-phi-2))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *skewness(daten_Q1,na.rm = T)-((phi^2+phi)/mean(rev.ins.contract2[,i],na.rm = T)^2)*(mean(daten_Q1)-mean(rev.ins.contract2[,i],na.rm = T))^2)
  
  
  # Q2
  RP_ins2_EU_Q2[[m]][i]<-(1/2)*(0.5-0.25)*
    ((phi*mean(daten_Q2)^(-phi-1))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *var(daten_Q2)+(phi/mean(rev.ins.contract2[,i],na.rm = T))*(mean(daten_Q2)-mean(rev.ins.contract2[,i],na.rm = T))^2)+
    (1/6)*(0.5-0.25)*
    (((phi^2+phi)*mean(daten_Q2)^(-phi-2))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *skewness(daten_Q2,na.rm = T)-((phi^2+phi)/mean(rev.ins.contract2[,i],na.rm = T)^2)*(mean(daten_Q2)-mean(rev.ins.contract2[,i],na.rm = T))^2)
  
  
  # Q3
  RP_ins2_EU_Q3[[m]][i]<-(1/2)*(0.75-0.5)*
    ((phi*mean(daten_Q3)^(-phi-1))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *var(daten_Q3)+(phi/mean(rev.ins.contract2[,i],na.rm = T))*(mean(daten_Q3)-mean(rev.ins.contract2[,i],na.rm = T))^2)+
    (1/6)*(0.75-0.5)*
    (((phi^2+phi)*mean(daten_Q3)^(-phi-2))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *skewness(daten_Q3,na.rm = T)-((phi^2+phi)/mean(rev.ins.contract2[,i],na.rm = T)^2)*(mean(daten_Q3)-mean(rev.ins.contract2[,i],na.rm = T))^2)
  
  # Q4
  RP_ins2_EU_Q4[[m]][i]<-(1/2)*(1-0.75)*
    ((phi*mean(daten_Q4)^(-phi-1))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *var(daten_Q4)+(phi/mean(rev.ins.contract2[,i],na.rm = T))*(mean(daten_Q4)-mean(rev.ins.contract2[,i],na.rm = T))^2)+
    (1/6)*(1-0.75)*
    (((phi^2+phi)*mean(daten_Q4)^(-phi-2))/
       (((0.25-0)*mean(daten_Q1)^(-phi))+((0.5-0.25)*mean(daten_Q2)^(-phi))+((0.75-0.5)*mean(daten_Q3)^(-phi))+((1-0.75)*mean(daten_Q4)^(-phi)))
     *skewness(daten_Q4,na.rm = T)-((phi^2+phi)/mean(rev.ins.contract2[,i],na.rm = T)^2)*(mean(daten_Q4)-mean(rev.ins.contract2[,i],na.rm = T))^2)
  
  # RP overall 
  
  RP_ins2_EU[[m]][i]<-(1/2)*(phi/mean(rev.ins.contract2[,i],na.rm = T))*var(rev.ins.contract2[,i],na.rm = T)+(1/6)*(-(phi^2+phi)/mean(rev.ins.contract2[,i],na.rm = T)^2)*skewness(rev.ins.contract2[,i],na.rm=T)
  remove(daten_Q1)
  remove(daten_Q2)
  remove(daten_Q3)
  remove(daten_Q4)
}


#  Statistical test interpolations vs NN

print(paste("Statistical test for Interpolation Method: ", m))
print(wilcox.test(RP_ins1_EU_Q1[[m]],RP_ins2_EU_Q1[[m]],alternative="l",paired=T))
print(wilcox.test(RP_ins1_EU_Q2[[m]],RP_ins2_EU_Q2[[m]],alternative="l",paired=T))
print(wilcox.test(RP_ins1_EU_Q3[[m]],RP_ins2_EU_Q3[[m]],alternative="l",paired=T))
print(wilcox.test(RP_ins1_EU_Q4[[m]],RP_ins2_EU_Q4[[m]],alternative="l",paired=T))
print(wilcox.test(RP_ins1_EU[[m]],RP_ins2_EU[[m]],alternative="l",paired=T))

}