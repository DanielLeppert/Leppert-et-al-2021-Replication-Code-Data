# Calculate relative changes in simulated risk premia from switching between insurance contracts

EUDIFF100 <- array(dim=c(131,3*length(coe.ris),3))
EUDIFF90 <- array(dim=c(131,3*length(coe.ris),3))
EUDIFF80 <- array(dim=c(131,3*length(coe.ris),3))
EUDIFF70 <- array(dim=c(131,3*length(coe.ris),3))
EUDIFF60 <- array(dim=c(131,3*length(coe.ris),3))
EUDIFF50 <- array(dim=c(131,3*length(coe.ris),3))


for (i in 0:2) {

EUDIFF100[ , i*6 + 1:6, 1] <- t(((eut.rev[[1+i]] - eut.rev.contract2[[1+i]])/eut.rev[[1+i]])*100)
EUDIFF100[ , i*6 + 1:6, 2] <- t(((eut.rev[[1+i]] - eut.rev.contract1[[1+i]])/eut.rev[[1+i]])*100)
EUDIFF100[ , i*6 + 1:6, 3] <- t(((eut.rev.contract2[[1+i]] - eut.rev.contract1[[1+i]])/eut.rev.contract2[[1+i]])*100)

}

for (i in 0:2) {
  
  EUDIFF90[ , i*6 + 1:6, 1] <- t(((eut.rev[[1+i]] - eut.rev.contract2[[1+i]])/eut.rev[[1+i]])*100)
  EUDIFF90[ , i*6 + 1:6, 2] <- t(((eut.rev[[1+i]] - eut.rev.contract1[[1+i]])/eut.rev[[1+i]])*100)
  EUDIFF90[ , i*6 + 1:6, 3] <- t(((eut.rev.contract2[[1+i]] - eut.rev.contract1[[1+i]])/eut.rev.contract2[[1+i]])*100)
  
}

for (i in 0:2) {
  
  EUDIFF80[ , i*6 + 1:6, 1] <- t(((eut.rev[[1+i]] - eut.rev.contract2[[1+i]])/eut.rev[[1+i]])*100)
  EUDIFF80[ , i*6 + 1:6, 2] <- t(((eut.rev[[1+i]] - eut.rev.contract1[[1+i]])/eut.rev[[1+i]])*100)
  EUDIFF80[ , i*6 + 1:6, 3] <- t(((eut.rev.contract2[[1+i]] - eut.rev.contract1[[1+i]])/eut.rev.contract2[[1+i]])*100)
  
}

for (i in 0:2) {
  
  EUDIFF70[ , i*6 + 1:6, 1] <- t(((eut.rev[[1+i]] - eut.rev.contract2[[1+i]])/eut.rev[[1+i]])*100)
  EUDIFF70[ , i*6 + 1:6, 2] <- t(((eut.rev[[1+i]] - eut.rev.contract1[[1+i]])/eut.rev[[1+i]])*100)
  EUDIFF70[ , i*6 + 1:6, 3] <- t(((eut.rev.contract2[[1+i]] - eut.rev.contract1[[1+i]])/eut.rev.contract2[[1+i]])*100)
  
}

for (i in 0:2) {
  
  EUDIFF60[ , i*6 + 1:6, 1] <- t(((eut.rev[[1+i]] - eut.rev.contract2[[1+i]])/eut.rev[[1+i]])*100)
  EUDIFF60[ , i*6 + 1:6, 2] <- t(((eut.rev[[1+i]] - eut.rev.contract1[[1+i]])/eut.rev[[1+i]])*100)
  EUDIFF60[ , i*6 + 1:6, 3] <- t(((eut.rev.contract2[[1+i]] - eut.rev.contract1[[1+i]])/eut.rev.contract2[[1+i]])*100)
  
}

for (i in 0:2) {
  
  EUDIFF50[ , i*6 + 1:6, 1] <- t(((eut.rev[[1+i]] - eut.rev.contract2[[1+i]])/eut.rev[[1+i]])*100)
  EUDIFF50[ , i*6 + 1:6, 2] <- t(((eut.rev[[1+i]] - eut.rev.contract1[[1+i]])/eut.rev[[1+i]])*100)
  EUDIFF50[ , i*6 + 1:6, 3] <- t(((eut.rev.contract2[[1+i]] - eut.rev.contract1[[1+i]])/eut.rev.contract2[[1+i]])*100)
  
}


EUDIFF <- data.frame(
  'alpha' = rep(c(0:5), 7),
  'change' = c(colMeans(EUDIFF100, na.rm = T)[1:6, 1], 
               colMeans(EUDIFF100, na.rm = T)[, 2],
               colMeans(EUDIFF100, na.rm = T)[, 3]),
  'method' = c(rep('NN', 6), rep(c('IDW','IDW','IDW','IDW','IDW','IDW',
                   'OK','OK','OK','OK','OK','OK',
                   'RK','RK','RK','RK','RK','RK'), 2 ))
)


class(EUDIFF$alpha) <- 'numeric'
class(EUDIFF$change) <- 'numeric'


RPDIFF <- data.frame(
  'NAME' = rep(c(mindist$NAME), 4),
  'dist' = rep(c(mindist$MINDIST), 4),
  'method' = c(rep('NN', 131),rep('IDW', 131),rep('OK', 131),rep('RK', 131)),
  'Q1' = c(((RP_ins2_EU_Q1[[1]] - RP_noins_EU_Q1[[1]])/RP_noins_EU_Q1[[1]])*100,
           ((RP_ins1_EU_Q1[[1]] - RP_noins_EU_Q1[[1]])/RP_noins_EU_Q1[[1]])*100,
           ((RP_ins1_EU_Q1[[2]] - RP_noins_EU_Q1[[2]])/RP_noins_EU_Q1[[2]])*100,
           ((RP_ins1_EU_Q1[[3]] - RP_noins_EU_Q1[[3]])/RP_noins_EU_Q1[[3]])*100),
  'Q2' = c(((RP_ins2_EU_Q2[[1]] - RP_noins_EU_Q2[[1]])/RP_noins_EU_Q2[[1]])*100,
           ((RP_ins1_EU_Q2[[1]] - RP_noins_EU_Q2[[1]])/RP_noins_EU_Q2[[1]])*100,
           ((RP_ins1_EU_Q2[[2]] - RP_noins_EU_Q2[[2]])/RP_noins_EU_Q2[[2]])*100,
           ((RP_ins1_EU_Q2[[3]] - RP_noins_EU_Q2[[3]])/RP_noins_EU_Q2[[3]])*100),
  'Q3' = c(((RP_ins2_EU_Q3[[1]] - RP_noins_EU_Q3[[1]])/RP_noins_EU_Q3[[1]])*100,
           ((RP_ins1_EU_Q3[[1]] - RP_noins_EU_Q3[[1]])/RP_noins_EU_Q3[[1]])*100,
           ((RP_ins1_EU_Q3[[2]] - RP_noins_EU_Q3[[2]])/RP_noins_EU_Q3[[2]])*100,
           ((RP_ins1_EU_Q3[[3]] - RP_noins_EU_Q3[[3]])/RP_noins_EU_Q3[[3]])*100),
  'Q4' = c(((RP_ins2_EU_Q4[[1]] - RP_noins_EU_Q4[[1]])/RP_noins_EU_Q4[[1]])*100,
           ((RP_ins1_EU_Q4[[1]] - RP_noins_EU_Q4[[1]])/RP_noins_EU_Q4[[1]])*100,
           ((RP_ins1_EU_Q4[[2]] - RP_noins_EU_Q4[[2]])/RP_noins_EU_Q4[[2]])*100,
           ((RP_ins1_EU_Q4[[3]] - RP_noins_EU_Q4[[3]])/RP_noins_EU_Q4[[3]])*100),
  'full' = c(((RP_ins2_EU[[1]] - RP_noins_EU[[1]])/RP_noins_EU[[1]])*100,
           ((RP_ins1_EU[[1]] - RP_noins_EU[[1]])/RP_noins_EU[[1]])*100,
           ((RP_ins1_EU[[2]] - RP_noins_EU[[2]])/RP_noins_EU[[2]])*100,
           ((RP_ins1_EU[[3]] - RP_noins_EU[[3]])/RP_noins_EU[[3]])*100)
  
)

class(RPDIFF$dist) <- 'numeric'

RPDIFF2 <- data.frame('NAME' = rep(c(mindist$NAME), 3),
                      'dist2' = rep(c(mindist$MINDIST), 3),
                      'method2' = c(rep('IDW', 131),rep('OK', 131),rep('RK', 131)),
                      'Q1two' = c(((RP_ins1_EU_Q1[[1]] - RP_ins2_EU_Q1[[1]])/RP_ins2_EU_Q1[[1]])*100,
                              ((RP_ins1_EU_Q1[[2]] - RP_ins2_EU_Q1[[2]])/RP_ins2_EU_Q1[[2]])*100,
                              ((RP_ins1_EU_Q1[[3]] - RP_ins2_EU_Q1[[3]])/RP_ins2_EU_Q1[[3]])*100),
                      'Q2two' = c(((RP_ins1_EU_Q2[[1]] - RP_ins2_EU_Q2[[1]])/RP_ins2_EU_Q2[[1]])*100,
                               ((RP_ins1_EU_Q2[[2]] - RP_ins2_EU_Q2[[2]])/RP_ins2_EU_Q2[[2]])*100,
                               ((RP_ins1_EU_Q2[[3]] - RP_ins2_EU_Q2[[3]])/RP_ins2_EU_Q2[[3]])*100),
                      'Q3two' = c(((RP_ins1_EU_Q3[[1]] - RP_ins2_EU_Q3[[1]])/RP_ins2_EU_Q3[[1]])*100,
                               ((RP_ins1_EU_Q3[[2]] - RP_ins2_EU_Q3[[2]])/RP_ins2_EU_Q3[[2]])*100,
                               ((RP_ins1_EU_Q3[[3]] - RP_ins2_EU_Q3[[3]])/RP_ins2_EU_Q3[[3]])*100),
                      'Q4two' = c(((RP_ins1_EU_Q4[[1]] - RP_ins2_EU_Q4[[1]])/RP_ins2_EU_Q4[[1]])*100,
                               ((RP_ins1_EU_Q4[[2]] - RP_ins2_EU_Q4[[2]])/RP_ins2_EU_Q4[[2]])*100,
                               ((RP_ins1_EU_Q4[[3]] - RP_ins2_EU_Q4[[3]])/RP_ins2_EU_Q4[[3]])*100),
                      
                      'fulltwo' = c(((RP_ins1_EU[[1]] - RP_ins2_EU[[1]])/RP_ins2_EU[[1]])*100,
                               ((RP_ins1_EU[[2]] - RP_ins2_EU[[2]])/RP_ins2_EU[[2]])*100,
                               ((RP_ins1_EU[[3]] - RP_ins2_EU[[3]])/RP_ins2_EU[[3]])*100)
                      
)

class(RPDIFF2$dist2) <- 'numeric'



# TEST RP CHANGES FOR DIFFERENT LEVELS OF TAU:

print(mean(na.omit(((RP_ins1_EU[[1]] - RP_ins2_EU[[1]])/RP_ins2_EU[[1]])*100)))
print(mean(na.omit(((RP_ins1_EU[[2]] - RP_ins2_EU[[2]])/RP_ins2_EU[[2]])*100)))
print(mean(na.omit(((RP_ins1_EU[[3]] - RP_ins2_EU[[3]])/RP_ins2_EU[[3]])*100)))

print(mean(((RP_ins1_EU[[1]] - RP_noins_EU[[1]])/RP_noins_EU[[1]])*100))
print(mean(((RP_ins1_EU[[2]] - RP_noins_EU[[2]])/RP_noins_EU[[2]])*100))
print(mean(((RP_ins1_EU[[3]] - RP_noins_EU[[3]])/RP_noins_EU[[3]])*100))
print(mean(na.omit(((RP_ins2_EU[[1]] - RP_noins_EU[[1]])/RP_noins_EU[[1]])*100)))