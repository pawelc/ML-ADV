library(rjags)
library(coda)

mod.string <- "model {

  ################## Aposestoriori probabilities for alleles
  alleleAprProb[1] <- 0.28 # A
  alleleAprProb[2] <- 0.06 # B
  alleleAprProb[3] <- 0.66 # O

  ################## Allele + Allele = blod group, maps 2 alleles categories to probability of given bload group
  # A + A = A
  allelesToBlood[1,1,1] <- 1
  allelesToBlood[1,1,2] <- 0
  allelesToBlood[1,1,3] <- 0
  allelesToBlood[1,1,4] <- 0
  # B + B = B
  allelesToBlood[2,2,1] <- 0
  allelesToBlood[2,2,2] <- 1
  allelesToBlood[2,2,3] <- 0
  allelesToBlood[2,2,4] <- 0
  # A + B = AB
  allelesToBlood[1,2,1] <- 0
  allelesToBlood[1,2,2] <- 0
  allelesToBlood[1,2,3] <- 1
  allelesToBlood[1,2,4] <- 0
  # B + A = AB
  allelesToBlood[2,1,1] <- 0
  allelesToBlood[2,1,2] <- 0
  allelesToBlood[2,1,3] <- 1
  allelesToBlood[2,1,4] <- 0
  # A + O = A
  allelesToBlood[1,3,1] <- 1
  allelesToBlood[1,3,2] <- 0
  allelesToBlood[1,3,3] <- 0
  allelesToBlood[1,3,4] <- 0
  # O + A = A
  allelesToBlood[3,1,1] <- 1
  allelesToBlood[3,1,2] <- 0
  allelesToBlood[3,1,3] <- 0
  allelesToBlood[3,1,4] <- 0
  # B + O = B
  allelesToBlood[2,3,1] <- 0
  allelesToBlood[2,3,2] <- 1
  allelesToBlood[2,3,3] <- 0
  allelesToBlood[2,3,4] <- 0
  # O + B = B
  allelesToBlood[3,2,1] <- 0
  allelesToBlood[3,2,2] <- 1
  allelesToBlood[3,2,3] <- 0
  allelesToBlood[3,2,4] <- 0
  # O + O = O
  allelesToBlood[3,3,1] <- 0
  allelesToBlood[3,3,2] <- 0
  allelesToBlood[3,3,3] <- 0
  allelesToBlood[3,3,4] <- 1

  #dychotomous var
  dych[1] <- 1
  dych[2] <- 1

  #vector of unormalized mass for a population in a generation so I choose parent
  for(p in 1:POP_NUM) {
    popInGen[p] <- 1

    #this is dummy variable not used so I can recursively refer to parent allele
    personAllele[1,p,1] <- 1
    personAllele[1,p,2] <- 1
  }
 
  #create GEN_NUM generations, 1 is the oldest and GEN_NUM is the yongest
  for(g in 2:GEN_NUM+1){

    #create population in generation g asuming that there is POP_NUM in each generation
    for(p in 1:POP_NUM) {
      #parents from the previous generation, used for generations younger than oldest
      parent[g,p,1] ~  dcat(popInGen)
      parent[g,p,2] ~  dcat(popInGen)

      chooseParentAllele[g,p,1] ~ dcat(dych)
      chooseParentAllele[g,p,2] ~ dcat(dych)
  
      personAlleleFromParent[g,p,1] <- personAllele[g-1,parent[g,p,1], chooseParentAllele[g,p,1]]
      personAlleleFromParent[g,p,2] <- personAllele[g-1,parent[g,p,2], chooseParentAllele[g,p,2]]
  
      #used in case of 1st generation
      personAlleleApr[g,p,1] ~ dcat(alleleAprProb)
      personAlleleApr[g,p,2] ~ dcat(alleleAprProb)

      #choose correct allle depending on generation
      personAllele[g,p,1] <- ifelse(g==2, personAlleleApr[g,p,1], personAlleleFromParent[g,p,1])
      personAllele[g,p,2] <- ifelse(g==2, personAlleleApr[g,p,2], personAlleleFromParent[g,p,2])

      personBlood[g,p] ~ dcat(allelesToBlood[personAllele[g,1,1],personAllele[g,1,2],]) # blood group to numerical value A = 1, B = 2, AB = 3, O = 4
    }
  }

}"

GEN_NUM <- 3
POP_NUM <- 50

jags <- jags.model(textConnection(mod.string),
                   n.chains=1, n.adapt=100, data=list("GEN_NUM"=GEN_NUM,"POP_NUM" = POP_NUM))
#burn in
update( jagsModel , n.iter=1000 )
samplesBloodLastGen <- coda.samples(jags, c("personAllele[4,1,1]","personAllele[4,1,2]"), 500, thin=1)

# What is the probability distribution of blood groups in the last generation?
plot(samplesBloodLastGen)

#setting evidence
personBlood <- array(NA,dim=c(4,POP_NUM));
personBlood[4,] <- rep(3,POP_NUM);
jagsEvidence <- jags.model(textConnection(mod.string),
                   n.chains=1, n.adapt=100, data=list("GEN_NUM"=GEN_NUM,"POP_NUM" = POP_NUM, "personBlood"=personBlood))
update( jagsEvidence , n.iter=1000 )
samplesBlood1stGen <- coda.samples(jagsEvidence, c("personBlood[2,1]"), 500, thin=1)
plot(samplesBlood1stGen)
