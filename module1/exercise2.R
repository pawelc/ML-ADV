require(gRain)

#alleles values
alleles   <- c("A","B","O")
twoAlleles   <- c("AA","BA","OA","AB","BB","OB","AO","BO","OO")
bloodTypes <- c("A","B","AB","O")

#blood type of for given combination of alleles
blood.prob.tbl <- matrix( c( 1, 0, 0, 0, #"AA"
                             0, 0, 1, 0, #"BA"
                             1, 0, 0, 0, #"OA"
                             0, 0, 1, 0,#AB
                             0, 1, 0, 0,#BB
                             0, 1, 0, 0,#OB
                             1, 0, 0, 0,#AO
                             0, 1, 0, 0,#BO
                             0, 0, 0, 1),#OO
                          nrow=4, ncol=9, byrow=FALSE, dimnames=list(bloodTypes, twoAlleles))

#####
#Parent of the mother, grandparents 1
#####

#1st allele value for grandma 1
allele1Grandma1  <- cptable( ~ allele1Grandma1, values=c(0.28, 0.06 ,0.66), levels=alleles)
#2nd allele value for grandma 1
allele2Grandma1  <- cptable( ~ allele2Grandma1, values=c(0.28, 0.06 ,0.66), levels=alleles)
#blood type of grandma 1
bloodGrandma1 <- cptable( ~ bloodGrandma1|allele1Grandma1 + allele2Grandma1, values=blood.prob.tbl, levels=bloodTypes)

#1st allele value for grandpa 1
allele1Grandpa1  <- cptable( ~ allele1Grandpa1, values=c(0.28, 0.06 ,0.66), levels=alleles)
#2nd allele value for grandpa 1
allele2Grandpa1  <- cptable( ~ allele2Grandpa1, values=c(0.28, 0.06 ,0.66), levels=alleles)
#blood type of grandpa 1
bloodGrandpa1 <- cptable( ~ bloodGrandpa1|allele1Grandpa1 + allele2Grandpa1, values=blood.prob.tbl, levels=bloodTypes)

#####
#Parent of the farher, grandparents 2
#####

#1st allele value for grandma 2
allele1Grandma2  <- cptable( ~ allele1Grandma2, values=c(0.28, 0.06 ,0.66), levels=alleles)
#2nd allele value for grandma 2
allele2Grandma2  <- cptable( ~ allele2Grandma2, values=c(0.28, 0.06 ,0.66), levels=alleles)
#blood type of grandma 2
bloodGrandma2 <- cptable( ~ bloodGrandma2|allele1Grandma2 + allele2Grandma2, values=blood.prob.tbl, levels=bloodTypes)

#1st allele value for grandpa 2
allele1Grandpa2  <- cptable( ~ allele1Grandpa2, values=c(0.28, 0.06 ,0.66), levels=alleles)
#2nd allele value for grandpa 2
allele2Grandpa2  <- cptable( ~ allele2Grandpa2, values=c(0.28, 0.06 ,0.66), levels=alleles)
#blood type of grandpa 2
bloodGrandpa2 <- cptable( ~ bloodGrandpa2|allele1Grandpa2 + allele2Grandpa2, values=blood.prob.tbl, levels=bloodTypes)

child.allele.prob.tbl <- matrix( c( 1, 0, 0, #"AA"
                                    0.5, 0.5, 0, #"BA"
                                    0.5, 0, 0.5, #"OA"
                                    0.5, 0.5, 0,#AB
                                    0, 1, 0,#BB
                                    0, 0.5, 0.5,#OB
                                    0.5, 0, 0.5,#AO
                                    0, 0.5, 0.5,#BO
                                    0, 0, 1),#OO
                                 nrow=3, ncol=9, byrow=FALSE, dimnames=list(alleles, twoAlleles))

#####
#Parents
#####

#1st allele value for mother
allele1Monther  <- cptable( ~ allele1Monther | allele1Grandma1 + allele2Grandma1, values=child.allele.prob.tbl, levels=alleles)
#2nd allele value for mother
allele2Monther  <- cptable( ~ allele2Monther | allele1Grandpa1 + allele2Grandpa1, values=child.allele.prob.tbl, levels=alleles)
#blood type of mother
bloodMother <- cptable( ~ bloodMother|allele1Monther + allele2Monther, values=blood.prob.tbl, levels=bloodTypes)

#1st allele value for father
allele1Father  <- cptable( ~ allele1Father | allele1Grandma2 + allele2Grandma2, values=child.allele.prob.tbl, levels=alleles)
#2nd allele value for father
allele2Father  <- cptable( ~ allele2Father | allele1Grandpa2 + allele2Grandpa2, values=child.allele.prob.tbl, levels=alleles)
#blood type of father
bloodFather <- cptable( ~ bloodFather|allele1Father + allele2Father, values=blood.prob.tbl, levels=bloodTypes)

#####
#Children
#####

#1st allele value for a child (from mother)
allele1Child <- cptable( ~ allele1Child | allele1Monther + allele2Monther, values=child.allele.prob.tbl, levels=alleles)
#2nd allele value for a child (from father)
allele2Child <- cptable( ~ allele2Child | allele1Father + allele2Father, values=child.allele.prob.tbl, levels=alleles)

#blood type of child
bloodChild <- cptable( ~ bloodChild|allele1Child + allele2Child, values=blood.prob.tbl, levels=bloodTypes)

#compile nodes
cptlist <- compileCPT(list(allele1Grandma1,allele2Grandma1,allele1Grandma2,allele2Grandma2,allele1Grandpa1,allele2Grandpa1,allele1Grandpa2,allele2Grandpa2,
                           bloodGrandma1,bloodGrandma2,bloodGrandpa1,bloodGrandpa2,                         
                           allele1Monther,allele2Monther, bloodMother, allele1Father, allele2Father, bloodFather, allele1Child, allele2Child, bloodChild))

#create grain object
g<-grain(cptlist)

#show plot of the probabilistic graphical model
iplot(g)