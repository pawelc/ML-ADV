yn <- c("yes", "no")
p.R <- parray("Rain", levels=list(yn), values=c(.2, .8))
p.S_R <- parray(c("Sprinkler","Rain"), levels=list(yn,yn), values=c(.01, .99, .4, .6))
p.G_SR <- parray(c("GrassWet","Sprinkler","Rain"), list(yn,yn,yn), values=c(.99, .01, .8, .2, .9, .1, 0, 1))
ftable(p.G_SR, row.vars = "GrassWet")

p.GSR <- tabListMult( list(p.G_SR, p.S_R, p.R) )
ftable( p.GSR, row.vars = "GrassWet" )

p.RG <- tabMarg(p.GSR, c("Rain", "GrassWet")); p.RG
p.G <- tabMarg(p.RG, "GrassWet"); p.G
p.R_G <- tabDiv(p.RG, p.G); p.R_G

data(lizardRAW, package="gRbase")
head(lizardRAW, 4)

data(lizard, package="gRbase")
ftable( lizard, row.vars=1 )
lizard

d <- dag( ~species + height|species + diam|species ); iplot(d)

s <- tabMarg(lizard, ~species); s

h_s <- tabMarg(lizard, ~height + species); h_s

d_s <- tabMarg(lizard, ~diam + species); d_s

p.s <- as.parray(s, normalize="first"); p.s

p.h_s <- as.parray(h_s, normalize="first"); p.h_s

p.d_s <- as.parray(d_s, normalize="first"); p.d_s

yn <- c("yes","no")
x <- c(5,95,1,99)
t.a <- array(x, dim=c(2,2), dimnames=list(tub=yn,asia=yn))
