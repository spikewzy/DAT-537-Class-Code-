rm(list = ls())
library(cbw)
library(czzg)
library(ggplot2)
library(gridExtra)
data("factor12");
datdf = factor12
datdf = datdf[1:528,]; # rest is for evaluation
fnames = names(datdf)

# full model xnames = fnames

thetam = CZZfactorg(xnames = fnames,
                    data = datdf,
                    m = 20000) # one MVR model

dim(thetam)
summaryczz(thetam = thetam)
CZZsummaryb(thetam = thetam)

# another model/split

xnames = c("Mkt","SMB","MCM","ROE","PEAD","MGMT");

xnamesff3 = c("Mkt","SMB","HML")
wnamesff3 = setdiff(fnames,xnamesff3)

# 6 + 21 lamx + Omegax
# 36 + 21 Gamma + Omegaw.x
# 42 + 42 = 84

thetam = CZZfactorg(xnames = xnames,
                    data = datdf,
                    m = 20000);
summaryczz(thetam)

pout = pricing(xnames = xnames,
               data = datdf)

# summary of b
CZZsummaryb(thetam = thetam)
bm = attr(thetam,"bm")
bmean = apply(bm,2,"mean")
weights = bmean/sum(bmean)
weights  # weights on a factor portfolio that 

