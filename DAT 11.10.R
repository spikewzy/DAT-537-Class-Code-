rm(list = ls())
library(cbw)
library(czzg)
library(ggplot2)
library(gridExtra)
data("factor12")
datdf = factor12
datdf = datdf[1:528,]; #rest is for evaluation
fnames = names(datdf);

# with K = 12
# 2^K -1 possible splits



scanls  = CZZscang(data = datdf,nclust = 4);
scandford = scanls$scanord
xbest = names(which(scandford[1,] == 1))

thetam = CZZfactorg(xnames = xbest,
                    data  = datdf,
                    m = 20000);
dim(thetam)
CZZsummaryb(thetam);

pout = pricing(xnames = xbest,
               data = datdf)
pout

xnamesff6 = c("Mkt","SMB","HML","RMW","CMA","MOM" )

# let us the model with the ff6 factors

modind = 0;

for (j in 1:4095) {
  cat("this is j",j,"n") 
    facj = xbest = names(which(scandford[j,] == 1))
    if (length(facj) == 6) {
      comj = facj == xnamesff6;
      if (mean(comj) == 1) {
        cat("this is the ff6 model","\n")
        modind = j
      }
    }
    
}


indff6 = c(rep(1,6),rep(0,6))

ff6find = scandford[,1:12] == indff6
ff6find[1:2,]

pout = pricing(xnames = xnamesff6,
               data = datdf)
pout

thetam = CZZfactorg(xnames = xnamesff6,
                    data = datdf)
CZZsummaryb(thetam)

xnames = c("Mkt","SMB","HML")
wnames = setdiff(datnames,xnames); J = length(wnames);

make3by2factor = function(g = g,
                          datals = datals,
                          rename = rename,
                          mvename = mvename,
                          cname = cname) {
  data = datals[[g]];
  vnames = c(rename,mvename,cname)
  datas = data[,vnames]
  datas[,mvename] = exp(datas[,mvename])
  
  mvemed = median(datas[,mvename])
  cq = quantile(datas[,cname],probs = c(.33,.67))
  
  ind11 = (datas[,mvename] < mvemed) & (datas[,cname] < cq[1]);
  ind12 = (datas[,mvename] > mvemed) & (datas[,cname] < cq[1]);
  ind31 = (datas[,mvename] < mvemed) & (datas[,cname] > cq[2]);
  ind32 = (datas[,mvename] > mvemed) & (datas[,cname] > cq[2]);
  
  dat11 = datas[ind11,]
  w11 = dat11[,mvename]/sum(dat11[,mvename]);
  vw11 = sum(w11*dat11[,rename])
  dat11 = datas[ind11,]
  w11 = dat11[,mvename]/sum(dat11[,mvename]);
  vw11 = sum(w11*dat11[,rename])
  
  dat12 = datas[ind12,]
  w12 = dat12[,mvename]/sum(dat12[,mvename]);
  vw12 = sum(w12*dat12[,rename])
  
  dat31 = datas[ind31,]
  w31 = dat31[,mvename]/sum(dat31[,mvename]);
  vw31 = sum(w31*dat31[,rename])
  
  dat32 = datas[ind32,]
  w32 = dat32[,mvename]/sum(dat32[,mvename]);
  vw32 = sum(w32*dat32[,rename])
  
  ft = 0.5*(vw31 + vw32) - 0.5*(vw11 + vw12)
  return(ft)
}
classdata = readRDS("classdata.Rds")
fbm = sapply(1:T,
             FUN = "make3by2factor",
             datals = calssdata,
             rename = "Re",
             mvename = "mve",
             cname = "bm")

T = length()