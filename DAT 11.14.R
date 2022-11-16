# making factors by 3by2 sorts by the FF method
# example classdata.RDS

rm(list = ls())
datals = readRDS("classdata.RDS")
T = length(datals)  # 8 years of monthly data

data1 = datals[[1]]
date1 = data1$date[1]
date1
class(data1)



# for a particular month
# returns a 1*1 factor value in month t

make3by2factor = function(g = g,
                          datals = datals,
                          rename = rename,
                          mvename = mvename,
                          cname = cname) {
  
  if (cname == mvename) {
    stop("this function does not make the mve factor","\n")
  }
  data = datals[[g]];
  vnames = c(rename,mvename,cname)
  datas = data[,vnames]
  datas[,mvename] = exp(datas[,mvename])
  
  mvemed = median(datas[,mvename])
  cq = quantile(datas[,cname],probs = c(.30,.70));
  
  vw = matrix(0,nr = 3,nc = 2);
  
  for (i in 1:3) {
    for (j in 1:2) {
      if (i == 1 & j == 1) {
        ind = (datas[,cname] <= cq[1]) & (datas[,mvename] <= mvemed);
      } else if (i == 1 & j == 2) {
        ind = (datas[,cname] <= cq[1]) & (datas[,mvename] > mvemed);
        
      } else if (i == 2 & j == 1) {
        ind = (datas[,cname] > cq[1]) & (datas[,cname] <= cq[2]) & (datas[,mvename] <= mvemed); 
      } else if (i == 2 & j == 2) {
        ind = (datas[,cname] > cq[1]) & (datas[,cname] <= cq[2]) & (datas[,mvename] > mvemed);
        
      } else if (i == 3 & j == 1) {
        ind = (datas[,cname] > cq[2]) & (datas[,mvename] <= mvemed); 
      } else {
        ind = (datas[,cname] > cq[2]) & (datas[,mvename] > mvemed);
      }
      datij = datas[ind,]
      wij = datij[,mvename]/sum(datij[,mvename]);
      vw[i,j] = sum(wij*datij[,rename]);  
    }
  }
  
  ft = 0.5*(vw[3,1] + vw[3,2]) - 0.5*(vw[1,1] + vw[1,2])
  attr(ft,"vw") = vw;
  return(ft)
}

make3by2factormve = function(g = g,
                             datals = datals,
                             rename = rename,
                             mvename = mvename,
                             bmname = bmname,
                             operprofname = operprofname,
                             agrname = agrname) {
  
  fbm = make3by2factor(g = g,
                       datals = datals,
                       rename = rename,
                       mvename = mvename,
                       cname = bmname);
  vwbm = attr(fbm,"vw");
  bm = sum(vwbm[,1]) - sum(vwbm[,2]);
  
  foperprof = make3by2factor(g = g,
                             datals = datals,
                             rename = rename,
                             mvename = mvename,
                             cname = operprofname);
  vmoperprof = attr(foperprof,"vw");
  operprof = sum(vmoperprof[,1]) - sum(vmoperprof[,2])
  
  fagr = make3by2factor(g = g,
                        datals = datals,
                        rename = rename,
                        mvename = mvename,
                        cname = agrname);
  vmagr = attr(fagr,"vw");
  agr = sum(vmagr[,1]) - sum(vmagr[,2]);
  
  ft = (1/3)*(bm + operprof + agr);
  return(ft)
}


fbm = sapply(1:T,
             FUN = "make3by2factor",
             datals = datals,
             rename = "Re",
             mvename = "mve",
             cname = "bm");

fmve = sapply(1:T,
              FUN = "make3by2factormve",
              datals = datals,
              rename = "Re",
              mvename = "mve",
              bmname = "bm",
              operprofname = "operprof",
              agrname = "agr");

T = length(datals)
dataT = datals[[T]]
dateT = dataT$date[1]
head(dataT)

factor10 = matrix(0,nr = T,nc = 10);
cnames = names(dataT)[4:13];

for (j in 1:9) {
  cnamej = cnames[j]
  factor10[,j] = sapply(1:T,
                        FUN = "make3by2factor",
                        datals = datals,
                        rename = "Re",
                        mvename = "mve",
                        cname = cnamej
  );
}

factor10[,10] = fmve
factor10 = as.data.frame(factor10)
names(factor10) = paste("f",cnames,sep = "");
class(factor10)
head(factor10)

library(cbw)
datsp500 = getfinmdat(symbols = "^gspc",
                      symnames = "sp500",
                      from = as.Date("2012-12-31"),
                      to = as.Date("2020-12-31"))
rnames = rownames(datsp500)
rownames(factor10) = rnames;
prmsp500 = datsp500$prmsp500;
factor11 = cbind(prmsp500,factor10)
head(factor11)

library(czzg)
scanls = CZZscang(data = factor11,trainpct = .20)
scanord = scanls$scanord
head(scanord)
xst = names(which(scanord[1,] == 1))

pout = pricing(xnames = xst,
               data = factor11)
pout

# suppose that K is too large (eg. 48)

# step 1

library(czfactor)

xnames = names(factor11)
thetam = CZfactorg(xnames = xnames,
                   data = factor11,
                   priorczz = 1,
                   trainpct = .2)

b = CZsummaryb(thetam = thetam)


bb = b[,4] * b[,5]
bind = bb > 0
bind

xbsig = "prmsp500"


# step 2

pout = pricing(xnames = xbsig,data = factor11)
pout
xnames = c(xbsig,"fbeta","fidiovol","fmom12m");


# step 3

factor11s = factor11[,xnames]

library(czzg)

scanls = CZZscang(data = factor11s,trainpct = .2)
scanord = scanls$scanord
scanord