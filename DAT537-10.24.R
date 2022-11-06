library(cbw)
library(dplyr)
prmdf0 = getfinmdat()
prmdf00 = filter(prmdf0,rownames(prmdf0) >= "2010-02-26")
prmdf = getfinmdat(from = "2010-01-01",
                   to = "2021-03-31")
ns = dim(prmdf)[1]; # n + s
s = 10
n = (ns-s);
prmdfn = prmdf[1:n,,drop = F]; # train sample + estimation sample
prmdff = prmdf[(n+1):ns,,drop = F] # hold out sample for prediction test

thetamcapmnc = MCMCregressg(prmibm~prmsp500-1,
                            data = prmdfn);

thetamcapm = MCMCregressg(prmibm~prmsp500,
                          data = prmdfn);

d12 = logmarglik(thetamcapmnc) - logmarglik(thetamcapm)
d12 

thetamff5 = MCMCregressg(modelfrm = prmibm ~ prmsp500+hml+smb+cma+rmw,
                         data = prmdfn)

thetamff5nc = MCMCregressg(modelfrm = prmibm~ prmsp500+hml+smb+cma+rmw-1,
                           data = prmdfn)
d12 = logmarglik(thetamff5nc) - logmarglik(thetamff5)

# model search on the nu space

nug = c(3.1,3.2,3.3,3.4,3.5)

thetamls = mapply(FUN = "MCMCregresst",
                  nu = nug,
                  MoreArgs = list(modelfrm = prmibm~prmsp500+hml+smb+cma+rmw-1,
                                  data = prmdfn),
                  SIMPLIFY = FALSE)

logmarg = logmarglik(thetamls)
logmarg
A = cbind(nug,t(logmarg))
A
