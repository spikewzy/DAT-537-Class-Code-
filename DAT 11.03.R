library(cbw)
logmarglik(thetamls)
####
rm(list = ls())
library(cbw)

datdf = getfinmdat(symbols = c("AAPL","GOOG","MSFT","^gspc"),
                   symnames = c("apple","google","microsoft","sp500"),
                   from = "2010-01-01",
                   to = "2022-05-31")

modelfrmls = list(prmapple ~ prmsp500 + hml + smb + cma + rmw - 1,
                  prmgoogle ~ prmsp500 + hml + smb + cma + rmw - 1,
                  prmmicrosoft ~ prmsp500 + hml + smb + cma + rmw - 1)

thetamdigals = mapply(FUN = "MCMCregressg",
                      modelfrm = modelfrmls,
                      MoreArgs = list(data = datdf),
                      SIMPLIFY = FALSE)

logmargdiag = sum(logmarglik(thetamdigals))


thetam = MCMCsureg(modelfrmls = modelfrmls,
                   data = datdf,
                   mvr = TRUE)
logmargnondiag = logmarglik(thetam =  thetam)

cbind(logmargdiag,logmargnondiag)
## MVR - MVT
## what is the best nu
## optimize o a grid of nu values

nug = seq(from = 3,
          to = 6,
          by = .2)
thetamtls = mapply(FUN = "MCMCsuret",
                  nu = nug,
                  MoreArgs = list(modelfrmls = modelfrmls,
                                  data = datdf,
                                  mvr = TRUE),
                  SIMPLIFY = FALSE)
length(thetamtls)              
logmargt = logmarglik(thetamtls)
plot(nug,logmargt)
ind = which.max(logmargt)
ind
nu = nug[ind]
thetamt = thetamtls[[ind]]
summarymcmc(thetamt,header = TRUE)
logmargnondiagt = logmarglik(thetamt)

thetamdiagtls = mapply(FUN = "MCMCregresst",
                       modelfrm = modelfrmls,
                       MoreArgs = list(data = datdf,
                                       nu = nu),
                       SIMPLIFY = FALSE)
logmargdiagt = sum(logmarglik(thetamdiagtls))
cbind(logmargdiagt,logmargnondiagt)
logmarglik(thetamt)
logmarglik(thetam)

modelfrmls1 = modelfrmls

modelfrmls2 = list(prmapple ~ prmsp500 + hml + smb + cma + rmw - 1,
                  prmgoogle ~ prmsp500 + hml + smb + cma + rmw - 1,
                  prmmicrosoft ~ prmsp500 + hml + smb + cma + rmw)

modelfrmls3 = list(prmapple ~ prmsp500 + hml + smb + cma + rmw - 1,
                   prmgoogle ~ prmsp500 + hml + smb + cma + rmw,
                   prmmicrosoft ~ prmsp500 + hml + smb + cma + rmw - 1)

thetam2 = MCMCsureg(modelfrmls = modelfrmls2,
                    data = datdf,
                    mvr = FALSE)

modelfrmls = list(modelfrmls1,
                  modelfrmls2,
                  modelfrmls3)

thetamls = mapply(FUN = "MCMCsureg",
                  modelfrmls = modelfrmls,
                  MoreArgs = list(data = datdf,
                                  mvr = FALSE),
                  SIMPLIFY = FALSE)

logmarglik(thetamls)
