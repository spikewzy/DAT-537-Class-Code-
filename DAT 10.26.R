mypredictregresst = function(thetam = thetam,
                             pdatdf = pdatdf,  # data frame with one row
                             seed = 201) {
  modelfrm = attr(thetam,"modelfrm");
  nu = attr(thetam,"nu");
  k1 = dim(thetam)[2];
  k = k1 - 1;
  betam = thetam[,1:k,drop = F];
  s2m = thetam[,k1];
  tau2m = s2m*(nu-2)/nu;
  m = dim(thetam)[1];
  Xf = model.matrix(modelfrm,pdatdf);  # x at t+1
  dts = as.Date(rownames(pdatdf));
  yname = attr(thetam,"yname");
  yf = model.frame(modelfrm,pdatdf)[,yname];
  yfm = matrix(0, nr = m, nc = 1);
  set.seed(seed)
  for (g in 1:m) {
    beta = as.matrix(betam[g,]);
    tau2 = tau2m[g];
    lamg = rgamma(1,shape = nu/2,rate = nu/2);
    yfg = Xf %*% beta + sqrt(tau2) * rnorm(1)/sqrt(lamg);
    yfm[g,1] = yfg
  }
  yfhat = apply(yfm, 2, "mean");
  sdf = apply(yfm, 2, "sd");
  lower = apply(yfm, 2, "quantile", 0.025);
  upper = apply(yfm, 2, "quantile", 0.975);
  outls = list(yf = yf,
               yfm = yfm,
               yfhat = yfhat,
               sdf = sdf,
               lower = lower,
               upper = upper,
               dts = dts);
  return(outls);
}

library(cbw);
datdf = getfinmdat(symbols = c("AAPL","^gspc"),
                   symnames = c("apple","sp500"),
                   from = "2010-01-01",
                   to = "2022-06-30")

capmfrm = prmapple~prmsp500 - 1
nu = 12.5;   # this nu is not the best nu at time nt + t
nt = 20; 
t = nt + 24;
datdft = datdf[1:t,,drop = F];
thetam = MCMCregresst(modelfrm = capmfrm,
                      data = datdft,
                      trainsize = nt,
                      nu = nu,
                      m = 10000);

pdatdft = datdf[(t+1),,drop = F]



poutls = mypredictregresst(thetam = thetam,
                           pdatdf = pdatdft);


# loop over months on an expanding window

capmfrm = prmapple~prmsp500 - 1
nu = 12.5;   # this nu is not the best nu at time nt + t
nt = 20; 
t = nt + 24;
n = dim(datdf)[1]
r = n - t 

yfhatm = rep(0,r);
ytruem = rep(0,r);
datesm = structure(rep(NA_real_,r),
                   class="Date")

for (j in 1:r) {
  cat("j",j,"\n");
  datdft = datdf[1:t,,drop = F];
  thetam = MCMCregresst(capmfrm,data = datdft,trainsize = nt,
                        nu = nu,
                        m = 10000)
  pdatdf = datdf[(t+1),,drop = F]
  poutls = mypredictregresst(thetam,pdatdf);
  dts = poutls$dts
  yfhat = poutls$yfhat;
  yf = poutls$yf;
  
  cat("forecast month","\n");
  print(dts);
  cat("yf",yf,"\n");
  cat("yfhat",yfhat,"\n");
  cat("\n");
  
  yfhatm[j] = yfhat;
  ytruem[j] = yf;
  datesm[j] = dts;
  t = t + 1;
}

plot(ytruem,yfhatm)
cor(ytruem,yfhatm)
datesm

validationdf = data.frame(month = datesm,
                          ytrue = ytruem,
                          yfhat = yfhatm)
validationdf