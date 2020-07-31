# Example =
#data(CO2)
#df = CO2
#dv = "uptake"
# mu = 0
#tt_mu_txt(dv = "uptake", mu = 200,df = CO2)
#tt_mu_txt(dv = "uptake",df = CO2)

tt_mu_txt <- function(dv , mu = 0, df){
  # by default mu = 0
  library(effsize)

  fit<-as.formula(paste(dv, "~" ,mu))
  # to enter fit object in t.test

  ttest<-t.test(x = df[,dv],y=NULL, mu = mu, data = df)
  # one sample t test

  q<-round(ttest$statistic,2)# statistic q
  dof<-round(ttest$parameter,2)# global degree of freedom
  pv<-round(ttest$p.value,6)# raw p value. Beyond 10^-6, the round will give 0.
  b<-round(ttest$estimate,2)# estimation of the beta slope.

  cd  <- (mean(df[,dv])-mu)/sd(df[,dv])
  # cohen's dz

  cd_ci<-effsize::cohen.d(df[, dv],f=NA, conf.level = 0.95,mu = mu,
                           hedges.correction = F)
  # confidence interval thanks to the effsize package

  inline<-paste("*t*(",dof,")=",q,", *p*<",pv,", *d*=",round(cd,2),", 95% CI[",round(cd_ci$conf.int[1],2),", ",
                round(cd_ci$conf.int[2],2),"]", sep = "")

  return(inline)
}

