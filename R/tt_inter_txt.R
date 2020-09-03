#' Add together two numbers
#'
#' @param x A number
#' @param y A number
#' @return The sum of \code{x} and \code{y}
#' @examples
#' tt_inter_txt(dv = "uptake", iv = "Type", df = CO2, welch = TRUE) # ...or welch = T


tt_inter_txt <- function(dv, iv , df, welch= c(F, T)){
  library(effsize)
  # if welch is forgotten, the default will be "FALSE", that is group variances are supposed equal.

  fit<-as.formula(paste(dv, "~" ,iv))
  # to enter fit object in t.test

  ttest<-t.test(fit, data = df, var.equal = !welch, paired = FALSE)
  # t test

  dof1<-table(df[,iv])[1]
  dof2<-table(df[,iv])[2]
  # degree of freedom

  mean1<-tapply(df[,dv], df[,iv], mean)[1]
  mean2<-tapply(df[,dv], df[,iv], mean)[2]
  sd1<-tapply(df[,dv], df[,iv],sd)[1]
  sd2<-tapply(df[,dv], df[,iv],sd)[2]
  # mean and standard deviation

  q<-round(ttest$statistic,2)# statistic q
  dof<-round(ttest$parameter,2)# global degree of freedom
  pv<-round(ttest$p.value,6)# raw p value. Beyond 10^-6, the round will give 0.
  b<-round(ttest$estimate,2)# estimation of the beta slope.

  #Now, we can calculate the Cohen's d (cd) effect size
  #(see https://memory.psych.mun.ca/models/stats/effect_size.shtml) =

  dof1_cd <- dof1 - 1
  dof2_cd <- dof2 - 1
  numerator  <- mean1 -  mean2
  denominator <- sqrt(
     ( (dof1_cd*(sd1^2)) + (dof2_cd*(sd2^2)) ) / (dof1_cd + dof2_cd)
    )
  cd  <- numerator/denominator
  # cohen's d

  cd_ci<-effsize::cohen.d(df[, dv], f = df[, iv], conf.level = 0.95,
                           hedges.correction = F)
 # confidence interval thanks to the effsize package

  txt<-paste("*t*(",dof,")=",q,", *p*<",pv,", *d*=",round(cd,2),", 95% CI[",round(cd_ci$conf.int[1],2),", ",
                round(cd_ci$conf.int[2],2),"]", sep = "")

  return(txt)
}
