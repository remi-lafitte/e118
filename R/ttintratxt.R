#' Add together two numbers
#'
#' @param x A number
#' @param y A number
#' @return The sum of \code{x} and \code{y}
#' @examples
#' add(1, 1)
#' add(10, 1)

tt_intra_txt <- function(dv, iv , df){
  # library(effsize)

  fit<-as.formula(paste(dv, "~" ,iv))
  # to enter fit object in t.test

  ttest<-t.test(fit, data = df, paired = T)
  # t test

  mean1<-tapply(df[,dv], df[,iv], mean)[1]
  mean2<-tapply(df[,dv], df[,iv], mean)[2]
  sd1<-tapply(df[,dv], df[,iv],sd)[1]
  sd2<-tapply(df[,dv], df[,iv],sd)[2]
  # mean and standard deviation

  q<-round(ttest$statistic,2)# statistic q
  dof<-round(ttest$parameter,2)# global degree of freedom
  pv<-round(ttest$p.value,6)# raw p value. Beyond 10^-6, the round will give 0.
  b<-round(ttest$estimate,2)# estimation of the beta slope.

  #Now, we can calculate the Cohen's dZ (cdz) effect size
  #(see =
  #https://memory.psych.mun.ca/models/stats/effect_size.shtml) =
  #http://jakewestfall.org/blog/index.php/2016/03/25/five-different-cohens-d-statistics-for-within-subject-designs/
  #https://www.frontiersin.org/articles/10.3389/fpsyg.2013.00863/full
  #https://stats.stackexchange.com/questions/201629/cohens-d-for-dependent-sample-t-test
  #)

  cdz  <- q/sqrt(dof)
  # cohen's dz

  cdz_ci<-effsize::cohen.d(df[, dv], f = df[, iv], conf.level = 0.95,
                   hedges.correction = F, paired = T, pooled = T, within=F)
  # confidence interval thanks to the effsize package

  inline<-paste("*t*(",dof,")=",q,", *p*<",pv,", *d*=",round(cdz,2),", 95% CI[",round(cdz_ci$conf.int[1],2),", ",
                round(cdz_ci$conf.int[2],2),"]", sep = "")

  return(inline)
}
# d[43,"genre"]
# d$genre[43,]
