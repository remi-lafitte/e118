#' @export tt_txt
#' @param
#' model = A t-test object
#' @examples
#' Poids des souris avant traitement
#'a<-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
#' Poids des souris après traitement
#'b<-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
#' x<-t.test(a , mu=  0)
#' y<-t.test(a,b)
#' z<-t.test(a,b, paired = T)
#' tt_txt(x)
#' tt_txt(y, beta = F)
#' tt_txt(z)
#' tt_txt(z, beta = T)
tt_txt <- function(model, beta = T){
  ttest<-model
  # one sample t test

  q<-round(ttest$statistic,2)# statistic q
  dof<-round(ttest$parameter,2)# global degree of freedom
  pv<-p_txt(ttest$p.value)# raw p value. Beyond 10^-6, the round will give 0.
  b <- round(ttest$estimate[1] - ifelse(is.na(ttest$estimate[2]),0,ttest$estimate[2]),2)
  # estimation of the beta slope.

  full<- paste(
    ifelse(isTRUE(beta),
           paste("*M* = ",b, ", ", sep = ""), ""),
   "95% CI [",round(ttest$conf.int[1],2),", ",
               round(ttest$conf.int[2],2),"], *t*(",dof,") = ",q,", ",pv,
    sep ="")

  small<- paste("*t*(",dof,") = ",q,", ",pv,
               sep ="")



  return(list(full=full, small=small))
}

# # by default mu = 0
# library(effsize)

# fit<-as.formula(paste(dv, "~" ,mu))
# # to enter fit object in t.test

# ci<-paste("95% CI [", round(ttest$conf.int[1],2),", ", round(ttest$conf.int[2],2),"]", sep = "")

# cd  <- (mean(df[,dv])-mu)/sd(df[,dv])
# cohen's dz
#   cd_ci<-effsize::cohen.d(df[, dv],f=NA, conf.level = 0.95,mu = mu,
#                            hedges.correction = F)
#   # confidence interval thanks to the effsize package
# ", *d*=",round(cd,2),
