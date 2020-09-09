#' @export tt_mu_txt
#' @param tt_mu_txt(t.test object)
#' @examples
#' data(mtcars)
#' x<-t.test(mtcars$qsec , mu=  0)
#' y<-tt_mu_txt(x)
#' y
tt_mu_txt <- function(model){

  p_txt<-function(p){

    a = 'p < '
    b = 'p = '
    p <- ifelse(is.character(p), as.numeric(p), p)
    p3 <- round(p, 3)


    pv<-
      ifelse(p < 0.000001,
             "p < 10^-6",
             ifelse(p < .00001,
                    "p < 10^-5",
                    ifelse(p < .0001,
                           "p < 10^-4",
                           ifelse(p < .001,
                                  "p < 0.001",
                                  ifelse(p < .05,
                                         paste("*p* < ", p3, sep = ""),
                                         paste("*p* = ", p3, sep = ""))))))

    return(pv)
  }

  # # by default mu = 0
  # library(effsize)

  # fit<-as.formula(paste(dv, "~" ,mu))
  # # to enter fit object in t.test

  ttest<-model
  # one sample t test

  q<-round(ttest$statistic,2)# statistic q
  dof<-round(ttest$parameter,2)# global degree of freedom
  pv<-p_txt(ttest$p.value)# raw p value. Beyond 10^-6, the round will give 0.
  b<-round(ttest$estimate,2)# estimation of the beta slope.
  # ci<-paste("95% CI [", round(ttest$conf.int[1],2),", ", round(ttest$conf.int[2],2),"]", sep = "")

  # cd  <- (mean(df[,dv])-mu)/sd(df[,dv])
  # cohen's dz
#   cd_ci<-effsize::cohen.d(df[, dv],f=NA, conf.level = 0.95,mu = mu,
#                            hedges.correction = F)
#   # confidence interval thanks to the effsize package
  # ", *d*=",round(cd,2),

  full<-
    paste(
    "$M = "
    ,b,
    "$, 95\\% CI $["
    ,round(ttest$conf.int[1],2),
    "$, $"
    ,round(ttest$conf.int[2],2),
    "]$, $t("
    ,dof,
    ") = "
    ,q,
    "$, $"
    ,pv,
    "$",
    sep =""
  )



  return(list(txt = full))
}



