#' @export cor_txt
#' @param
#' model = A cor.test object
#' @examples
#' Poids des souris avant traitement
#'a<-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
#' Poids des souris après traitement
#'b<-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
#' x<-cor.test(a,b)
#' cor_txt(x)
cor_txt <- function(model){
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
  b<-round(cor$estimate,2) # estimation of the beta slope.
  # q<-round(cor$statistic,2)# statistic q
  dof<-round(cor$parameter,2)# global degree of freedom
  pv<-p_txt(cor$p.value)# raw p value. Beyond 10^-6, the round will give 0.

  full<- paste("$r(",dof,") = ",b,
               ", 95\\% CI $[",
               round(cor$conf.int[1],2),
               "$, $",
               round(cor$conf.int[2],2),
               "]$, $",
               pv,"$",
               sep ="")

  small<- paste("$r(",dof,") = ",b,"$, $",pv,"$",
                sep ="")


  return(list(full=full, small=small))

}
