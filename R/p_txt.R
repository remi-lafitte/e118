#' @export p_txt
#' @param
#' A p value
#' @examples
p_txt<-function(p){

  a = 'p < '
  b = 'p = '
  p <- ifelse(is.character(p), as.numeric(p), p)
  p3 <- round(p, 3)


  pv<-
    ifelse(p < 0.000001,
           "*p* < 10^-6^",
           ifelse(p < .00001,
                  "*p* < 10^-5^",
                  ifelse(p < .0001,
                         "*p* < 10^-4^",
                         ifelse(p < .001,
                                "*p* < 0.001",
                                ifelse(p < .05,
                                       paste("*p* < ", p3, sep = ""),
                                       paste("*p* = ", p3, sep = ""))))))

  return(pv)
}
# p_txt(0.00006)
