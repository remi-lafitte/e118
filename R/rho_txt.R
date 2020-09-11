#' @export rho_txt
#' @param
#' model = A cor.test object
#' @examples
#' Poids des souris avant traitement
#'a<-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
#' Poids des souris après traitement
#'b<-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
#' x<-cor.test(a,b, method = 'spearman')
#' rho_txt(x)

rho_txt <- function(model){
  cor<-model
  b<-round(cor$estimate,2) # estimation of the beta slope.
  pv<-p_txt(cor$p.value)# raw p value. Beyond 10^-6, the round will give 0.

  small<- paste("*r*~s~ =", b,", ",pv, sep = "")

  return(list(small=small))

}
