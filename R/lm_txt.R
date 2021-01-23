
#' @export lm_txt
#' @param lm object
#' @return lm output with inline for rmarkdown
#' @examples
#'example 1
#'library(magrittr)
#'library(dplyr)
#'library(tidyr)
#' set.seed(42)
#' z <- data.frame(a1 = c(rnorm(100,2), rnorm(100,1),rnorm(100,0)),
#'                 b  = rep(c("A", "B", "C"), each = 100),
#'                 c  = factor(rbinom(300, 1, .5)),
#'                 ID = 1:300,
#'                 a2 = c(rnorm(100,2), rnorm(100,1),rnorm(100,0)),
#'                 a3 = c(rnorm(100,2), rnorm(100,1),rnorm(100,0)))
#' model<-lm(a1 ~ 1 + b*c,z) # between-subject design
#' txt<-lm_txt(model = model)
#' result$b$full
# txt$`bC:c1`$full
# PRE=(sum(modelC$residuals^2)-sum(modelA$residuals^2))/sum(modelC$residuals^2)


lm_txt<- function(model){

  par<-broom::glance(model)

   txt<-
    broom::tidy(model, conf.int = T, ddl = T) %>%
    mutate_if(is.numeric, round, digits=2) %>%
    group_by(term) %>%
    mutate(p = p_txt(p.value)) %>%
    mutate(t = paste("*t*(",par$df.residual,") = ",statistic, sep = "")) %>%
    mutate(slope = paste("$B$ = ", estimate, sep = "")) %>%
    mutate(slope_ci= paste("95% CI [",conf.low,", ",conf.high,"]", sep = "")) %>%
    mutate(slope_full =  paste(slope,", ", slope_ci, sep = "")) %>%
    mutate(full= paste(t,", ",p,", ", slope_full, sep = "")) %>%
    mutate(small= paste(t,", ",p, sep = ""))
   rownames(txt)<- txt$Parameter
   # list <- setNames(split(txt, seq(nrow(txt))), rownames(txt))
   # txt<-txt[txt$Parameter == effect,]

   return(txt)
}

# https://csrgxtu.github.io/2015/03/20/Writing-Mathematic-Fomulars-in-Markdown/
