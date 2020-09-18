
#' @export anova_txt
#' @param anova object that compares two lm objects
#' @return anova output with inline for rmarkdown
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
#' ma<-lm(a1 ~ 1+b,z) # between-subject design
#' mc<-lm(a1 ~1 , z)
#' x<-anova_txt(mc, ma)
#' x$full

anova_txt<- function(mc, ma){

anov<-  anova(mc, ma)
PRE<-round((sum(mc$residuals^2)-sum(ma$residuals^2))/sum(mc$residuals^2),2) # Proportion of reduction or error
txt<-
  anov[2,] %>%
  broom::tidy(.) %>%
  mutate_if(is.numeric, round, digits=2) %>%
  mutate(p = p_txt(p.value)) %>%
  mutate(F = paste("*F*(",df, ", ",res.df,") = ",statistic, sep = "")) %>%
  mutate(PRE = paste("*PRE* = ",PRE)) %>%
  mutate(full= paste(F,", ",p,", ", PRE, sep = "")) %>%
  mutate(small= paste(F,", ",p, sep = ""))
  # pes<-
  #   effectsize::eta_squared(model, partial = TRUE, ci = 0.9) %>%
  #   data.frame %>%
  #   mutate_if(is.numeric, round, digits=3) %>%
  #   dplyr::rename(term ="Parameter")

  # txt<-txt[txt$Parameter == effect,]
  return(txt)
}

# https://csrgxtu.github.io/2015/03/20/Writing-Mathematic-Fomulars-in-Markdown/
