
#' @export lme4_txt
#' @param afex::lme4 object
#' @return anova output with inline for rmarkdown
#' @examples
#'example 1
#'library(lme4)
#'library(magrittr)
#'library(dplyr)
#'library(broom.mixed)
#'library(tidyr)
#' set.seed(42)
#' z <- data.frame(a1 = c(rnorm(100,2), rnorm(100,1),rnorm(100,0)),
#'                 b  = rep(c("A", "B", "C"), each = 100),
#'                 c  = factor(rbinom(300, 1, .5)),
#'                 ID = 1:300,
#'                 a2 = c(rnorm(100,2), rnorm(100,1),rnorm(100,0)),
#'                 a3 = c(rnorm(100,2), rnorm(100,1),rnorm(100,0)))
#' w<-gather(z, key = a, value = x, c("a1", "a2", "a3"))
#' model<-lme4::lmer(x~ a + (1|ID),w) # between-subject design
#' x<-lme4_txt(model = model)
#' x$aa2$slope

lme4_txt<- function(model){

txt<-
  broom.mixed::tidy(model, conf.int = T, df = T) %>%
  dplyr::filter(effect == "fixed") %>%
  dplyr::group_by(term) %>%
  mutate_if(is.numeric, round, digits=2) %>%
  mutate(slope = paste("$B$ = ", estimate, sep = "")) %>%
  mutate(slope_ci= paste("95% CI [",conf.low,", ",conf.high,"]", sep = "")) %>%
  mutate(slope_full =  paste(slope,", ", slope_ci, sep = ""))


  rownames(txt)<- txt$term

  list <- setNames(split(txt, seq(nrow(txt))), rownames(txt))
  return(list)
}

# https://csrgxtu.github.io/2015/03/20/Writing-Mathematic-Fomulars-in-Markdown/
