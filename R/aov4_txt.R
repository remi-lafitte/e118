
#' @export aov4_txt
#' @param afex::aov_4 object
#' @return anova output with inline for rmarkdown
#' @examples
#'example 1
#'library(afex)
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
#' model<-afex::aov_4(a1~ b * c + (1|ID),z) # between-subject design
#' result<-aov4_txt(model = model)
#' result$c$full
emmeans(model, ~ b * c)
model$lm$coefficients
View(model)

aov4_txt<- function(model){

   pes<-
    effectsize::eta_squared(model, partial = TRUE, ci = 0.9) %>%
    data.frame %>%
    mutate_if(is.numeric, round, digits=3)


    table<-
      model$anova_table %>%
      data.frame %>%
      mutate(Parameter = rownames(.))%>%
      mutate_if(is.numeric, round, digits=2) %>%
      inner_join(., pes, by = "Parameter")


    txt<-
       table %>%
      dplyr::group_by(Parameter) %>%
      mutate(p = p_txt(Pr..F.)) %>%
      mutate(F = paste("*F*(",num.Df, ", ",den.Df,") = ",F, sep = "")) %>%
      mutate(ges = paste("$\\hat{\\eta}^2_G$ = ",round(ges,2))) %>%
      mutate(pes = paste("$\\hat{\\eta}^2_p$ = ",round(Eta_Sq_partial,2))) %>%
      mutate(pes_ci =  paste("95% CI [",round(CI_low,2),", ",round(CI_high,2),"]", sep = "")) %>%
      mutate(pes_full =  paste(pes,", ", pes_ci, sep = "")) %>%
      mutate(full= paste(F,", ",p,", ", ges,", ", pes,", ", pes_ci, sep = "")) %>%
      mutate(small= paste(F,", ",p, sep = ""))
#
     rownames(txt)<- txt$Parameter
     list <- setNames(split(txt, seq(nrow(txt))), rownames(txt))
     # txt<-txt[txt$Parameter == effect,]

    return(list(txt  = list))
}

# https://csrgxtu.github.io/2015/03/20/Writing-Mathematic-Fomulars-in-Markdown/
