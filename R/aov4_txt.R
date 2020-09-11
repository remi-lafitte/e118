
#' @export aov4_txt
#' @param afex::aov_4 object
#' @param effect = the effect of the model that you want to extract. if not filled, extract all the effects
#' @return anova output with inline for rmarkdown
#' @examples
#'example 1
#'library(afex)
#'library(magrittr)
#'library(dplyr)
#'library(tidyr)
#' data(mtcars)
#' data<-mtcars
#' data$ID<-1:nrow(data)
#' model<-afex::aov_4(mpg~ vs * am + (1|ID), data) # between-subject design
#' result<-anova_txt(model = model, effect = "vs")
#' result$txt
#' example 2
#' data(mtcars)
#' data<-mtcars %>% mutate(ID = rep(1:16, 2), intra = rep(c(0,1), each=16))
#' model<-afex::aov_4(mpg~ 1 + (intra|ID), data) # within-subject design
#' result<-anova_txt(model = model)
#' result$anova$txt
aov4_txt(model, effect = "vs")

aov4_txt<- function(model_aov_4, effect = txt$Parameter){
    # library(afex)
    # library(effectsize)
    # library(dplyr)
    # library(magrittr)

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


     txt<- table %>%
      mutate(
        txt= paste("*F*(",
              num.Df,
              ", ",
              den.Df,
              ") = ",
              F,
              ", ",
              p_txt(Pr..F.),
              ", ges = ",
              round(ges,2),
              ", $\eta^{2}_p$ = ",
              round(Eta_Sq_partial,2),
              ", 95% CI [",
              round(CI_low,2),
              ", ",
              round(CI_high,2),
              "]",
              sep = "")

      )



     txt<-txt[txt$Parameter == effect,]

     return(txt)
}

# https://csrgxtu.github.io/2015/03/20/Writing-Mathematic-Fomulars-in-Markdown/
