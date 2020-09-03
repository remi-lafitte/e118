#' A function for slackers who don't love writing anova outputs by hand
#'
#' This function allows you to express ANOVA output in plain text.
#' see =
#' https://tysonbarrett.com/jekyll/update/2018/03/14/afex_anova/
#' http://www.dwoll.de/r/ssTypes.php
#' @param model_aov_4 = a model fitted with affex::aov_4
#' @keywords aov_4
#' @export
#' @examples
#' example 1 =
#'library(afex)
#'library(magrittr)
#'library(dplyr)
#'library(tibble)
#'library(tidyr)
#' data(mtcars)
#' data<-mtcars %>% mutate(ID = 1:nrow(.))
#' model<-afex::aov_4(mpg~ vs * am + (1|ID), data) # between-subject design
#' result<-anova_txt(model = model)
#' result$anova$txt
#'
#' example 2 =
# data(mtcars)
# data<-mtcars %>% mutate(ID = rep(1:16, 2), intra = rep(c(0,1), each=16))
# model<-afex::aov_4(mpg~ 1 + (intra|ID), data) # within-subject design
# result<-anova_txt(model = model)
# result$anova$txt



anova_txt<- function(model_aov_4){ # enter afex (aov_4) object here
    library(afex)
    library(effectsize)
    library(dplyr)
    library(magrittr)
   pes<-
    effectsize::eta_squared(model, partial = TRUE, ci = 0.9) %>%
    data.frame %>%
    mutate_if(is.numeric, round, digits=3)


    table<-
      model$anova_table %>%
      data.frame %>%
      rownames_to_column("Parameter") %>%
      mutate_if(is.numeric, round, digits=2) %>%
      inner_join(., pes, by = "Parameter")


     table<- table %>%
      mutate(
        txt= paste("*F*(",
              num.Df,
              ", ",
              den.Df,
              ") = ",
              F,
              ", *p* < ",
              Pr..F.,
              ", ges = ",
              ges,
              ", pes = ",
              Eta_Sq_partial,
              ", 95% CI [",
              CI_low,
              ", ",
              CI_high,
              "]",
              sep = "")

      )
     return(list(anova = table))
}

