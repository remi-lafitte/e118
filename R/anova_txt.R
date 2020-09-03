
anova_txt<- function(model_aov_4){
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

