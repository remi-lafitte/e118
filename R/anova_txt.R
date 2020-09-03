 # https://tysonbarrett.com/jekyll/update/2018/03/14/afex_anova/
# http://www.dwoll.de/r/ssTypes.php

anova_txt<- function(model){ # enter afex (aov_4) object here
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

# txt<-anova_txt(anova_1)
# txt$anova$txt[line,]
#
#
# set.seed(42)
# z <- data.frame(a1 = c(rnorm(100,2), rnorm(100,1),rnorm(100,0)),
#                 b  = rep(c("A", "B", "C"), each = 100),
#                 c  = factor(rbinom(300, 1, .5)),
#                 ID = 1:300,
#                 a2 = c(rnorm(100,2), rnorm(100,1),rnorm(100,0)),
#                 a3 = c(rnorm(100,2), rnorm(100,1),rnorm(100,0)))
# str(z)
# library("lsmeans")
# library("afex")
# library(tidyverse)
#
# aov1 <- z %>%
#   aov_car(a1 ~ b + Error(ID),
#           data = ., eff_size())
# aov1
#
# aov1$aov %>%
#   plot()
#
# aov1 %>%
#   lsmeans(specs = "b") %>%
#   data.frame %>%
#   ggplot(aes(b, lsmean)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL))
#
# aov1 %>%
#   lsmeans(specs = "b") %>%
#   pairs() %>%
#   update(by=NULL, adjust = "holm")
#
# z %>%
#   aov_ez(id = "ID",
#          dv = "a1",
#          between = "b",
#          data = .)
#
# z %>%
#   aov_4(a1 ~ b + (1|ID),
#         data = .)
#
# #factorial
# aov2 <- z %>%
#   aov_4(a1 ~ b * c + (1|ID),
#         data = .)
#
# aov2
# aov2$aov %>%
#   plot()
#
# w<-mtcars %>%rowid_to_column("ID") %>%
#   mutate(qsec=qsec - mean(qsec)) %>%
#   mutate(vs=vs - mean(vs))
#
# ma<-  aov_4(mpg ~ qsec * vs  + (1|ID),
#         factorize = FALSE ,
#         data = w,
#          anova_table = list(es = c("pes", "ges"))
#         )
#   ma<-lm(mpg ~ vs*qsec, w)
#   mc<-lm(mpg ~   vs+qsec, w)
#   anova(mc, ma)
#   library(effectsize)
#   effectsize::eta_squared(ma, partial = TRUE, ci = 0.9)
#   effectsize::cohens_f(ma, partial = TRUE, ci = 0.9)
#
# aov2 %>%
#   lsmeans(specs = c("b", "c")) %>%
#   data.frame %>%
#   ggplot(aes(b, lsmean, group = c, color = c)) +
#   geom_point(position = position_dodge(width = .3)) +
#   geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
#                 position = position_dodge(width = .3),
#                 width = .2)
# # Idea = créer une liste gplot de couches esthétiques.
# #within
# z_long <- z %>%
#   tidyr::gather("meas", "value", a1, a2, a3)
# str(z_long)
# table(z_long$meas)
# aov_rm <- z_long %>%
#   aov_car(value ~ 1 + Error(ID/meas),
#           data = .)
# aov_rm
#
# aov_rm <- z_long %>%
#   aov_4(value ~ 1 + (meas|ID),
#         data = .)
# aov_rm
# library(lme4)
# anova(lmer(value ~  meas + (1|ID), z_long), type =2)
# anova(lmer(value ~  meas + (1|ID), z_long), type =3)
#
#
#
# mtcars
# ma<-lm(mpg ~  qsec + vs, mtcars)
# mc<-lm(mpg ~  vs, mtcars)
# anova(ma,mc)
# anova(lm(mpg ~  vs + qsec, mtcars))
#
# aov_rm %>%
#   lsmeans(specs = "meas") %>%
#   data.frame %>%
#   ggplot(aes(meas, lsmean)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL))
#
# # Mixed model
# mixed_mod <- z_long %>%
#   aov_4(value ~ b + c + (meas|ID),
#         data = .)
# mixed_mod
# mixed_mod %>%
#   lsmeans(specs = c("meas", "b", "c")) %>%
#   data.frame %>%
#   ggplot(aes(meas, lsmean, group = b, color = b)) +
#   geom_point() +
#   geom_line() +
#   geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
#                 width = .2) +
#   facet_grid(~c)
#
#
#
#
#
#
#
#
# # To report this analysis, researchers could write: “Participants reported
# # higher evaluations for Movie 1 (M = 8.7, SD = 0.82) than Movie 2 (M = 7.7,
# # SD = 0.95), F(1, 9) = 22.50, p = 0.001, η2p = 0.71, 90% CI [0.31, 0.82],
# # η2G = 0.26.” Note that I've chosen to report both partial eta squared
# # (including the 90% confidence interval, using the scripts provided by
# # Smithson, 2001) as generalized eta squared. By providing η2p, researchers
# # can perform a-priori power analyses, and by providing η2G, researchers can
# # easily include the study in a future meta-analysis that compares effects
# # across different designs (see Olejnik and Algina, 2003). Providing two
# # effect sizes is in line with the suggestion that reporting multiple effect
# # sizes can yield a greater understanding of a specific effect (Preacher and
# # Kelley, 2011).
