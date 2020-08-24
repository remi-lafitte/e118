


anova_txt <- function(dv, iv_inter, df){
  library("supernova")
  library(dplyr)
  library(broom)
  fit<-as.formula(paste(dv, "~" ,iv_inter))
  model<-lm(fit, df)
  anov<-supernova(model)
  df_error<-  anov$tbl$df[1:2]
  anov$tbl$txt<-
      paste("(*F*(",
            anov$tbl$df,
            ", ",
            df_error[2],
            ") = ",
            round(  anov$tbl$F,2),
            ", *p* < ",
            round(  anov$tbl$p,6),
            ", PRE = ",
            round(    anov$tbl$PRE,2),
            ")",
            sep = "")
slope<-broom::tidy(model, conf.int = T, conf.level = 0.95)
slope$txt<-paste("*B* = ",
                 round(slope$estimate,2),
                 ", [",
                 round(slope$conf.low,2),
                 ", ",
                 round(slope$conf.high,2),
                 "]", sep = "")
slope<-slope[, c("term", "txt")]

  return(list(anova = anov$tbl,slope = slope))
}
# m<-anova_txt(dv = "mpg", iv_inter = "vs", df = mtcars)
View(m)
anova_txt(dv = "mpg", iv_inter = "1", df = mtcars)

m$anova$txt[1]

data(mtcars)
mc<-lm(mpg~1,mtcars)
ma<-lm(mpg~vs,mtcars)
ma2<-lm(mpg~vs +disp ,mtcars)
ma3<-lm(mpg~1 + vs +disp ,mtcars)

anova(mc,ma)

c<-supernova(ma)
View(c)
var(mtcars$mpg)


supernova(ma)
supernova(ma2)
supernova(ma3,type = 3)
supernova(ma3)

 m0<-lm(mpg ~ 0, mtcars)
 summary(lm(mpg ~ 0, mtcars))
 # 20.95
 summary(lm(mpg ~ 1, mtcars))
 # 6.027
anova(m0,mc)
# The 0 + suppresses the fitting of the intercept by lm.
# edit To plot the fit, use
lm(ma) %>% summary()
lm(m0) %>% summary()

supernova(mc)
summary(ma)
anova(ma)
a<-sum ((mtcars$mpg -0 )^2)
b<-sum ((mtcars$mpg -mean(mtcars$mpg) )^2)
(a-b)/a
?supernova
