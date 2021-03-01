# https://stackoverflow.com/questions/14219887/how-to-delete-a-file-with-r
# https://blog.revolutionanalytics.com/2009/01/10-tips-for-making-your-r-graphics-look-their-best.html
# https://stats.stackexchange.com/questions/313471/always-use-welch-t-test-unequal-variances-t-test-instead-of-student-t-or-mann
# https://stackoverflow.com/questions/29583849/save-a-plot-in-an-object
# https://stats.stackexchange.com/questions/59879/logistic-regression-anova-chi-square-test-vs-significance-of-coefficients-ano
# dv<-varcateg



# matplot(1:3, t( df[ ,3:5] ), type="l")

# data<-dat
# iv<-"group"
# formula<-"age ~group"
# mydiag(formula = formula, data = data)
# https://stats.stackexchange.com/questions/244647/how-to-use-analysis-of-deviance-to-test-parameters
# my_small_diag_lmer<-function(dv = NULL,iv=NULL, data =NULL, name = "Residuals of "){
#   formula <- as.formula(paste0(dv, "~", iv,"+ (1|ID)"))
#   tmp<-lmer(formula, data)
#   plot.new()
#   par(mfrow=c(1,2))
#   print(plot(tmp, which=2))
#   print(plot(tmp, which=3))
#   title(paste0(name, paste0(dv, "~", iv)), line = -2, outer = TRUE)
#   plots<-recordPlot()
#   
#   time <- Sys.time()
#   time2<-paste0(substr(time, 12,13), substr(time, 15,16))
#   
#   name2 <-paste0(paste0(name, paste0(dv, "~", iv)),"-", time2, ".png")
#   print(name2)
#   plot.new()
#   png(filename= here::here("output", "PLOT", name2), width = 800)
#   print(plots)
#   dev.off()
#   myappend2("```{r}")
#   myappend2(paste0("x<-'",here::here('output', 'PLOT', name2),"'"))
#   path_rmd<-paste0("knitr::include_graphics(x)")
#   myappend2(path_rmd)
#   myappend2("```") 
# }
# dv = "margin"
# data<-iccR
myIRR<-function(data =NULL, dv = NULL, icc = "ICC2", name =""){# long format with S J and multiple DV
    outme<-list()
    for(i in dv){
      tmp<-data %>% 
        select(S,J, !!i) %>%
        spread(J, !!i) %>% select(-S) %>% 
        na.omit()
      res<-psych::ICC(tmp,lmer=F)

      tmp2<- res$results %>% as.data.frame() %>% 
        dplyr::filter(type == icc) %>% 
        mutate(resid = res$stats[3,3],
               var_inter_judges = res$stats[3,2]) %>% 
        mutate(sem = sqrt(resid+var_inter_judges)) %>% 
        mutate(mdc_95 = sem*1.96*sqrt(2))
      tmp2[,-c(1,8)]<-round(tmp2[,-c(1,8)],3)
      tmp2$p<-p_txt(tmp2$p)
      tmp2$Measure <- i
      outme[[i]] <-tmp2
    }
    tmp3<-bind_rows(outme)
    save_table(tmp3, paste0("IRR_", name))
    return(tmp3)
  }
  
# MPD<-here::here("DIAGNOSTIC_STAT.Rmd")# MY PATH of DIAGNOSTIC
my_small_diag<-function(dv = NULL,iv=NULL, data =NULL, name = "Residuals of "){
  formula <- as.formula(paste0(dv, "~", iv))
  tmp<-lm(formula, data)
  plot.new()
  library(ggfortify)
  p<-autoplot(tmp, which = c(2,3))+theme_bw(base_size = 18)
  # title(paste0(name, paste0(dv, "~", iv)), line = -2, outer = TRUE)
  # par(mfrow=c(1,2))
  # print(plot(tmp, which=2))
  # print(plot(tmp, which=3))
  # plots<-recordPlot()
  time <- Sys.time()
  time2<-paste0(substr(time, 12,13), substr(time, 15,16))
  name2 <-paste0(paste0(name, paste0(dv, "~", iv)),"-", time2, ".png")
  # print(name2)
  # plot.new()
  png(filename= here::here("output", "PLOT", name2), width = 800)
  print(p)
  dev.off()
  myappend2(" ", file = MPD)
  myappend2(paste0("#### ",paste0(dv, "~", iv)),
                   file = MPD)
  myappend2(" ", file = MPD)
  myappend2("```{r}", file = MPD)
  myappend2(paste0("x<-'",here::here('output', 'PLOT', name2),"'"), file = MPD)
  path_rmd<-paste0("knitr::include_graphics(x)")
  myappend2(path_rmd, file = MPD)
  myappend2("```", file = MPD) 
  print(p)
}

mydiag<-function(data = data, iv = NULL, dv = NULL, name = NULL){
  for(i in dv){
formula <- as.formula(paste0(i, "~", iv))
print(formula)
tmp<-lm(formula=formula,data=data)
library(ggfortify)
p1<-autoplot(tmp)+theme_bw(base_size = 12)
resid<-data.frame(res = tmp$residuals)
p2<-ggplot(data = resid, aes(x = tmp$residuals))+
  geom_histogram(fill = "darkgrey", col = "black",
                 binwidth = 2)+
  labs(x = paste0("Residuals of ", paste0(i, "~", iv)))+th
# p3<-plotInfluence(tmp)
library(patchwork)
pfinal<-p1+p2
time <- Sys.time()
time2<-paste0(substr(time, 3,4), substr(time, 6,7),substr(time, 9,10),"-",
              substr(time, 12,13), substr(time, 15,16))
name2 <-paste0(i,"~", iv,"-",name, "-", time2, ".png")
print(name2)
ggsave(plot=pfinal, filename= here::here("output", "PLOT", name2))

myappend2(" ", file = MPD)
myappend2(paste0("### ",iv, "~", i),file = MPD)
myappend2(" ", file = MPD)
myappend2("```{r, fig.cap = 'Diagnostic plots for linear regression.', out.width = '70%'}",
          file = MPD)
myappend2(paste0("x<-'",here::here('output', 'PLOT', name2),"'"),file = MPD)
path_rmd<-paste0("knitr::include_graphics(x)")
myappend2(path_rmd,file = MPD)
myappend2("```",file = MPD)
  }
}

myor<-function(model){
  x<- cbind(round(exp(coef(model)),2),round(exp(confint(model)),2))
  y<-as.data.frame(x)
  colnames(y)<-c("Odd_Ratios", "95%LL", "95%UL")
  # z<-y %>% mutate(txt = paste0("The OR of ",row.names(.),
  #                              " was ", or,"[",ll,";",ul,"]"))
 save_table(y, name = "Odd_Ratios_LR")
}

mylogistic_univ <- function(data = NULL, iv = NULL, dv = NULL, name = "",
                            caption = ""){
  output<-list()
  txt<-list()
  
  formula_null <- as.formula(paste0(dv, "~", 1))
 
  for(k in iv){
    formula <- as.formula(paste0(dv, "~1+", k))
    reg<-data[,c(k,dv)] %>% na.omit()
    mc <- glm(formula_null,data=reg,family = binomial(link="logit"),
              na.action = na.exclude)
    ma<- glm(formula,data=reg,family = binomial(link="logit"),
             na.action = na.exclude)
    beta<-summary(ma)$coefficients
    beta[,-4]<-round( beta[,-4], 2)
    beta[,4]<-round( beta[,4], 4)
    colnames(beta)<-c("Estimate", "SE", "Wald_z", "p")
    Dev<-anova(mc, ma, test = "Chisq") %>% as.data.frame() %>% 
      mutate(Deviance = round(Deviance,2)) %>% 
      select(Deviance,`Resid. Dev`) %>%
      mutate(PRD = round(Deviance/`Resid. Dev`[1],2)) %>% 
      mutate(IV  = c("Intercept", k))
    Dev[is.na(Dev)]<-""
    tmp<-cbind(beta,Dev)
    tmp$pR2 <-c("", round((1 - ma$deviance / ma$null.deviance),2)) # works for glm
    tmp[,6]<-round(tmp[,6],2)
    sep<-rep("====", each = length(tmp))
    tmp2<-rbind(tmp, sep)
    tmp3<-tmp2[,c(8,1:4,6,5,7,9)]
    tmp4<-paste0("$\\\\chi^2$","(1) = ",tmp3$Deviance[2],", ", 
                 p_txt(tmp3$p[2]))
    output[[k]]<-as.data.frame(tmp3)
    txt[[k]]<-tmp4
  }
  tmp5<-bind_rows(output)
  row.names(tmp5)<-NULL
  tmp6<-bind_rows(txt)
  tmp7<-paste0(as_vector(tmp6), collapse = "; ")
  
  fn<-paste0("Wald_",name)
  save_table(as.data.frame(tmp5), name = fn, caption = caption)
  }

mylogistic_multiv <- function(data = NULL, iv = NULL, dv = NULL, name = "",
                              caption = ""){
  output<-list()
  output2<-list()
  txt<-list()
  formula <- as.formula(paste0(dv, "~1+", iv))
  formula_null <- as.formula(paste0(dv, "~", 1))
  mc <- glm(formula_null,data=data,family = binomial(link="logit"))# Gives null deviance
  ma<- glm(formula,data=data,family = binomial(link="logit"),
           na.action = na.exclude)
  for(k in iv){
    beta<-summary(ma)$coefficients
    beta[,-4]<-round( beta[,-4], 2)
    beta[,4]<-round( beta[,4], 4)
    colnames(beta)<-c("Estimate", "SE", "Wald_z", "p")
     
     output[[k]]<-as.data.frame(beta)
     # txt[[i]]<-inline
  }
  tmp2<-bind_rows(output)
  
  IVS<-rownames(tmp2)[-1]
    for(j in  seq(1:length(IVS))){
      pred <-paste0(paste0(IVS[-j], "+"), collapse = "")
      pred2<-substr(pred, 1, nchar(pred)-1)
      tmpf <- as.formula(paste0(dv, "~", pred2))
      mn<- glm(tmpf,data=data,family = binomial(link="logit"),
               na.action = na.exclude)
      Dev<-anova(mn, ma, test = "Chisq") %>% as.data.frame() %>% 
        mutate(Deviance = round(Deviance,2)) %>% 
        select(Deviance,`Resid. Dev`) %>%
        mutate(PRD = paste0(round(Deviance/`Resid. Dev`[1],2)*100,"%")) %>% 
        mutate(IV  = IVS[j])
      output2[[j]]<-Dev[2,-2]
    }
     
    tmp3<-bind_rows(output2)
    firstrow<-c(NA,NA,NA,NA)
    tmp3<-rbind(firstrow,tmp3)
    
    tmp4<-cbind(tmp2,tmp3)
    sep<-rep("====", each = length(tmp4))
    tmp5<-rbind(tmp4, sep)
    tmp5[is.na(tmp5)]<-""
    tmp5<-tmp5[c("IV", setdiff(names(tmp5), "IV"))]
    tmp5[1,1]<-"Intercept"
    
    pR2<-round((1 - ma$deviance / ma$null.deviance),2)
    rowpR2<-c("pR2 = ", pR2, rep("", each = length(tmp5)-2))
    tmp6<-rbind(tmp5, rowpR2)
    row.names(tmp6)<-NULL
  
    tmp7<-paste0("$\\\\chi^2$","(1) = ",tmp5$Deviance,", ", 
         sapply(tmp5$p, p_txt))
    tmp7<-paste0(as_vector(tmp6), collapse = "; ")

  fn<-paste0("Wald_",name)
  save_table(as.data.frame(tmp6), name = fn, caption = caption)
}


mywilcoxon <- function(data = NULL, iv = NULL, dv = NULL, name = ""){
  output<-list()
  txt<-list()
  for(i in dv){
    my_small_diag(dv = i, iv=iv, data = data)
    formula <- as.formula(paste0(i, "~", iv)) 
    tmp<-rstatix::wilcox_test(formula =formula , data = data, detailed = T) %>% 
      select(-c(9:12))
    tmp$R<-rstatix::wilcox_effsize(data=data, formula=formula) %>% select(effsize) %>% 
      round(.,3) %>% as_vector()
    tmp$Z <-round(qnorm(tmp$p/2),2)
    
    inline<-paste0("Z = ",tmp$Z,", ",p_txt(tmp$p),
                   ", r = ",tmp$R)
    colnames(tmp)[c(1,2)]<-c("Diff", "DV")
    tmp<-tmp[c("DV", setdiff(names(tmp), "DV"))]
    
    output[[i]]<-tmp
    txt[[i]]<-inline
  }
  tmp2<-bind_rows(output)
  tmp2<-tmp2 %>% arrange(p)
  tmp3<-paste0(as_vector(bind_rows(txt)), collapse = "; ")
  fn<-paste0("Wilcox_Ind_",name)
  save_table(tmp2, name = fn, caption = tmp3)
}

mycor<-function(vars = NULL, data = NULL, method = "pearson",
                use="na.or.complete", name = ""){
   tmp<-rstatix:::cor_test(vars = var, data = data, 
                     method = method,
                     use=use) %>% 
  filter(statistic != Inf) %>% filter(cor != 1) %>% 
  arrange(p) %>% 
  distinct(p,.keep_all= TRUE)
   tmp$statistic<-round(tmp$statistic,2)
   tmp$conf.low<-round(tmp$conf.low,2)
   tmp$conf.high<-round(tmp$conf.high,2)
r<-ifelse(method == "pearson","*r*", "*r~s~*")
tmp$APA<-paste0(r," = ",round(tmp$cor,2),"[", round(tmp$conf.low,2),";",round(tmp$conf.high,2),"]",
                ", ", p_txt(tmp$p),
               ", *R^2^* = ",paste0(100*round(tmp$cor*tmp$cor,2),"%"))
time <- Sys.time()
time2<-paste0(substr(time, 3,4), substr(time, 6,7),substr(time, 9,10),"-",
              substr(time, 12,13), substr(time, 15,16))
fn <-paste0("Corr-", name,"-", time2)
save_table(tmp, name = fn)
   }
 

mystudent <- function(data = NULL, iv = NULL, dv = NULL, name = "",
                      var.equal = F){
  output<-list()
  txt<-list()
  for(i in dv){
    my_small_diag(dv = i, iv=iv, data = data)
    formula <- as.formula(paste0(i, "~", iv))
    tmp<-rstatix::t_test(formula =formula , data = data, var.equal = var.equal) 
    tmp$d<-rstatix:::cohens_d(data,formula) %>% select(effsize) %>% 
      round(.,3) %>% as_vector()
    tmp$statistic<-round(tmp$statistic,2)
    
    inline<-paste0("*t*","(",round(tmp$df,1),") = ",tmp$statistic,", ", p_txt(tmp$p),
                   ", *d* = ",tmp$d)
    tmp<-tmp %>% 
      rename(DV = ".y.", Cohens_d = "d")

    output[[i]]<-tmp
    txt[[i]]<-inline
  }
  tmp2<-bind_rows(output)
  tmp2<-tmp2 %>% arrange(p)
  tmp3<-paste0(as_vector(bind_rows(txt)), collapse = "; ")
  fn<-paste0("Stud_T_Ind_",name)
  save_table(tmp2, name = fn, caption = tmp3)
}
mystudent_paired <- function(data = NULL, iv = NULL, dv = NULL, name = ""){

  output<-list()
  txt<-list()
  for(i in dv){
    formula <- as.formula(paste0(i, "~", iv))
    tmp<-rstatix::pairwise_t_test(
      formula=formula,data = data, paired = TRUE,detailed = T) %>% 
      mutate(dz = round(statistic/sqrt(n1),3)) %>% 
      select(estimate,dz, group1,group2,n1,n2,statistic,df,p,p.adj.signif)

    tmp$statistic<-round(tmp$statistic,2)
    tmp$estimate <-round(tmp$estimate,2)
    inline<-paste0("*t*(",tmp$df,") = ",tmp$statistic,", ", p_txt(tmp$p),
                   ", *d~z~* = ",tmp$dz)
    colnames(tmp)[1:4]<-c("Diff", "Cohen_dz", "Time1", "Time2")
    output[[i]]<-tmp
    txt[[i]]<-inline
  }
  tmp2<-bind_rows(output)
  tmp3<-paste0(as_vector(bind_rows(txt)), collapse = "; ")
  fn<-paste0("Stud_T_paired_",name)
  save_table(tmp2, name = fn, caption = tmp3)
}

mystudent_zero <- function(data = NULL, dv = NULL, name = ""){
  
  output<-list()
  txt<-list()
  for(i in dv){
    my_small_diag(dv = i, iv= "1", data = data)
    formula <- as.formula(paste0(i, "~ 1"))
    tmp<-rstatix::t_test(
      formula=formula,data = data,detailed = T) %>% 
      mutate(dz = round(statistic/sqrt(n),3)) %>% 
      select(estimate,dz, n,statistic,df,p)
    
    tmp$statistic<-round(tmp$statistic,2)
    tmp$estimate <-round(tmp$estimate,2)
    inline<-paste0("*t*(",tmp$df,") = ",tmp$statistic,", ", p_txt(tmp$p),
                   ", *d~z~* = ",tmp$dz)
    colnames(tmp)[1:2]<-c("Diff", "Cohen_dz")
    tmp$DV<-i
    tmp<-tmp[c("DV", setdiff(names(tmp), "DV"))]
    output[[i]]<-tmp
    txt[[i]]<-inline
  }
  tmp2<-bind_rows(output)
  tmp3<-paste0(as_vector(bind_rows(txt)), collapse = "; ")
  fn<-paste0("Stud_T_Zero_",name)
  save_table(tmp2, name = fn, caption = tmp3)
}


mychi2 <- function(data = NULL, iv = NULL, dv = NULL, name = ""){
  data<-as.data.frame(data)
  output<-list()
  txt<-list()
  for(i in dv){
    tab<-table(data[,iv], data[,i])
    # tab[tab == 0] <- 0.5
    tmp<-rstatix:::chisq_test(tab) %>%
      mutate(Cramer_V =rstatix:::cramer_v(tab) %>% round(.,2)) %>% 
      mutate(IV = iv, DV = i)
    tmp$statistic<-round(tmp$statistic,2)
    inline<-paste0("$\\\\chi^2$","(",tmp$df,") = ",tmp$statistic,", ", p_txt(tmp$p))
    
    output[[i]]<-tmp
    txt[[i]]<-inline
  }
  tmp2<-bind_rows(output)
  tmp2<-tmp2 %>% arrange(p)
  tmp3<-paste0(bind_rows(txt), collapse = "; ")
  fn<-paste0("Chi2_",name)
  save_table(tmp2, name = fn, caption = tmp3)
}


stat_desc_categ_biv <- function(data = NULL,dv =  NULL, iv =  NULL, name = ""){
  output<-list()
 data<-as.data.frame(data)
  for(i in dv){
    data[,i] <-as.character(data[,i])
    data[,iv] <-as.character(data[,iv])
    
   tmp<- table(data[,i], data[,iv]) %>% 
      as.data.frame.matrix() %>% 
      rownames_to_column("DV_Levels") %>% 
      gather(IV_Levels, freq, - DV_Levels) %>% 
      mutate(DV = i) %>% 
      mutate(IV = iv) %>% group_by(IV_Levels) %>% 
      mutate(N = sum(freq)) %>% 
      mutate(Perc = paste0(round(freq/N*100,1),"%")) %>% 
     ungroup()
   
    tmp2<-tmp[,c(4,5,1,2,3,7,6)]
    output[[i]]<-tmp2
  }
  tmp3<-bind_rows(output)
  time <- Sys.time()
  time2<-paste0(substr(time, 3,4), substr(time, 6,7),substr(time, 9,10),"-",
                substr(time, 12,13), substr(time, 15,16))
  name <-paste0(name,"desc-", time2, ".csv")
  path<-here::here("output", "DESCRIPTIVE_STAT",name)
  write.csv2(tmp3,file = path, row.names = F)
  myappend2("```{r}")
  myappend2(paste0("x<-read.csv2(here::here('output', 'DESCRIPTIVE_STAT','",
                   name,"'),","header=T)"))
  myappend2("knitr::kable(x, row.names = F, caption = 'Descriptive statistics')")
  myappend2("```")
  return(tmp3)
}


mybx<-function(dv = NULL, data = NULL){
for(v in dv){
  lv<-length(v)
  ave1<-list()
  sd1<-list()
  ci1<-list()
  tmp<-as.matrix(data[,v])
  colnames(tmp)<-v
  for(i in 1:ncol(tmp)){
    ave1[[i]]<-mean(tmp[,i], na.rm = T)
    sd1[[i]]<-sd(tmp[,i], na.rm = T)
    ci1[[i]]<-sd(tmp[,i], na.rm = T)/sqrt(length(tmp[,i]))*1.96
  }
  ave<-as_vector(ave1)
  sd<-as_vector(sd1)
  ci<-as_vector(ci1)

  # se<-apply(data[,v],2,function(x) sd(x, na.rm = T)/sqrt(length(x)))
  # ci<-apply(data[,v],2,function(x) (sd(x, na.rm = T)/sqrt(length(x)))*1.96)
  boxplot(data[,v])
  arrows(c(1:lv)-0.1, ave-sd,c(1:lv)-0.1, ave+sd, length=0.06, angle=90, code=3, col = "blue")
  # arrows(c(1:lv)-0.4, ave-ci,c(1:lv)-0.4, ave+ci, length=0.06, angle=90, code=3, col = "blue")
  segments(c(1:lv)-0.4, ave-ci,c(1:lv)-0.4, ave+ci,col = "blue",lwd=8)
  
  # arrows(c(1:3)-0.3, ave-se,c(1:3)-0.3, ave+se, length=0.08, angle=90, code=3, col = "blue")
  points(c(1:lv)-0.1, ave, pch = 21, col = "blue", bg = "red", cex = 1)
  # lines(c(1:lv)-0.1, ave, col = "red", pch = 16)
  abline(h=0, col="grey", lty = "dashed")
  ifelse(ncol(as.matrix(data[,v])) == 1, axis(1,1, v), print(""))

  p<-recordPlot()
  print(p)
  save_plot(p, name = paste0("Boxplot_", paste0(v, collapse="")))
}
}

stat_desc_cont_biv <- function(data = NULL,dv =  NULL, iv =  NULL, name = ""){
  output<-list()
  for(i in dv){
    tmp<- aggregate(as.formula(paste0(i, "~" ,iv)),data = data,
                    FUN = function(x) c(mean=round(mean(x),2), 
                                        sd=round(sd(x),2), 
                                        med = round(median(x),2),
                                        round(quantile(x, c(0.25,0.75)),2),
                                        round(IQR(x),2),
                                        round(range(x),2),
                                        n = length(x),
                                        missing = round(sum(is.na(x)),0)))
    tmp2<-cbind(tmp[-ncol(tmp)], tmp[[ncol(tmp)]])
    tmp2$missing<-sum(is.na(data[,i]))
    tmp2$N <- length(data[,i])
    tmp2$DV<-i
    colnames(tmp2)[-1]<-c("Mean", "SD", "Med", "Q1", "Q3", "IQR", "Min", "Max",
                      "n", "Missing","N", "DV")
    output[[i]]<-tmp2
  }
  tmp3<-bind_rows(output)
  tmp4<-tmp3[c("DV", setdiff(names(tmp3), "DV"))]
  time <- Sys.time()
  time2<-paste0(substr(time, 3,4), substr(time, 6,7),substr(time, 9,10),"-",
                substr(time, 12,13), substr(time, 15,16))
  name <-paste0(name, "desc-", time2, ".csv")
  path<-here::here("output", "DESCRIPTIVE_STAT",name)
  write.csv2(tmp4,file = path, row.names = F)
  myappend2("```{r}")
  myappend2(paste0("x<-read.csv2(here::here('output', 'DESCRIPTIVE_STAT','",
                   name,"'),","header=T)"))
  myappend2("knitr::kable(x, row.names = F, caption = 'Descriptive statistics')")
  myappend2("```")
  return(tmp4)
}


stat_desc_categ <- function(data = NULL, name = ""){
  # Data is a data frame where one column is on DV.
  output<-list()
  for(i in colnames(data)){
    data[,i]<-as.character(data[,i])
     tmp<-
      data[,i] %>% 
      na.omit() %>% 
      tabyl() %>% 
      adorn_pct_formatting(digits = 1) %>% 
      mutate(DV = i) %>% 
      mutate(N = length(data[,i])) %>% 
      mutate(missing = sum(is.na(data[,i]))) 
    colnames(tmp)<-c("Levels", "Freq", "Perc", "DV", "n", "Missing")
    tmp2<-tmp[,c(4,1,2,3,5,6)]
    output[[i]]<-tmp2
  }
  tmp3<-bind_rows(output)
  time <- Sys.time()
  time2<-paste0(substr(time, 3,4), substr(time, 6,7),substr(time, 9,10),"-",
                substr(time, 12,13), substr(time, 15,16))
  name <-paste0(name,"desc-", time2, ".csv")
  path<-here::here("output", "DESCRIPTIVE_STAT",name)
  write.csv2(tmp3,file = path, row.names = F)
  myappend2("```{r}")
  myappend2(paste0("x<-read.csv2(here::here('output', 'DESCRIPTIVE_STAT','",
                   name,"'),","header=T)"))
  myappend2("knitr::kable(x, row.names = F, caption = 'Descriptive statistics')")
  myappend2("```")
  return(tmp3)
}



stat_desc_cont <- function(data =  NULL, name = ""){
  # Data is a data frame where one column is on DV.
  output<-list()
  for(i in colnames(data)){
    tmp<- aggregate(as.formula(paste0(i, "~" ,1)),data = data,
                    FUN = function(x) c(mean=round(mean(x),2), 
                                        sd=round(sd(x),2), 
                                        med = round(median(x),2),
                                        round(quantile(x, c(0.25,0.75)),2),
                                        round(IQR(x),2),
                                        round(range(x),2),
                                        n = length(x),
                                        missing = round(sum(is.na(x)),0)))
    tmp2<-cbind(tmp[-ncol(tmp)], tmp[[ncol(tmp)]])
    tmp2$missing<-sum(is.na(data[,i]))
    tmp2$N <- length(data[,i])
    tmp2$DV<-i
    colnames(tmp2)<-c("Mean", "SD", "Med", "Q1", "Q3", "IQR", "Min", "Max",
                      "n", "Missing","N", "DV")
    output[[i]]<-tmp2
  }
  tmp3<-bind_rows(output)
  tmp4<-tmp3[c("DV", setdiff(names(tmp3), "DV"))]
  time <- Sys.time()
  time2<-paste0(substr(time, 3,4), substr(time, 6,7),substr(time, 9,10),"-",
         substr(time, 12,13), substr(time, 15,16))
  name <-paste0(name, "desc-", time2, ".csv")
  path<-here::here("output", "DESCRIPTIVE_STAT",name)
  write.csv2(tmp4,file = path, row.names = F)
  myappend2("```{r}")
  myappend2(paste0("x<-read.csv2(here::here('output', 'DESCRIPTIVE_STAT','",
                   name,"'),","header=T)"))
  myappend2("knitr::kable(x, row.names = F, caption = 'Descriptive statistics')")
  myappend2("```")
  return(tmp4)
}
# outme<-function(txt){return(here::here("output",txt))}
# data = d_rbd
# iv = "genre"
# dv ="margin"
ttme <-function(data = NULL, iv = "1", dv = NULL, note ="",
                var.equal = T){
  
  formula <- as.formula(paste0(dv, "~", iv))
  name<-paste0("ttme_",paste0(dv, "~", iv),note)
    desc<-
    aggregate(formula, data, 
            function(x) c(mean=round(mean(x),2), 
                          sd=round(sd(x),2), 
                          med = median(x),
                          n = length(x),
                          missing = round(sum(is.na(x)),0)))
  mydesc<-cbind(desc[-ncol(desc)], desc[[ncol(desc)]])
 

  mylm<-lm(formula = formula, data = data)
  mytt<-t_test(data, formula, var.equal = var.equal)
 
  fn<-paste0(name, ".png")
  fnhere<-here::here("output", "PLOT", fn)
 
  plot.new()
  par(mfrow = c(2,3))
  plot(mylm, which = 1)
  plot(mylm, which = 2)
  plot(mylm, which = 3)
  plot(mylm, which = 4)
  print(hist(mylm$residuals))
  ifelse(iv == "1", boxplot(data[,dv],xlab = dv),boxplot(formula, data))
  points( desc[dv][1][[1]][,1],col="red")
  p <- recordPlot()
  plot.new()
  png(file = fnhere)
  p
  dev.off()
  path_for_plot<-paste0("![](output/PLOT/",fn,")")

  myres<-lm(sqrt(abs(mylm$residuals))~mylm$fitted.values)
  sum_myres<-summary(myres)
  p_myres<-sum_myres$coefficients %>% round(.,2)
  r_myres <-sqrt(sum_myres$r.squared) %>% round(.,2)
  # Medium effect = from .30

  mytxt <- tt_txt(model = t.test(formula, data))$full

  # Now we append all these results ! 
  myappend(paste0("## Independent T-test = ", dv, " by ",iv))
  myappend("### Description")
  tableme(mydesc)
  myappend("### Inference")
  tableme(mytt)
  tableme(mylm)
  myappend(mytxt)
  myappend("### Diagnostic plot")
  myappend(path_for_plot)
  myappend("Is the variance unequal between groups ?")
  tableme(myres)
  myappend(paste0("The correlation between the fitted values and the square root 
  of the absolute residuals is ", r_myres))
}

plotme<-function(plot= NULL, name = "coucou"){
  fn<-paste0(name, ".png")
  fnhere<-here::here("output", "PLOT", fn)
  png(file = fnhere)  
  path<-paste0("![](PLOT/",fn,")")
  myappend(path)
  print(plot)
  dev.off()
}

save_plot<-function(plot, name = "Test"){
  time <- Sys.time()
  time2<-paste0(substr(time, 3,4), substr(time, 6,7),substr(time, 9,10),"-",
                substr(time, 12,13), substr(time, 15,16))
  name2 <-paste0(name,"-", time2, ".png")
  print(name2)
  png(filename= here::here("output", "PLOT", name2), width = 800)
  print(plot)
  dev.off()
  print(plot)
  myappend2("```{r}")
  myappend2(paste0("x<-'",here::here('output', 'PLOT', name2),"'"))
  path_rmd<-paste0("knitr::include_graphics(x)")
  myappend2(path_rmd)
  myappend2("```") 
}

save_table<-function(table, name = "Test", caption = "Table"){
time <- Sys.time()
time2<-paste0(substr(time, 3,4), substr(time, 6,7),substr(time, 9,10),"-",
              substr(time, 12,13), substr(time, 15,16))
name <-paste0(name, time2, ".csv")
path<-here::here("output", "TABLE",name)
write.csv2(table,file = path, row.names = F)
myappend2("```{r}")
myappend2(paste0("x<-read.csv2(here::here('output', 'TABLE','",
                 name,"'),","header=T)"))
path_rmd<-paste0("knitr::kable(x, row.names = F, caption ='",caption,"')")
myappend2(path_rmd)
myappend2("```")
return(table)  
}


# tableme<-function(table = NULL, caption = NULL){
#   myappend2(txt = pander_return(table, 
#                                style = "rmarkdown",
#                                caption = caption))
# }
# oldtableme<-function(table= NULL, name = "coucou"){
#   fn<-paste0(name, ".csv")
#   fnhere<-here::here("output", "TABLE", fn)
#   write.table(table,file = fnhere, sep=";")  
#   myappend("```{r}")
#   myappend(paste0("x<-read.csv(here::here('output', 'TABLE','",fn,"'),",
#                   "header=T, sep = ';', dec ='.')"))
#   myappend("knitr::kable(x, row.names = F)")
#   myappend("```")
#   knitr::kable(table)
# }

w<-function(txt = NULL, file = MPI){
  write(txt,file=file,append=TRUE)
  # write(txt,file=MPD,append=TRUE)
}

myappend<-function(txt = NULL, file = MPI){
space = "\n"
out<-paste0(txt, space)
write(out,file=file,append=TRUE)
}

myappend2<-function(txt = NULL, file = MPI){
  out<-paste0(txt)
  write(out,file=file,append=TRUE)
}


mychione<-function(data = NULL, iv = NULL){
df<-
  data[,iv] %>% 
  tabyl(!!iv) %>% 
  adorn_pct_formatting(digits = 2) %>% 
  mutate(txt=paste0("Number of ",iv,"=",.[,1], " is ", n, "(", percent,"),or (n=",n,", ",percent,")"))
myappend(txt =paste0("The DV is '",iv,"'"))
myappend(txt = df$txt)
}
# mychione(data = d_rbd, iv = "csp_ctg")
# data = d_rbd
# iv = "genre"
# writeme<-function(object = "coucou", name = "coucou.txt"){
# fn<-paste0(name, ".txt")
# write.table(object, file = here::here("output", fn))  
# }


library(ggplot2)
th <- list(theme_bw(base_size = 18))

irr_apa<-function(data, dv){# long format with S J and vd
  sh<-data%>% 
    select(S,J, !!dv) %>%
    spread(J, !!dv) %>% select(-S) %>% 
    na.omit()
  rownames(sh) <- paste("S",1:nrow(sh),sep="")
  
  sf<-psych::ICC(sh,lmer=T)
  sf2<-sf$results %>% 
    select(type, ICC,'lower bound', 'upper bound', p) %>% 
    filter(type == "ICC2")
  sem<-sqrt(sf$lme[3,1])
  mdc<-sem*1.96*sqrt(2)
  
  sf2$out<-paste0("ICC=",nozero(digits=3,round(sf2$ICC,3)),
                  "[",nozero(digits=3,round(sf2$'lower bound',3)),",",
                  nozero(digits=3,round(sf2$'upper bound',3)),"],",p_txt(sf2$p),
                  ",SEM=",round(sem,2),",MDC=", round(mdc,2))
  myappend(paste0("Inter-rater reliability (ICC[2,1]) for **",dv,"** is =",
                  sf2$out))
  return(sf2$out)

}
# irr_apa(data = iccN, "margin")
# data = iccN
# dv = "margin"

msdy<-function(dv = NULL, iv = "1", data = NULL){
  formula <- as.formula(paste0(dv, "~", iv))
  ave<-aggregate(formula,data=data, FUN =mean)
  std<-aggregate(formula,data=data, FUN =sd)
  n<-aggregate(formula,data=data, FUN =length)
  out<-paste0(dv," by ",iv," is M=",round(ave,2),";SD=", round(std,2),";n=",n)
  myappend(out)
  print(out)
}
# msdy(dv = "tmean", iv = "group", data = d)

iqry<-function(dv = NULL, iv = "1", data = NULL){
  formula <- as.formula(paste0(dv, "~", iv))
  q1<-aggregate(formula,data=data, 
                 FUN =function (x) quantile(x, c(0.25)))
  q2<-aggregate(formula,data=data, 
                FUN =function (x) quantile(x, c(0.5)))
  q3<-aggregate(formula,data=data, 
                FUN =function (x) quantile(x, c(0.75)))
  n<-aggregate(formula,data=data, FUN =length)
  out<-paste0(dv," by ",iv," is Med=",q2,";Q1-Q3=",paste0(q1,q3,sep=";"),";n=",n)
  myappend(out)
  print(out)
}
#iqry(dv = "tmean", iv = "group", data = d)
ron<-function(x){
  ave<-round(mean(x),2)
  vari<-round(sd(x)*2,2)
  range <-c(ave - vari, ave + vari) 
  return(range)
  print(range)
}

ph_txt<-function(pair){
  tab<-pair
  tab<-tab %>% 
    group_by(statistic) %>% 
    mutate(contrast = paste0(group1,"_vs_",group2)) %>% 
    mutate(txt= paste0("*t*(1, ", round(df,2), ") = ", round(statistic,2), ", ", p_txt(p))) %>% 
    ungroup()
  list<-tab %>% select(contrast, txt)
  return(list)  
}

wtf_is <- function(x) {
  # For when you have no idea what something is.
  # https://stackoverflow.com/questions/8855589
  cat("1. typeof():\n")
  print(typeof(x))
  cat("\n2. class():\n")
  print(class(x))
  cat("\n3. mode():\n")
  print(mode(x))
  cat("\n4. names():\n")
  print(names(x))
  cat("\n5. slotNames():\n")
  print(slotNames(x))
  cat("\n6. attributes():\n")
  print(attributes(x))
  cat("\n7. str():\n")
  print(str(x))
}


mdc<-function(d, type){
  # d est une matrice du type code + rater 1 + rater 2 
  x1<- d %>% 
    mutate(Test_Score = mean(rater1), Retest_Score = mean(rater2)) %>% 
    mutate(s1 = sd(rater1)) %>% 
    mutate(w = rater1 - rater2, SD_diff = sd(w)) %>% 
    #mutate(SEM = SD_diff/sqrt(2)) %>% 
    dplyr::select(-code, - w) %>% 
    nest(., c(rater1, rater2)) %>% 
    dplyr::mutate(
      icc = purrr::map(data, ~ (psych::ICC(.x,
                                           lmer=T) %>% 
                                  .$results %>% .[c(type),]))
    ) %>% 
    unnest(icc) 
  vl<- names(x1) 
  colnames(x1)<-gsub("[ [:punct:]]", "" , vl)
  mdc<-x1 %>% 
    mutate(SEM = s1 * sqrt(1-ICC)) %>% 
    mutate(MDC = SEM * sqrt(2) * 1.96) %>% 
    mutate (ICC_95_CI = paste(round(ICC,2), " [",
                              round(lowerbound,2),", ",
                              round(upperbound,2),
                              "]", sep = "")) %>% 
    dplyr::select(-df2, - type, - data) %>% 
    mutate(p = round(p, 6)) %>%
    mutate(MDC = round(MDC, 3), 
           SEM = round(SEM, 3),  
           F = round(F,2), 
           'SD Rater A' = round(s1,2),
           'Rater A' = round(TestScore, 2), 
           'Rater B'= round(RetestScore, 2))  %>% 
    dplyr::select( TestScore, RetestScore,ICC_95_CI, MDC, SEM)
  
  
  return(mdc)
}
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
  rownames(txt)<- txt$term
  # list <- setNames(split(txt, seq(nrow(txt))), rownames(txt))
  # txt<-txt[txt$Parameter == effect,]
  
  return(txt)
}

aov4_txt<- function(model){
  options(warn=-1)
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
    mutate(p = model$anova_table$`Pr(>F)`) %>% 
    dplyr::group_by(Parameter) %>%
    mutate(p1 = p_txt(p)) %>%
    mutate(p2 = p_txt2(p)) %>%
    mutate(f = paste("*F*(",num.Df, ", ",den.Df,") = ",F, sep = "")) %>%
    mutate(ges = paste("$\\hat{\\eta}^2_G$ = ",round(ges,2))) %>%
    mutate(pes = paste("$\\hat{\\eta}^2_p$ = ",round(Eta2_partial,2))) %>%
    mutate(pes_ci =  paste("95% CI [",round(CI_low,2),", ",round(CI_high,2),"]", sep = "")) %>%
    mutate(pes_full =  paste(pes,", ", pes_ci, sep = "")) %>%
    mutate(full= paste(f,", ",p1,", ", ges,", ", pes,", ", pes_ci, sep = "")) %>%
    mutate(docx =
             paste0("F(",num.Df, ", ",den.Df,") = ",F,", ",p2,", ", "pes = ", 
                    nozero(round(Eta2_partial,2)))) %>% 
    mutate(small= paste(F,", ",p, sep = "")) %>% as.data.frame()
  rownames(txt)<- txt$Parameter
  # list <- setNames(split(txt, seq(nrow(txt))), rownames(txt))
  # txt<-txt[txt$Parameter == effect,]
  # return(txt)

  tit("ANOVA output")
  print(model)
  tit("ANOVA text")
  print(txt$docx)
}


nozero<-function(x, digits=2){sub("^(-?)0.", "\\1.", 
                                  sprintf(paste0("%.",digits,"f"), x))}

# nozero(0.0005, digits = 6)
mean2<-function(x, na =F){
 y<- round(mean(x, na.rm=na),2)
 return(y)
}
sd2<-function(x, na =F){
  y<- round(sd(x, na.rm=na),2)
  return(y)
}
med2<-function(x, na =F, round=2){
  y<- round(median(x, na.rm=na),round)
  return(y)
}

# color utility functions
col.desat <- function( acol , amt=0.5 ) {
  acol <- col2rgb(acol)
  ahsv <- rgb2hsv(acol)
  ahsv[2] <- ahsv[2] * amt
  hsv( ahsv[1] , ahsv[2] , ahsv[3] )
}
rangi2 <- col.desat("blue",0.5)# a beautiful color

percent <-function(number, dec = 0){ 
  percentage<-paste(round(number*100,dec),"%",sep = "")
  return(percentage)
}
# function that returns a percentage from a proportion
# EX  = percent(7/10)

# require(plyr)
# myfun <- function(x, data, ...) {
#   c(qqnorm(data[[x]], main = names(data)[x], ...),
#     qqline(data[[x]])
#   )
# }
qqplot <- function(x, data, ...) {
  c(qqnorm(data[[x]], main = names(data)[x], ...),
    qqline(data[[x]])
  )
}
histplot <- function(x, data, ...) {
  hist(data[[x]], main = names(data)[x], ...)
} 

shape <- function(data){
  qqplot <- function(x, data, ...) {
    c(qqnorm(data[[x]], main = names(data)[x], ...),
      qqline(data[[x]])
    )
  }
  histplot <- function(x, data, ...) {
    hist(data[[x]], main = names(data)[x], ...)
  }
require(plyr)
x<-data
n<-ncol(x)
fn<-paste0("SHAPE/", "shape_",colnames(x),".png")
png(filename = outme(fn))
par(mfrow = c(1,2))
l_ply(seq_len(ncol(x)),c(histplot, qqplot), data = x)
dev.off()

}

# x<-d %>% select(bdn_score, nbn_score, total_score, vv_tilt, pv_tilt)
# png(filename ="shape.png", width = 15, height = 30, units = "cm", res = 100)
# shape(x)
# dev.off()

plot_corr<-function(data, rowx, list_corr){
  ls<-list_corr[rowx,]
  v1<-ls$var1 
  v2<-ls$var2
  b<-ls$coef
  ci<-ls$ci
  ggplot(data = d) + aes_string(x = v2, y=v1)+ 
    feat+
    geom_point(col = ls$col, alpha = .4, size  = 5) +
    labs(y = ls$lab1, x = ls$lab2) + 
    annotate("text", x = x_cor, y = y_cor, 
             label = bquote(italic(r)~ "=" ~.(b)~","~.(ci)),size = 5)+
    annotate("text", x = x_p, y = y_p, 
             label =p_plot(ls$p_bon),size = 5)
}

rho_txt <- function(model){
  cor<-model
  b<-round(cor$estimate,2) # estimation of the beta slope.
  pv<-p_txt(cor$p.value)# raw p value. Beyond 10^-6, the round will give 0.
  
  small<- paste("*r*~s~ =", b,", ",pv, sep = "")
  
  return(list(small=small))
  
}

# spearman(d, "total_score", "total_tilt")


# p_txt<-function(p){
#   library(latex2exp)
#   a = 'p < '  
#   b = 'p = '  
#   p <- ifelse(is.character(p), as.numeric(p), p)
#   p2 <-round(p, 2)
#   p3 <- round(p, 3)
#   pc2 <- as.character(p2)
#   pc3 <- as.character(p3)
#   
#   pv<-
#     ifelse(p < 0.000001,
#            "$$\\{p < } 10^{-6}$$",
#            ifelse(p < .00001, 
#               "$$\\{p < } 10^{-5}$$",
#                   ifelse(p < .0001, 
#                   "$$\\{p < } 10^{-4}$$",
#                          ifelse(p < .001, 
#                            "$$\\textit{p < } 0.001$$",
#                                 ifelse(p < .05, 
#                                        paste("*p* < ", p, sep = ""), 
#                                        paste("*p* = ", p, sep = ""))))))
#   inline<-pv
#   return(inline)
# }

p_txt<-function(p){
  a = 'p < '
  b = 'p = '
  p <- ifelse(is.character(p), as.numeric(p), p)
  p2 <- round(p, 2)
  p3 <- round(p, 3)
  pv<-
    ifelse(p < 0.000001,
           "*p* < 10^-6^",
           ifelse(p < .00001,
                  "*p* < 10^-5^",
                  ifelse(p < .0001,
                         "*p* < 10^-4^",
                         ifelse(p < .001,
                                "*p* < .001",
                                ifelse(p < .01,
                                       "*p* < .01",
                                  ifelse(p < .05,
                                       paste("*p* = ", nozero(p3), sep = ""),
                                        paste("*p* = ", nozero(p2), sep = "")))))))
  
  return(pv)
}
p_txt2<-function(p){
  a = 'p < '
  b = 'p = '
  p <- ifelse(is.character(p), as.numeric(p), p)
  p2 <- round(p, 2)
  p3 <- round(p, 3)
  pv<-
    ifelse(p < 0.000001,
           "p < 10^-6^",
           ifelse(p < .00001,
                  "p < 10^-5^",
                  ifelse(p < .0001,
                         "p < 10^-4^",
                         ifelse(p < .001,
                                "p < .001",
                                ifelse(p < .01,
                                       "p < .01",
                                       ifelse(p < .05,
                                              paste("p = ", nozero(p3), sep = ""),
                                              paste("p = ", nozero(p2), sep = "")))))))
  
  return(pv)
}


# p_txt_raw(0.000005)
# p_txt(0.555)
p_plot<-function(p){
  a = 'p < '
  b = 'p = '
  p <- ifelse(is.character(p), as.numeric(p), p)
  p2 <- round(p, 2)
  p3 <- round(p, 3)
  p20<-nozero(p2)
  p30<-nozero(p3)
  star3<-nozero(0.001, 3)
  star2<-nozero(0.01, 2)
  pv<-
    ifelse(p < 0.000001,
           parse(text="bquote(italic(p)~'<'~10^-6)"),
           ifelse(p < .00001,
                  parse(text="bquote(italic(p)~'<'~10^-5)"),
                  ifelse(p < .0001,
                         parse(text="bquote(italic(p)~'<'~10^-4)"),
                         ifelse(p < .001,
                                parse(text="bquote(italic(p)~'<'~.(star3))"),
                                ifelse(p < .01,
                                       parse(text="bquote(italic(p)~'<'~.(star2))"),
                                       ifelse(p < .05,
                                              parse(text= paste0("italic(p)~'<'~",
                                              nozero(p3))),
                                              parse(text= paste0("italic(p)~'='~",
                                                                 nozero(p2)))))))))
  
  return(pv)
}

q2q1q3 <-function(data, dec = 2){  
  # dec is round value
  x<-quantile(data, probs = c(0.5, 0.25, 0.75), na.rm = TRUE) 
  y<-paste(round(x[1],dec), " (",round(x[2],dec), "; ", 
               round(x[3],dec), ")", sep = "")
  return(y)
}
# function that returns "Q2 (Q1; Q3)"
# EX  = q2q1q3(xxx)

q1q3 <-function(data, round= 2){  
  q1_q3<-quantile(data, probs = c(0.25, 0.75), na.rm = TRUE) 
  q1_q3<-paste("(",round(q1_q3[1],round), "; ", 
               round(q1_q3[2],round), ")", sep = "")
  return(q1_q3)
}
# function that returns "(Q1; Q3)
# EX  = q1q3(all$ben_angle_line1)

q1q3bis <-function(data, round= 2){  
  q1_q3<-quantile(data, probs = c(0.25, 0.75), na.rm = TRUE) 
  q1_q3<-paste("[",round(q1_q3[1],round), "; ", 
               round(q1_q3[2],round), "]", sep = "")
  return(q1_q3)
}
# function that returns "(Q1; Q3)
# EX  = q1q3(all$ben_angle_line1)

tt_txt <- function(model, beta = T, unit = ""){
  ttest<-model
  # one sample t test
  
  q<-round(ttest$statistic,2)# statistic q
  dof<-round(ttest$parameter,2)# global degree of freedom
  pv<-p_txt(ttest$p.value)# raw p value. Beyond 10^-6, the round will give 0.
  b <- round(ttest$estimate[1] - ifelse(is.na(ttest$estimate[2]),0,ttest$estimate[2]),2)
  contrast<-ttest$data.name
  side<-ttest$alternative
  # estimation of the beta slope.
  
  full<- paste(
    ifelse(isTRUE(beta),
           paste(contrast,"; ", side, "; *M_diff* = ",b,unit, ", ", sep = ""), ""),
    "95% CI [",round(ttest$conf.int[1],2),unit,", ",
    round(ttest$conf.int[2],2),unit,"], *t*(",dof,") = ",q,", ",pv,
    sep ="")
 
  small<- paste("*t*(",dof,") = ",q,", ",pv,
                sep ="")
  
  M<- paste("*M* = ",b,unit, sep ="")
  CI<- paste("95% CI [",round(ttest$conf.int[1],2),unit,", ",
             round(ttest$conf.int[2],2),unit,"],", sep ="")
  CI_raw <- paste("[",round(ttest$conf.int[1],2),unit,", ",
                  round(ttest$conf.int[2],2),unit,"],", sep ="")
  
  M_CI <-paste(M, CI, sep= "")
  myappend(full)
  
  return(list(full=full, small=small, M = M, CI = CI, M_CI = M_CI,
              M_raw = b, CI_raw = CI_raw, p = pv))
}
# function that returns a t-test with APA style

cor_txt <- function(model, data, spearman = F, feat = T){
   model<-cor.test(mtcars$mpg, mtcars$disp)
  # data<-mtcars
  # q<-cor_txt(model, data, spearman=T)
  # q$plot
  # q$full
  # plot(data$mpg, data$vs, xlab =  q$plot_txt$full)
   # model<-cor.test(d$vv_sd, d$bell_prop)
  cor<-model
  b<-round(cor$estimate,2) # estimation of the beta slope.
  coef<-nozero(b)
  # q<-round(cor$statistic,2)# statistic q
  dof<-if(!is.null(cor$parameter)){round(cor$parameter,2)} # global degree of freedom
  pv<-p_txt(cor$p.value)# raw p value. Beyond 10^-6, the round will give 0.
  ll<- if(!is.null(cor$conf.int)){nozero(round(cor$conf.int[1],2))}
  ul<- if(!is.null(cor$conf.int)){nozero(round(cor$conf.int[2],2))}
  
  var1<-strsplit(cor$data.name, "$",fixed=TRUE)[[1]][3]
  var02<-strsplit(cor$data.name, "$",fixed=TRUE)[[1]][2]
  var2 <-strsplit(var02, " ",fixed=TRUE)[[1]][1]
  var<-paste(var1, " ~ ", var2, sep = "")
 
  library(DescTools)
  ll<-
    if(spearman==T) {
        nozero(round(SpearmanRho(data[,var1], y = data[,var2], use = "complete.obs", 
                                conf.level = 0.95)[1],2))
    } else {
        ll
    }

  ul<-
    if(spearman==T) {
      nozero(round(SpearmanRho(data[,var1], y = data[,var2], use = "complete.obs", 
                        conf.level = 0.95)[2],2))
    } else {
      ul
    }

  
  full<- paste("*r* = ",coef,", 95% CI [",ll,"; ", ul,"], ",pv,"",sep ="")
  medium<- paste("*r* = ",coef,", 95% CI [",ll,"; ", ul,"]",sep ="")
  small<- paste("*r* = ",coef,", ",pv,sep ="")
  
  p_for_plot<-p_plot(cor$p.value)
  
  full_plot<- parse(text="bquote(italic(r)~'='~.(coef)~', 95% CI ['~.(ll)~'; '~.(ul)~'], '~.(eval(p_for_plot)))")
  medium_plot<- parse(text="bquote(italic(r)~'='~.(coef)~', 95% CI ['~.(ll)~'; '~.(ul)~']')")
  small_plot<- parse(text="bquote(italic(r)~'='~.(coef)~', '~.(eval(p_for_plot)))")
                    
   # plot(data$mpg, data$vs,xlab = eval(medium_plot))
  # expr <- bquote(bgroup("(",frac(1,3)*", "*frac(1,2),")")) 
  # plot(0, 0, xlab = bquote(alpha~"is in "~.(expr)), ylab = "")

  ci<-paste0("95% CI [",ll,"; ", ul,"]")
  
  lab1<-attributes(data[,var1])$label
  lab2<-attributes(data[,var2])$label
  labs<-paste(lab1, " ~ ", lab2, sep = "")
  
  annotation <- function() {return(eval(full_plot))}
  plot<-
    ggplot(data = data) + aes_string(x = var1, y=var2)+ 
    labs(y = lab1, x = lab2) + 
      annotate("text", x= -Inf,y=+Inf,
               hjust=-0.05,vjust=1,
                  label=do.call(annotation, args = list()))
  
  feature <- list(
    geom_point(alpha=0.5),
    geom_smooth(method = "lm",col = "black", alpha = 0.6, size =1.2),
    theme_set(theme_bw(base_size = 18)))

  if(feat==T) {
    plot <- plot + feature
  }
  return(list(full=full, medium = medium, small=small, p = pv, plot = plot, 
              plot_txt = list(full = eval(full_plot), medium = eval(medium_plot), small = eval(small_plot)),
              p_raw =cor$p.value, corr = b,ci =ci,coef=coef,
              labels = labs, var = var,
              var1 = var1, 
              var2 = var2, lab1 = lab1, lab2 = lab2))
  
}
# function that returns a Pearson correlation with APA style

chi_txt<-function(table){
  # https://stats.stackexchange.com/questions/188651/statistical-reporting-of-chi-square-and-odds-ratio
  # https://www.rdocumentation.org/packages/esc/versions/0.5.1/topics/esc_chisq
  khi<-chisq.test(table)
  stati<-round(khi$statistic,2)
  pv<-khi$p.value
  ddl<-khi$parameter
  inline<-paste("$\\chi^2$","(",ddl,", N = ",sum(khi$observed),") = ",stati,", ", p_txt(pv), sep ="")
  docx<-paste("X^2(",ddl,") = ",stati,", ", p_txt2(pv), sep ="")
  
  return(inline)
}
# function that returns a Chi Square with APA style

fisher_txt<-function(table){
  # https://stats.stackexchange.com/questions/188651/statistical-reporting-of-chi-square-and-odds-ratio
  # https://www.rdocumentation.org/packages/esc/versions/0.5.1/topics/esc_chisq
  fish<-fisher.test(table)
  pv<-p_txt(fish$p.value)
  return(pv)
}
# function that returns a Chi Square with APA style

or<-function(matrix){
  or<-(matrix[1,1]*matrix[2,2])/(matrix[1,2]*matrix[2,1])
  log_or<-log(or)
  se<-sqrt(1/matrix[1,1] + 1/matrix[2,2] + 1/matrix[1,2] + 1/matrix[2,1])
  upper<-exp(log_or+1.96*se)
  lower<-exp(log_or-1.96*se)
  txt<-paste("OR = ", round(or,2), ", ","95% CI [",round(lower,2),",",round(upper,2),"]", sep="")
  txt_ci<-paste("95% CI [",round(lower,2),", ",round(upper,2),"]", sep="")
  txt_ci2<-paste("[",round(lower,2),", ",round(upper,2),"]", sep="")
  return(list(or = round(or,2), lower_ci = round(lower,2), 
              upper_ci = round(upper,2), txt = txt, txt_ci=txt_ci,
              txt_ci2=txt_ci2))
}

out <- function(model, data, cook = 4, hat=3, intercept = FALSE){
  library(ggfortify)
# model<-lm(vv_tilt ~ bdn_score_c, d)
  data$id<-seq(1:nrow(data))
  cutoff_cook <- cook/((nrow(data)-length(model$coefficients)-2))
  df_cook<-data[which(cooks.distance(model) > cutoff_cook),] %>% 
    as_tibble() %>% 
    mutate(tool = "cookd")
  
  cutoff_hat <- mean(hatvalues(model)) * hat
  df_hat<-data[which(hatvalues(model) > cutoff_hat),] %>% 
    as_tibble() %>% 
    mutate(tool = "hat")
  
  n = nrow(data) 
  cutoff_sdr = qt(1 - 0.05 / (2*n), (n - 4)) 
  df_sdr<-data[which(abs(rstudent(model)) > cutoff_sdr),] %>% 
    as_tibble() %>% 
    mutate(tool = "sdr")
  
  # plot 1
  multi_plot<-autoplot(model)
  p1<-multi_plot[2]+
    theme_bw(base_size=10)
  # plot 2
  p2<-multi_plot[1]+
    theme_bw(base_size=10)
  # plot 3 = d of cook
  p3<-cooks.distance(model) %>% 
    as_tibble() %>% rownames_to_column("id") %>% 
    ggplot()+aes(x=value, y = id, label=id)+
    labs(y =NULL)+guides(y = "none")+
    geom_label(size = 2, col = "blue")+
    theme_bw(base_size = 10)+
    geom_vline(xintercept = cutoff_cook, 
               lty="dashed", col="red",size=1)+
    labs(x = "Cook D", y ="Participant")
  # plot 4 = hat value
  p4<-hatvalues(model) %>% 
    as_tibble() %>% rownames_to_column("id") %>% 
    ggplot()+aes(x=value, y = id, label=id)+
    geom_label(size = 2, col = "blue")+
    labs(y =NULL)+guides(y = "none")+
    theme_bw(base_size = 10)+
    geom_vline(xintercept = cutoff_hat, lty="dashed", col="red",size=1)+
    labs(x = "Leverage", y ="Participant")
  # plot 5 = SDR
  p5<- rstudent(model) %>% 
    as_tibble() %>% rownames_to_column("id") %>% 
    ggplot()+aes(x=value, y = id, label=id)+
    geom_label(size = 2, col = "blue")+
    labs(y =NULL)+guides(y = "none")+
    theme_bw(base_size = 10)+
    geom_vline(xintercept = c(-cutoff_sdr,cutoff_sdr), lty="dashed", col="red",size=1)+
    labs(x = "R Student", y ="Participant")
  
  
  #p_out <- recordPlot()
  outlier_name <- 
    bind_rows(df_cook, df_hat, df_sdr) %>% # list of outliers
    select(id) %>% unique()

  data_no_outlier <- data %>% 
    dplyr::filter(!id %in% outlier_name$id)
  old_model<-model
  new_model <- update(model,data=data_no_outlier)
  coeff<-bind_rows(tidy(old_model, conf.int = T) %>% mutate(model = "old"),
                   tidy(new_model,conf.int = T) %>% mutate(model = "new"))
  
  coeff<-
    if(intercept == FALSE){coeff[-c(1,3),]}
  
  p6<-ggplot(data = coeff, 
             aes(x = term, y = estimate,  
                 ymin = conf.low, ymax = conf.high, col = model)) +
    geom_point(size = 2, position = position_dodge(0.2)) +
    geom_errorbar(width = 0.01,  position = position_dodge(0.2)) +
    geom_hline(yintercept = 0, lty = "dashed",size=1,col="red") +
    coord_flip()+ 
    labs(x ="" , y = paste("Estimate\nOut=",paste0(outlier_name$id, collapse = ";")))+ 
    scale_color_discrete(name = "Model", 
                         labels = c("New", "Old"))+
    theme_bw(base_size=10)
  plot<-p1+p2+p3+p4+p5+p6
  
  return(list(plot=plot,outlier = outlier_name, 
              new_data = data_no_outlier, coeff=coeff))
}


mann_table <- function(mann, data){
  data<-df
  df<-data
  p<-mann$p.value
  # ci<-paste("95% CI [", mann$conf.int[1] %>% round(.,2),"; ",
  #           mann$conf.int[2] %>% round(.,2),"]", sep = "")
  zp<-paste("Z = ",round(qnorm(mann$p.value/2),2),", ", p_txt(p), sep="")
  z<-round(qnorm(mann$p.value/2),2)
  library(rcompanion)
  dv<-strsplit(mann$data.name, split = " by ")[[1]][1]
  iv<-strsplit(mann$data.name, split = " by ")[[1]][2]
  res<-z/sqrt(na.omit(nrow(na.omit(data[,dv]))))
  # es<-wilcoxonR(x = df[,dv], 
  #               g = df[,iv])
  # r<-paste("r = ", round(es[1],2),sep ="")
  # ci_r<-paste("95% CI [", round(es[2],2),", ",round(es[3],2), "]", sep = "")
  mat<-cbind(data[,dv], data[,iv])
  colnames(mat)<-c("measure","group")
  qqq<-tapply(mat$measure,mat$group, q2q1q3)

  table<-data.frame(dv = dv, group1 = qqq[1], group = qqq[2],wilcox = zp, 
                    r =round(res,2), p = round(mann$p.value,4))
  return(table)
}

median_table <- function(data, iv, dv){
  df<-data
  lev<-levels(as.factor(df[,iv]))
  attach(df)
  x<-cbind(eval(parse(text = iv)), eval(parse(text = dv)))
  lev1<-q2q1q3(x[x[,1] ==lev[1],2], dec=2)
  lev2<-q2q1q3(x[x[,1] ==lev[2],2], dec=2)
  detach(df)
  
  table<-data.frame(dv = dv, group_1 = lev1, group_2 = lev2)
  return(table)
}

baplot <- function(x,y){
  xstd = (x - mean(x))/sd(x)
  ystd = (y - mean(y))/sd(y)
  
  bamean = (xstd+ystd)/2
  badiff = (ystd-xstd)
  cis<-c(mean(badiff)+1.96 * sd(badiff),mean(badiff)-1.96 * sd(badiff))

  w<-data.frame(bamean = bamean, badiff=badiff)
  p<-ggplot(w, aes(x = bamean, y = badiff))+
    labs(title = "Bland-Altman plot", x="mean", y="difference")+
    geom_hline(yintercept=mean(badiff))+
    geom_hline(yintercept=cis, lty = "dashed")+
    geom_point(size=3, shape = 21, fill = "grey", col = "black")+
    theme_bw(base_size=18)
  print(p)

} 


irr<-function(data){
  SEM<-sem(data = data, type = c("sd"), conf.level = 0.95)
  mdc<-1.96*SEM$est*sqrt(2)
  output<-data.frame(SEM = round(SEM$est,2), MDC = round(mdc,2))
  print(output)
}

ch<-function(x){cat(x, sep = "\n")}
br<-function(x){cat(x="",sep = "\n")}
sep<-function(x){cat(x="-----------------------------------------------------------------------",
                     sep = "\n")}
sep2<-function(x){cat(x="=====================================================================",
                      sep = "\n")}
cod<-function(x){cat(x="```",sep = "\n")}
# sink(file = here::here("redaction", "stat.txt"), type = "output")
tit<-function(x){
  sep2()
  cat(x, sep = "\n")
  sep2()
}
tit2<-function(x){
  sep()
  cat(rep("=", nchar(x)),sep = "")
  cat(sep="\n")
  cat(x, sep = "\n")
  cat(rep("=", nchar(x)),sep = "")
  cat(sep="\n")
  sep()
}
# chi_table <- function(iv, dv, data){
# 
#   # P<-p_txt(x$p.value)
#   Result <- chi_txt(eval(parse(text = x$data.name)))
#   
#   library(rcompanion)
#   ES<-round(cramerV(table,conf=0.95, ci = T, digits = 2),2)
#   V<-paste("V = ",ES[1], sep ="")
#   V_CI<-paste("V = ",ES[1], ", 95% CI [",ES[2],", ",ES[3],"]", 
#               sep ="")
#   CI<-paste("95% CI [",ES[2],", ",ES[3],"]", sep ="")
#   
#   table<-data.frame(DV = dv_name,group_1 = "", group_2 = "", 
#                     KHI = Result,
#                     ES = V, CI = CI,  K = K$statistic)
#   
#   return(table)
# }

