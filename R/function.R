nozero<-function(x){sub("^(-?)0.", "\\1.", sprintf("%.2f", x))}


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
x<-data
n<-ncol(x)
par(mfrow = c(n,2),  mar=c(4, 4,4,4))
l_ply(seq_len(ncol(x)),c(histplot, qqplot), data = x)
}
# x<-d %>% select(bdn_score, nbn_score, total_score)
 # shape(x)


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
                                       paste("*p* < ", nozero(p3), sep = ""),
                                        paste("*p* = ", nozero(p2), sep = "")))))))
  
  return(pv)
}
# p_txt(0.555)
p_plot<-function(p){
  a = 'p < '
  b = 'p = '
  p <- ifelse(is.character(p), as.numeric(p), p)
  p2 <- round(p, 2)
  p3 <- round(p, 3)
  p20<-nozero(p2)
  p30<-nozero(p3)
  pv<-
    ifelse(p < 0.000001,
           parse(text="italic(p)~'<'~10^-6"),
           ifelse(p < .00001,
                  parse(text="italic(p)~'<'~10^-5"),
                  ifelse(p < .0001,
                         parse(text="italic(p)~'<'~10^-4"),
                         ifelse(p < .001,
                                parse(text="italic(p)~'<'~.001"),
                                ifelse(p < .01,
                                       parse(text="italic(p)~'<'~.01"),
                                       ifelse(p < .05,
                                              parse(text= paste0("italic(p)~'<'~",
                                              nozero(p3))),
                                              parse(text= paste0("italic(p)~'='~",
                                                                 nozero(p2)))))))))
  
  return(pv)
}

# p_plot(0.2)
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
  # estimation of the beta slope.
  
  full<- paste(
    ifelse(isTRUE(beta),
           paste("*M* = ",b,unit, ", ", sep = ""), ""),
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
  
  
  
  return(list(full=full, small=small, M = M, CI = CI, M_CI = M_CI,
              M_raw = b, CI_raw = CI_raw, p = pv))
}
# function that returns a t-test with APA style
cor_txt <- function(model, data, spearman = F){
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

  ci<-paste0("95% CI [",ll,"; ", ul,"]")

  lab1<-attributes(data[,var1])$label
  lab2<-attributes(data[,var2])$label
  labs<-paste(lab1, " ~ ", lab2, sep = "")

  
  return(list(full=full, medium = medium, small=small, p = pv,
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
