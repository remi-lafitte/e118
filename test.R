#ANOVA--------------
cool<-
  function(data,
           model,
           output = "html",
           open = F,
           test = "lm",
           filename = test){

    rmarkdown::render(paste0(test,".Rmd"), output_format = paste0(output, "_document"),
                      output_file = filename)
    if(open == T){
      browseURL(here::here(paste0("anova.", output)))
    }
    if(open == T & output == "word"){
      system2("open","anova.docx")
    }
  }

data("Ericksen")
d<-Ericksen
d$id<-seq(1:nrow(d))
model<-lm(highschool ~ city + language,d)

cool(data = d, model = model, output = "word", open = T, filename = "23",
     test = "lm")

data("mtcars")
d<-mtcars
d$id<-seq(1:nrow(d))
model<-afex::aov_4(mpg ~ vs * gear +  (1|id),d, factorize = T)
model
aov4_txt(model)$full
anova_report(model = model, data = d, output = "word", open=T)
#TTEST-----------------------
ttest_report<-
  function(data = mtcars,
                       model = t.test(mpg ~  vs,mtcars),
                       output = "html",
                       open = F,
                       filename = ""){

  if(filename == ""){
  filename<-"t-test"
  }
  rmarkdown::render("ttest.Rmd", output_format = paste0(output, "_document"),
                    output_file = filename)
  if(open == T){
    browseURL(here::here(paste0("t-test.", output)))
  }
  if(open == T & output == "word"){
    system2("open","t-test.docx")
  }
}
# data(iris)
# data<-iris[df$Species!="setosa",]
# model<-t.test(Sepal.Length ~  1, data)
# ttest_report(data =data, model = model,
#              output = "html", open =T)

#----------------------------------------------------------------
# library(officer)
# c<-read_docx(here::here("ttest.docx"))
# docx_summary(c)
# https://theautomatic.net/2020/07/21/how-to-read-and-create-word-documents-in-r/

ttest_report(output = "word", open =T)
