ttest_report<-function(data = mtcars,
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
    system2("open","ttest.docx")
  }
}

ttest_report(output = "html", open =T) # open = T don't work with word output
# library(officer)
# c<-read_docx(here::here("ttest.docx"))
# docx_summary(c)
# https://theautomatic.net/2020/07/21/how-to-read-and-create-word-documents-in-r/

ttest_report(output = "word", open =T)
