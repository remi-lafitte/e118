#' @export msd
#' @param df = data.frame
#' @param ivs = group of independent variables; eg = c("a", "b"), or just "a"
#' @param dv = dependent variable
#' @examples
#' set.seed(42)
#' z <- data.frame(a1 = c(rnorm(100,2), rnorm(100,1),rnorm(100,0)),
#'                 b  = rep(c("A", "B", "C"), each = 100),
#'                 c  = factor(rbinom(300, 1, .5)),
#'                 ID = 1:300,
#'                 a2 = c(rnorm(100,2), rnorm(100,1),rnorm(100,0)),
#'                 a3 = c(rnorm(100,2), rnorm(100,1),rnorm(100,0)))
#'msd(df = z, iv = "b", dv = "a1")


msd <- function(df, ivs, dv, ci = c(T,F)){

  ivs<-syms(ivs)

  msd<-
    df %>%
    group_by(!!! ivs) %>%
    summarise(mean = mean(!!sym(dv), na.rm = TRUE),
              sd = sd(!!sym(dv), na.rm = TRUE),
              lower.sd = mean - sd,
              upper.sd = mean + sd,
              n = n()) %>%
    mutate(se = sd / sqrt(n),
           lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(txt = paste("*M* = ",mean,", *SD* = ",sd, sep = ""))



  return(list(msd = msd))
}
