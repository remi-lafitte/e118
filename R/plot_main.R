plot_main <- function(df, iv, dv){

  iv <- enquo(iv)
  dv <- enquo(dv)
  msd_main<- df %>%
    dplyr::select(!!dv, !!iv) %>%
    dplyr::group_by(!!iv) %>%
    dplyr::summarise(n = length(!!iv),
                     m = mean(!!dv, na.rm = T),
                     sd = sd(!!dv, na.rm=T),
                     m = round(m,2),
                     sd = round(sd,2),
                     lower_sd = m - sd,
                     upper_sd = m+sd,
                     txt = paste("*M* = ",m,", *SD* = ",sd, sep = "")) %>%
    dplyr::ungroup()

  pd = position_dodge(0.2)
  plot_main <-
    ggplot(msd_main, aes({{iv}}, m, col = {{iv}}, fill = {{iv}}))+
    geom_line(data = msd_main, aes({{iv}}, m),group = 1,
              size = 1, col = "black")+
    # #geom_hline(yintercept = 0, lty = "dashed") +
    geom_jitter(data = df, aes({{iv}}, y = {{dv}}, col = {{iv}}, fill = {{iv}}),
                size = 5,
                alpha = 0.4,
                position = position_jitterdodge(jitter.width = 0.4,
                                                dodge.width = 0.25)
    )+
    geom_errorbar(data  = msd_main, aes(ymin = lower_sd, ymax = upper_sd),
                  width = 0.1, position = pd, size = 0.9,
                  col = "black") +
    geom_point(shape = 21,size = 4, stroke = 1.6, col = "black" )+
    theme(plot.title = element_text(hjust = 0.5))+
    theme_bw()+
    theme_set(theme_bw(base_size = 18))+
    guides(col = F, fill = F)

  return(list(plot=plot_main, msd = msd_main))
}
# x<-plot_main(dp_line, iv = line, dv = tilt)
# x$msd$txt
