plot_two_interaction <- function(df, iv1, iv2,iv3, dv, continuous = c(F, T)){
  # by default all ariables are categorical

  iv1 <- enquo(iv1)
  iv2 <- enquo(iv2)
  iv3 <- enquo(iv3)
  dv <- enquo(dv)
  msd_two_interaction<- df %>%
    dplyr::select(!!dv, !!iv1, !!iv2, !!iv3) %>%
    dplyr::group_by(!!iv1, !!iv2, !!iv3) %>%
    dplyr::summarise(n1 = length(!!iv1),
                     n2 = length(!!iv2),
                     n3 = length(!!iv3),
                     m = mean(!!dv, na.rm = T),
                     sd = sd(!!dv, na.rm=T),
                     m = round(m,2),
                     sd = round(sd,2),
                     lower_sd = m - sd,
                     upper_sd = m+sd,
                     txt = paste("*M* = ",m,", *SD* = ",sd, sep = "")) %>%
    dplyr::ungroup()

  pd = position_dodge(0.5)

  plot_two_interaction <-
    ggplot(msd_two_interaction, aes({{iv1}}, m, color = {{iv2}}, fill = {{iv2}}))+
    geom_line(data = msd_two_interaction, aes(y = m, group = {{iv2}}),
              size = 2, position = position_dodge(width = 0.5))+
    #geom_hline(yintercept = 0, lty = "dashed") +
    geom_jitter(data = df, aes({{iv1}}, y = {{dv}}, col = {{iv2}}, fill = {{iv2}}),
                size = 5,
                alpha = 0.4,
                position = position_jitterdodge(jitter.width = 0.3,
                                                dodge.width = 0.5)
    ) +
    geom_errorbar(data  = msd_two_interaction, aes(ymin = lower_sd, ymax = upper_sd),
                  width = 0.1, position = pd, size = 0.9,
                  col = "black") +
    geom_point(shape = 21,size = 4, stroke = 1.6, col = "black", position = pd )+
    theme(plot.title = element_text(hjust = 0.5))+
    labs( x = "", y = "")+
    theme_bw()+
    theme_set(theme_bw(base_size = 18))+
    guides(col = F, fill =F) +
    facet_wrap(vars(!!!iv3))

  return(list(plot = plot_two_interaction , msd = msd_two_interaction))


}
