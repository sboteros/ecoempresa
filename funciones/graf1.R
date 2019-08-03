graf1 <- function(df, x, y, g = NULL, fun, label, title, subtitle = "", 
                  xlab, ylab, colours = 1, legtext = NULL, xlim = NULL,
                  ylim = NULL, legpos = "none") {
  ggplot(df, aes_string(x, y, group = g, colour = g)) + geom_point() + 
    stat_function(fun = fun, colour = 1) + 
    geom_text(aes_string(label = label), vjust = "outward", hjust = "inward") + 
    geom_segment(aes_string(xend = x, yend = 0), 
                 linetype = "dashed", colour = "gray") + 
    geom_segment(aes_string(xend = 0, yend = y), 
                 linetype = "dashed", colour = "gray") + 
    scale_x_continuous(expand = c(0, 0), limits = xlim) + 
    scale_y_continuous(expand = c(0, 0), limits = ylim) + 
    theme_classic() + 
    scale_color_manual("", values = colours, labels = legtext) + 
    theme(axis.title.y = element_text(angle=0), legend.position = legpos) +
    ggtitle(title, subtitle) + xlab(xlab) + ylab(ylab)
}
