graf1 <- function(df, x, y, gr = NULL,
                  fun1 = NULL, fun2 = NULL, fun3 = NULL, fun4 = NULL,
                  label = NULL, df_lab = df, x_lab = x, y_lab = y, grid = TRUE,
                  df_somy = NULL, x_somy = NULL, y_somy_min = NULL,
                                  y_somy_max = NULL,
                  df_somx = NULL, x_somx_min = NULL, x_somx_max = NULL,
                                  y_somx = NULL,
                  title = "", subtitle = "", xtit = "", ytit = "", 
                  colours = 1, legtext = NULL, legpos = "none",
                  xlim = NULL, ylim = NULL, origen = c(0, 0)) {
  # Gráfico básico
  g <- ggplot(df, aes_string(x, y, group = gr, colour = gr)) + 
    scale_x_continuous(expand = origen, limits = xlim) + 
    scale_y_continuous(expand = origen, limits = ylim) + 
    theme_classic() + 
    scale_color_manual("", values = colours, labels = legtext) + 
    theme(axis.title.y = element_text(angle=0), legend.position = legpos) +
    ggtitle(title, subtitle) + xlab(xtit) + ylab(ytit)
  # Funciones
  if (!is.null(fun1)) {g <- g + stat_function(fun = fun1, colour = 1)}
  if (!is.null(fun2)) {g <- g + stat_function(fun = fun2, colour = 2)}
  if (!is.null(fun3)) {g <- g + stat_function(fun = fun3, colour = 3)}
  if (!is.null(fun4)) {g <- g + stat_function(fun = fun4, colour = 4)}
  # Labels
  if (!is.null(label)) {
    g <- g + geom_point(data = df_lab,
                        mapping = aes_string(x = x_lab, y = y_lab)) +
      geom_text(data = df_lab, 
                mapping = aes_string(x = x_lab, y = y_lab, label = label), 
                inherit.aes = FALSE, vjust = "outward", hjust = "inward")
    if (grid) {
      g <- g + geom_segment(data = df_lab, 
                            mapping = aes_string(x = x_lab, y = y_lab,
                                                 xend = x_lab, yend = 0),
                            inherit.aes = FALSE, linetype = "dashed", 
                            colour = "gray") + 
        geom_segment(data = df_lab, 
                     mapping = aes_string(x = x_lab, y = y_lab,
                                          xend = 0, yend = y_lab), 
                     inherit.aes = FALSE, linetype = "dashed", colour = "gray") 
    }
  }
  # Sombreados
  if (!is.null(df_somy)) {
    g <- g + geom_ribbon(data = df_somy,
                         mapping = aes_string(x = x_somy, ymin = y_somy_min,
                                              ymax = y_somy_max), 
                         inherit.aes = FALSE, fill = "blue", alpha = 0.4)
  }
  if (!is.null(df_somx)) {
    g <- g + geom_ribbon(data = df_somx,
                         mapping = aes_string(xmin = x_somx_min, 
                                              xmax = x_somx_max, y = y_somx), 
                         inherit.aes = FALSE, fill = "blue", alpha = 0.4)
  }
  # Imprimir
  g
}
