#' Grafica una distribución normal estandarizada
#'
#' Sombrea la región de rechazo para una prueba de dos colas
#'
#' @details Esta función grafica utilizando la distribución normal estandarizada,
#' considerando como valores de entrada un valor crítico pre-calculado y el estadístico de
#' prueba z.
#'
#' @param z_alfa Valor crítico en escala estandarizada
#' @param z Estadístico de prueba
#'
#' @return Un gráfico en ggplot
#'
#' @examples
#' zdist_plot_twotail(z_alfa = 1.96, z = 1)
#'
#' @import dplyr
#' @import ggplot2
#' @import cowplot
#' @import glue
#' @export
ztest_twotails <- function(z_alfa, z){ 
  
  require(glue)
  require(cowplot)
  
  xvals <- seq(-3, 3, length = 1000)
  
  p_valor <- (pnorm(q = abs(z)) * 2) %>% round(4)
  
  tibble(
    
    x = xvals,
    y = dnorm(xvals)
    
  ) %>% 
    
    ggplot(aes(x = x, y = y)) +
    
    stat_function(
      fun   = dnorm, 
      geom  = "area",
      fill  = "orange",
      alpha = 1,
      xlim  = c(-3, -z_alfa)) +
    
    
    stat_function(
      fun   = dnorm, 
      geom  = "area",
      fill  = "orange",
      alpha = 1,
      xlim  = c(z_alfa, 3)) +
    
    
    geom_line(size = 1.4) +
    
    geom_vline(xintercept = 0, linetype = "dotdash") +
    
    geom_vline(xintercept = z,
               linetype   = "dotted") +
    
    geom_vline(xintercept = c(-z_alfa, z_alfa), 
               linetype   = "longdash") +
    
    
    geom_point(aes(x = z, y = 0), size = 2) +
    geom_point(aes(x = -z_alfa, y = 0), size = 2) +
    geom_point(aes(x =  z_alfa, y = 0), size = 2) +
    
    scale_x_continuous(breaks = c(-z_alfa, z, 0, z_alfa),
                       labels = c(round(-z_alfa,3), z, scaleFUN(0), round(z_alfa, 3))) +
    
    
    geom_hline(yintercept = 0) +
    
    
    ggtitle(label    = "Two-Tailed Z-test",
            subtitle = glue("test statistic (z): {z}\ncritical value: {round(z_alfa,3)}\np-value = {p_valor}")) +
    

    theme_cowplot(font_size = 12) +
    
    theme(plot.title   = element_text(size = 16),
          plot.margin  = unit(c(1, 0.5, 1, 0.5),"cm"),
          axis.line    = element_blank(),
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_text(vjust = -0.5, size = 12))
  
} 
