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
ztest_onetail <- function(z, z_alfa, cola_inferior = TRUE){ 
  
  xvals <- seq(-3, 3, length = 1000) 
  
  p_valor <- case_when(
    
    cola_inferior = TRUE ~ pnorm(q = z, lower.tail = TRUE) %>% round(4), 
    cola_inferior = FALSE ~ pnorm(q = z, lower.tail = FALSE) %>% round(4) 
    
  )
  
  
  h <- case_when(
    
    cola_inferior == TRUE  ~ c(-3, z_alfa),
    cola_inferior == FALSE ~ c(z_alfa, 3))
  
  tibble(
    
    x = xvals,
    y = dnorm(xvals)
    
  ) %>% 
    
    ggplot(aes(x = x, y = y)) +
    
    stat_function(
      fun = dnorm, 
      geom = "area",
      fill = "orange",
      alpha = 1,
      xlim = h) +
    
    
    
    geom_line(size = 1.4) +
    
    geom_hline(yintercept = 0) +
    
    geom_vline(xintercept = 0, linetype = "dotdash") +
    
    geom_vline(xintercept = z, linetype = "dotted") +
    
    geom_vline(xintercept = z_alfa, linetype = "longdash") +
    
    
    geom_point(aes(x = z, y = 0), size = 2) +
    geom_point(aes(x = z_alfa, y = 0), size = 2) +
    
    scale_x_continuous(breaks = c(z_alfa, z, 0),
                       labels = c(round(z_alfa,3), z, scaleFUN(0))) +
    
    
    
    
    
    ggtitle(label    = "One-Tailed Z-test",
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
