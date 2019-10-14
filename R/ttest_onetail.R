#' Grafica una distribución t para una cola
#'
#' Sombrea la región de rechazo para una prueba de una cola
#'
#' @details Esta función grafica utilizando la distribución t. El parámetro
#' `lower_tail` dependerá de si la prueba es positiva (derecha) o negativa.  En
#' este caso el planteamiento de la hipótesis alternativa será lo que defina el
#' lado de la cola bajo estudio. Recordar que, por convención, siempre nos
#' interesará sombrear el área de rechazo y dejar la región de aceptación sin
#' sombrear.
#'
#' @param t Estadístico t (t-score)
#' @param t_alfa Valor crítico
#'
#' @return Un gráfico en ggplot
#'
#' @examples
#'
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom glue glue
#' @export
ttest_onetail <- function(n, t, t_alfa, lower_tail = FALSE){

  xvals <- seq(-3, 3, length = 1000)

  t_alfa <- t_alfa %>% round(3)


  p_valor <- case_when(

    lower_tail = TRUE  ~ pt(q = t, df = n - 1, lower.tail = TRUE) %>% round(4),
    lower_tail = FALSE ~ pt(q = t, df = n - 1, lower.tail = FALSE) %>% round(4),

  )


  # h representa la posición relativa
  h <- case_when(

    lower_tail == TRUE  ~ c(t_alfa, -3),
    lower_tail == FALSE ~ c(t_alfa, 3))

  # crear tibble
  g <- tibble(

    x = xvals,
    y = dt(xvals, n-1)

  )

  g %>%

    ggplot(aes(x = x, y = y)) +

    stat_function(
      fun = dt,
      args = list(df = n - 1),
      geom = "area",
      fill = "orange",
      alpha = 0.5,
      xlim = h) +


    # dibuja la curva bajo la función de densidad
    geom_line(size = 1.4) +


    # lineas verticales
    geom_hline(yintercept = 0) +

    geom_vline(xintercept = 0, linetype = "dotdash") +

    geom_vline(xintercept = t_alfa, linetype = "longdash") +

    geom_vline(xintercept = t, linetype = "dotted") +


    # puntos en las intersecciones
    geom_point(aes(x = t,      y = 0), size = 2) +
    geom_point(aes(x = t_alfa, y = 0), size = 2) +


    scale_x_continuous(breaks = c(t_alfa, t, 0),
                       labels = c(round(t_alfa,3), t, sprintf("%.f", 0))) +


    ggtitle(label    = "One-Tailed T-test",
            subtitle = glue("test statistic (t): {t}\ncritical value: {round(t_alfa,3)}\np-value = {p_valor}")) +



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
