#' Grafica una distribución t para dos colas
#'
#' Sombrea la región de rechazo para una prueba de una cola
#'
#' @details Esta función grafica utilizando la distribución t
#'
#' @param t Estadístico t (t-score)
#' @param t_alfa Valor crítico
#'
#' @return Un gráfico en ggplot
#'
#' @examples
#'
#' @import dplyr
#' @import ggplot2
#' @import cowplot
#' @import glue
#' @export
ttest_twotails <- function(n, t_score, nivel_confianza = 0.95) {


  df <- n - 1

  xvals <- seq(-5, 5, length = 1000)

  estadistico_t <- t_score %>%
    abs() %>%
    round(3)

  p_valor <- (pt(
    q = estadistico_t,
    df = df,
    lower.tail = FALSE
  ) * 2) %>%
    round(4)

  valor_critico <- t_critico(
    df = df,
    percentil = nivel_confianza
  ) %>% round(2)


  tibble(
    x = xvals,
    y = dt(xvals, df)
  ) %>%

    ggplot(aes(x = x, y = y)) +

    stat_function(
      fun = dt,
      args = list(df = df),
      geom = "area",
      fill = "gray85",
      alpha = 1,
      xlim = c(-5, -valor_critico)
    ) +

    stat_function(
      fun = dt,
      args = list(df = df),
      geom = "area",
      fill = "gray85",
      alpha = 1,
      xlim = c(valor_critico, 5)
    ) +


    geom_line(size = 2) +

    geom_point(aes(x = estadistico_t, y = 0), size = 3) +
    geom_point(aes(x = -valor_critico, y = 0), size = 3) +
    geom_point(aes(x = valor_critico, y = 0), size = 3) +

    geom_vline(xintercept = estadistico_t, linetype = "dotted") +


    geom_vline(xintercept = c(-valor_critico, valor_critico), linetype = "dashed") +

    geom_hline(yintercept = 0) +

    coord_cartesian(clip = "off") +


    scale_x_continuous(
      breaks = c(
        -valor_critico, 0,
        estadistico_t,
        valor_critico
      ),

      labels = c(
        -valor_critico,
        sprintf("%.f", 0),
        estadistico_t,
        valor_critico
      )
    ) +


    ggtitle(
      label = glue("degrees of freedom: {df}"),
      subtitle = glue("t-score: {round(t_score,2)} | t-critical: {valor_critico} | confidence-level: {nivel_confianza}")
    ) +


    annotate(
      geom = "text",
      x = 3.1075,
      y = 0.375, size = 8,
      label = glue("p-value\n{p_valor}")
    ) +


    theme_cowplot(font_size = 12) +

    theme(
      plot.title = element_text(size = 16),
      plot.margin = unit(c(1, 0.5, 1, 0.5), "cm"),
      axis.line = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),

      axis.text.x = element_text(vjust = -0.5)
    )
}
