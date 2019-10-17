#' Realiza pruebas de hipotesis para distribuciones t y z
#'
#' Sombrea la región de rechazo para pruebas de una y dos colas
#'
#' @details Esta función grafica en funcion de los parametros de entrada una
#'   distribucion \strong{z} o una \strong{t} y ejecuta el tipo de prueba
#'   seleccionado, el cual puede ser \strong{right}, \strong{left} y \strong{two}
#'
#' @section Aviso: La funcion requiere una validacion de argumentos mas rigurosa.
#'
#' @param sample_size Tamaño de la muestra
#' @param test_statistic Estadistico de contraste estandarizado
#' @param critical_value Valor critico dado un nivel de confianza
#' @param density_function Distribucion a utilizar \code{c("z", "t")}
#' @param tipo_prueba Tipo de prueba a realizar \code{c("right", "left", "two")}
#'
#' @return Un gráfico en ggplot
#'
#' @examples
#' plot_test(sample_size = 110, test_statistic = 2.52, critical_value = 1.645,
#' density_function = "t", tipo_prueba = "two")
#'
#' @import ggplot2
#' @importFrom dplyr case_when
#' @importFrom magrittr %>%
#' @importFrom cowplot theme_cowplot
#' @importFrom glue glue
#'
#' @export

plot_test <- function(sample_size      = 10,
                      test_statistic   = 1,
                      critical_value   = 1.5,
                      density_function = c("z", "t"),
                      tipo_prueba      = c("right", "left", "two")) {

  # crear matriz
  md <- density_matrix(distr = density_function, n = sample_size)


  # calcular p-valor
  pv <- calculate_pvalue(
      statistic      = test_statistic,
      n              = sample_size,
      prob_density   = density_function,
      tails          = tipo_prueba)


  # h reprenta el area a rellenar
  intervalo <- case_when(

    test_statistic < 0 ~ c(critical_value, -3),
    test_statistic > 0 ~ c(critical_value, 3))

  ## obtener titulo del grafico
  titulo <- get_label(density_function, tipo_prueba)


  ## crear capa basica
  grafico_base <- md %>% ggplot(aes(x = vals, y = func))

  ## obtener la region de rechazo con base a los parametros
  region_rechazo <- region_to_fill(density_function,
                                   tipo_prueba,
                                   critical_value,
                                   sample_size,
                                   intervalo)

  # graficar
  grafico_base +
    region_rechazo +


    # dibuja la curva bajo la funcióss de densidad
    geom_line(size = 1.4) +


    # lineas verticales
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0, linetype = "dotdash") +
    geom_vline(xintercept = critical_value, linetype = "longdash") +
    geom_vline(xintercept = test_statistic, linetype = "dotted") +

    # puntos en las intersecciones
    geom_point(aes(x = test_statistic, y = 0), size = 2) +
    geom_point(aes(x = critical_value, y = 0), size = 2) +

    scale_x_continuous(breaks = c(critical_value, test_statistic, 0),
                       labels = c(round(critical_value, 3),
                                  test_statistic, sprintf("%.f", 0))) +


    ggtitle(label    = titulo,
            subtitle = glue("p-value = {pv}")) +


    theme_yunkel()

}
