#' Sombrea las regiones de la distribucion
#'
#' Rellan con un color en funcion de la distribucion seleccionada y del tipo de
#' prueba
#'
#' @details Esta función aplica un relleno de color en funcion de si es una
#'   distribucion t o z, asi como si es una prueba unilateral o bilateral.
#'
#' @param density_function Distribucion a utilizar \code{c("z", "t")}
#' @param tipo_prueba Tipo de prueba a realizar \code{c("right", "left", "two")}
#' @param critical_value Valor critico dado un nivel de confianza
#' @param sample_size Tamaño de la muestra
#' @param intervalo El rango de los limetes de sombreado a nivel de eje x
#'
#' @return Un objeto de tipo ggplot
#'
#' @examples
#' plot_test(sample_size = 110, test_statistic = 2.52, critical_value = 1.645,
#' density_function = "t", tipo_prueba = "two")
#'
#' @import ggplot2
#' @importFrom dplyr case_when
#' @export

region_to_fill <- function(density_function,
                           tipo_prueba,
                           critical_value,
                           sample_size,
                           intervalo) {

  if (density_function == "t" & tipo_prueba %in% c("right", "left")){

    p <-  stat_function(
      fun = dt,
      args = list(df = sample_size - 1),
      geom = "area",
      fill = "orange",
      alpha = 0.5,
      xlim = intervalo)

    tipo_prueba <- "One Tailed T-test"

  } else if (density_function == "z" & tipo_prueba %in% c("right", "left")){

    p <- stat_function(
      fun = dnorm,
      geom = "area",
      fill = "orange",
      alpha = 1,
      xlim = intervalo)

    tipo_prueba <- "One Tailed Z-test"

  } else if (density_function == "z" & tipo_prueba == "two"){

    p <- list(

      stat_function(
        fun   = dnorm,
        geom  = "area",
        fill  = "orange",
        alpha = 1,
        xlim  = c(-3, -critical_value)),


      stat_function(
        fun   = dnorm,
        geom  = "area",
        fill  = "orange",
        alpha = 1,
        xlim  = c(critical_value, 3)),

      geom_vline(xintercept = c(-critical_value, critical_value), linetype = "dashed"),
      geom_vline(xintercept = c(-critical_value, critical_value), linetype = "longdash")

    )

    tipo_prueba <- "Two Tailed T-test"


  } else if (density_function == "t" & tipo_prueba == "two") {

    p <- list(

      stat_function(
        fun = dt,
        args = list(df = sample_size - 1),
        geom = "area",
        fill = "orange",
        alpha = 1,
        xlim = c(-3, -critical_value)),


      stat_function(
        fun = dt,
        args = list(df = sample_size - 1),
        geom = "area",
        fill = "orange",
        alpha = 1,
        xlim = c(critical_value, 3)),

      geom_vline(xintercept = c(-critical_value, critical_value), linetype = "dashed"),
      geom_vline(xintercept = c(-critical_value, critical_value), linetype = "longdash")

    )


  } else {
    stop("Valor invalido")
  }

  p

}
