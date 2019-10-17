#' Calcula el intervalo de confianza para un determinado nivel de confianza
#'
#' Funciona solo cuando se conoce la varianza de la poblacion.
#'
#' @details Esta funcion solo aplica para calcular el intervalo cuando se conoce
#' la desviacion estandar de la poblacion, ya que de lo contrario requerira que
#' se utilice el valor critico utilizando la distribucion t.
#'
#' @param x Media muestral
#' @param cl Nivel de confianza
#' @param n Tamano de la muestra
#' @param s Desviación estándar de la poblacion
#' @param tails Desviacion estándar de la poblacion
#'
#' @return Un vector de longitud 2 con el intervalo
#'
#' @examples
#' calcular_intervalo(x = 100, cl = 0.95, n = 50, s = 3, tails = 2)
#'
#' @importFrom dplyr case_when
#' @importFrom magrittr %>%
#' @export
confidence_interval <- function(x, cl, n, s, tails = 2){

  # calcular valor crítico
  z_alfa <- case_when(

    tails == 2 ~ qnorm(((1 - cl)/2)),
    tails == 1 ~ qnorm(1 - cl)
  )

  # error estandar
  se <-  s / sqrt(n)

  ci <- (x + c(1, -1) * z_alfa * se) %>% round(3)

  ci[order(ci)]

}
