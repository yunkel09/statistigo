#' Calcula el intervalo de confianza para un determinado nivel de confianza
#'
#' Funciona solo cuando se conoce la varianza de la población.
#'
#' @details Esta función solo aplica para calcular el intervalo cuando se conoce
#' la desviación estandar de la población, ya que de lo contrario requerirá que
#' se utilice el valor crítico utilizando la distribución t.
#'
#' @param x Media muestral
#' @param cl Nivel de confianza
#' @param n Tamaño de la muestra
#' @param s Desviación estándar de la población
#' @param s Desviación estándar de la población
#'
#' @return Un vector de longitud 2 con el intervalo
#'
#' @examples
#' calcular_intervalo(x = 100, ci = 0.95, n = 50, s = 3, tails = 2)
#' @importFrom dplyr case_when
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
