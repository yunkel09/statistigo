#' Calcula el cuantíl crítico
#'
#' Funciona para una distribución de dos colas
#'
#' @details Esta función solo aplica para calcular el cuantíl crítico en el caso
#' de una prueba de dos colas, ya que en una prueba de una cola solo es necesario
#' ingresar el nivel de confianza directamente.
#'
#' @param cl Nivel de confianza
#' @param tails Cantidad de colas
#'
#' @return Un vector de longitud 1 con el nivel de confianza ajustado
#'
#' @examples
#' cuantil_critico(cl = 0.90, tails = 2)
#'
#' @export
critical_quantile <- function(cl = 0.95, tails = 2){

  alpha <- (1 - cl)

  if (tails == 2)

  {

    ((1 + (1 - alpha)) / 2)

  } else {

    cl

    }

}
