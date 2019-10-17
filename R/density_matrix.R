#' Crear una matriz con base a una funcion de densidad especificada
#'
#' Sombrea la region de rechazo para una prueba de una cola
#'
#' @details Esta funcion grafica utilizando la distribucion t. El parametro
#' \code{lower_tail} dependera de si la prueba es positiva derecha o negativa.  En
#' este caso el planteamiento de la hipotesis alternativa sera lo que defina el
#' lado de la cola bajo estudio. Recordar que, por convencion, siempre nos
#' interesara sombrear el área de rechazo y dejar la region de aceptacion sin
#' sombrear.
#'
#' @param distr EDistribucion a utilizar \code{c("z", "t")}
#' @param n Tamano de la muestra
#'
#' @return Un tibble con dos columnas
#'
#' @examples
#' density_matrix(distr = "t", n = 30)
#'
#' @importFrom dplyr case_when
#' @importFrom dplyr tibble
#'
#' @export
density_matrix <- function(distr = "t", n = 20) {

  xvals <- seq(-3, 3, length = 1000)

  if(!is.character(distr))
    stop("La distribución debe ser una 't' o una 'z'")

  if(!distr %in% c("t", "z"))
    stop("Debe elegir entre una distribucion t o una z")



  d <- case_when(

    distr == "t" ~ dt(xvals, n - 1),
    distr == "z" ~ dnorm(xvals)

  )


  tibble(

    vals = xvals,
    func = d

  )

}


