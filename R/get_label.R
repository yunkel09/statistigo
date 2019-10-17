#' Obtiene la etiqueta para el titulo
#'
#' En funcion de los parametros \code{density_function} y \code{tipo_prueba}
#'
#' @details Esta funcion unicamente obtiene la etiqueta correcta para titulo del
#'   grafico
#'
#' @param density_function Distribucion a utilizar \code{c("z", "t")}
#' @param tipo_prueba Tipo de prueba a realizar \code{c("right", "left", "two")}
#'
#' @return Un vector de tipo caracter
#'
#' @examples
#' get_label("t", "right")
#'
#' @importFrom dplyr case_when
#' @export

get_label <- function(density_function, tipo_prueba){
  case_when(
    density_function == "t" & tipo_prueba == "right" ~ "Right-Tailed T-test",
    density_function == "t" & tipo_prueba == "left"  ~ "Left-Tailed T-test",
    density_function == "t" & tipo_prueba == "two" ~ "Two-Tailed T-test",

    density_function == "z" & tipo_prueba == "right" ~ "Right-Tailed Z-test",
    density_function == "z" & tipo_prueba == "left" ~ "Left-Tailed Z-test",
    density_function == "z" & tipo_prueba == "two" ~ "Two-Tailed Z-test",
    TRUE ~ "VALOR INCORRECTO"
  )

}
