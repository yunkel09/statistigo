#' Calcula el p-valor
#'
#' Con base a los parametros de la funcion se calcula el p-valor
#'
#' @details Esta funcion se calcula para poder incorporar el p-value dentro del
#'   grafico.
#'
#' @param statistic Estadistico de contraste estandarizado
#' @param n Tamaño de la muestra
#' @param prob_density Distribucion a utilizar \code{c("z", "t")}
#' @param tails Tipo de prueba a realizar \code{c("right", "left", "two")}
#'
#' @return Un vector de tipo numerico con el valor p
#'
#' @examples
#' calculate_pvalue(statistic = 3.08, n = 50, prob_density = "z", tails =
#' "right")
#'
#' @importFrom dplyr case_when
#' @importFrom magrittr %>%
#' @export

calculate_pvalue <- function(statistic      = 1,
                             n              = 10,
                             prob_density   = c("z", "t"),
                             tails          = c("right", "left", "two")) {


  case_when(


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Z-test                                                                  ####


    # condicion 1: right-tailed z-test
    tails        == "right" &
    prob_density == "z" ~

    # lower.tail = FALSE por que el area de rechazo esta hacia la derecha
      pnorm(q = statistic, lower.tail = FALSE) %>% round(4),


    # condicion 2: left-tailed test z-test
    tails        == "left" &
    prob_density == "z" ~

      # lower.tail = TRUE por que el area de rechazo esta hacia la izquierda
      pnorm(q = statistic, lower.tail = TRUE) %>% round(4),


    # condicion 3: two-tailed test z-test
    tails        == "two" &
    prob_density == "z" ~

      # es indistinto el signo del estadistico de prueba
      (pnorm(q = abs(statistic), lower.tail = FALSE) * 2) %>% round(4),



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### T-test                                                                  ####


    # condicion 4: right-tailed t-test
    tails        == "right" &
    prob_density == "t" ~

      # lower.tail = FALSE por que el area de rechazo esta hacia la derecha
      pt(q = statistic,
         df = n - 1,
         lower.tail = FALSE) %>% round(4),



    # condicion 5: left-tailed t-test
    tails        == "left" &
    prob_density == "t" ~

      # lower.tail = TRUE por que el area de rechazo esta hacia la izquierda
      pt(q = statistic,
         df = n - 1,
         lower.tail = TRUE) %>% round(4),



    # condicion 6: two-tailed test t-test
    tails        == "two" &
    prob_density == "t" ~

      # es indistinto el signo del estadistico de prueba
      (pt(q = abs(statistic),
          df = (n - 1),
          lower.tail = FALSE) * 2) %>% round(4)



  )



}
