#' Aplica un color de fuente a un texto en rmarkdowm
#'
#' Se puede apicar tanto a LaTex como a HTML
#'
#' @details Lo que hace la función es aplicar la función en Latex y
#'  la etiqueta de font color en HTML.
#'
#' @param x Cadena de texto que se requiere colorear
#' @param color Un color entre comillas de texto
#'
#' @return La cadena de texto con el color especificado
#'
#' @examples
#' coloring_font(x = "hola mundo", color = "blue")
#' @import knitr
#' @export

coloring_font <-  function(x, color){

  outputFormat <-  knitr::opts_knit$get("rmarkdown.pandoc.to")

    if(outputFormat == 'latex')

      paste("\\textcolor{",color,"}{",x,"}",sep="")

    else if(outputFormat == 'html')

      paste("<font color='",color,"'>",x,"</font>",sep="")

    else
      x
}
