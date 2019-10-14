#' Aplica un color de fuente a un texto en rmarkdowm
#'
#' Se puede apicar tanto a LaTex como a HTML
#'
#' @details Lo que hace la función es aplicar la función `\\textcolor` en Latex y
#' la etiqueta de font color en HTML.
#'
#' @param x Cadena de texto que se requiere colorear
#' @param color Un color entre comillas de texto
#'
#' @return La cadena de texto con el color especificado
#'
#' @examples
#'color_fuente("hola mundo", "blue")
#'  
#' 
#' @export

color_fuente <-  function(x, color){
  
  outputFormat <-  knitr::opts_knit$get("rmarkdown.pandoc.to")
  
    if(outputFormat == 'latex')
      
      paste("\\textcolor{",color,"}{",x,"}",sep="")
    
    else if(outputFormat == 'html')
      
      paste("<font color='",color,"'>",x,"</font>",sep="")
    
    else
      x
}