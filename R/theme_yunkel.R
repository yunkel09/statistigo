#' Tema especifico para graficar distribuciones
#'
#' Define espacios superior para una mejor impresion sobre rmarkdown
#'
#' @description Se construye a partir de cowplot
#'
#' @return Un gráfico en ggplot
#'
#' @examples
#' plot_test(sample_size = 110, test_statistic = 2.52, critical_value = 1.645,
#' density_function = "t", tipo_prueba = "two")
#'
#' @import ggplot2
#' @importFrom cowplot theme_cowplot
#'
#' @export

theme_yunkel <- function(){


    theme_cowplot(font_size = 12) +

    theme(plot.title   = element_text(size = 16),
          plot.margin  = unit(c(1, 0.5, 1, 0.5),"cm"),
          axis.line    = element_blank(),
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(vjust = -0.5, size = 12))

}
