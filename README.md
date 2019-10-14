# statistigo
Funciones estadísticas para optimizar el aprendizaje


1. Modificar el archivo `DESCRIPTION`

  1.1 Si colocas los paquetes en `Depends` entonces los paquetes se cargarán 
  automaticamente. 

2. En las opciones de configuración seleccionar todas las opciones para que
Roxygen se encarge de todo, especialmente de crear el archivo `NAMESPACE`.

3. Agregar comentarios roxygen a todas las funciones `.R`

  3.1 Para refererirse a bloques de código utilizar \code{ttest_onetail.R}
  3.2 Utilizar Ctrl + Shift + / para limitar el texto a 80 caracteres
  3.3 Agregar @section adicional: para extender los detalles de que hace la
      función.
  3.4 @seealso \url{http://www.r-project.org} para apuntar a alguna url o 
      \code{\link{functioname}} para linkear a alguna función interna

4. Correr `devtools::document()` o Ctrl + Shift + D

  4.1 Verificar que los archivos .Rd se hayan creado en la carpeta \man
  4.2 Debido a que el `NAMESPACE` se crea con roxygen es importante mantener
      actualizada la documentacion.

5. Verificar que la documentación está correcta con `?`

6. Correr todo con `devtools::load_all()` o Ctrl + Shift + L

  6.1 Si los archivos .Rd no están creados, Roxygen no será capaz de crear el
      archivo `NAMESPACE`
