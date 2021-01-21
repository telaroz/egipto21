#' Funcion usada para llenar los NAs en la parte de posesiones
#'
#' @param tabla Tabla que recibe
#' @param columna Columna a llenar los NAs
#'
#' @return
#' @export
#'
#' @examples llenar_nas(tabla, 'numero_jugada')
llenar_nas <- function(tabla, columna){

  tabla[, (columna) := get(columna)[1], cumsum(!is.na(get(columna)))]

  primer_no_na <- tabla[!is.na(get(columna))][[columna]][1]

  tabla[is.na(get(columna)), (columna) := primer_no_na]
}
