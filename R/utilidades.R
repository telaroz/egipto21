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


#' Función para descargar pdfs.
#'
#' @param enlace Página desde donde queremos descargar los partidos
#'
#' @return
#' @export
#'
#' @examples scrape_from_ihf(enlace = 'https://www.ihf.info/competitions/men/308/27th-ihf-men039s-world-championship-2021/22415/match-center/23765', carpeta = 'matches')
scrape_from_ihf <- function(enlace, carpeta){
  enlace %>%
    xml2::read_html() %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    stringr::str_subset("\\.pdf|\\.PDF") %>%
    purrr::walk2(., paste0(carpeta,'/', basename(.) %>% stringr::str_remove('[?=].*')), download.file, mode = "wb")

}
