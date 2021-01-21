#' Convierte la planilla Play by Play de la página de la IHF en una tabla tidy
#'
#' @param input Directorio donde se encuentra el archivo pbp.
#'
#' @return Una tabla con el partido en formato tidy (una observación por fila)
#' @export
#'
#' @examples generar_pbp_tidy('01pbp.pdf')
generar_pbp_tidy <- function(input){
  texto <- pdftools::pdf_text(input) %>%
    readr::read_lines()

  numero_partido_ext <- texto[stringr::str_detect(texto, 'Match No: ')][1] %>%
    stringr::str_extract('Match No: \\d+')  %>% # Esto es para asegurarnos que verdaderamente saquemos el número del partido y no otras cosas.
    stringr::str_extract('\\d+') %>%
    as.numeric()
  # 1.1 - Limpieza de jugadores ---------------------------------------------


  tables <- tabulizer::extract_tables(input, method = 'stream')


  equipos <- purrr::keep(tables, ~ .x[2,1] == '') %>%
    data.table::as.data.table()

  nombres_equipos <- unique(equipos$V1[equipos$V1 != ''])

  pbp <-  purrr::keep(tables, ~ .x[2,1] != '') %>%
    purrr::map(~data.table::as.data.table(.x))

  pbp_limpio <- pbp %>%
    purrr::keep(~ .x[2, V4] == 'Score')

  if(length(pbp_limpio) != 0){
    pbp_limpio <- pbp_limpio %>%
      purrr::when(ncol(.) == 8 ~ .[, V7 := paste0(V7, V8)], ~ .) %>%
      purrr::map_df(~ .x[,1:7]) %>%
      data.table::setnames(colnames(.), c('tiempo', 'numero_casa', 'accion_casa', 'marcador', 'ventaja_casa', 'numero_visita', 'accion_visita'))

  }else{
    pbp_limpio <- NULL
  }

  pbp_sucio <- NULL
  pbp_sucio1 <- NULL
  pbp_sucio2 <- NULL


  if(length(pbp %>%
            purrr::keep(~ .x[2, V4] == 'Score')) != length(pbp)){

    pbp_sucio <- pbp %>%
      purrr::keep(~ .x[2, V4] != 'Score')

    pbp_sucio1 <- pbp_sucio %>%
      purrr::keep(~length(.x) == 9)

    if(length(pbp_sucio1) != 0){
      pbp_sucio1 <- pbp_sucio1 %>%
        purrr::walk(~ .x[, V4 := NULL][, V9 := NULL]) %>%
        data.table::rbindlist() %>%
        data.table::setnames(colnames(.), c('tiempo', 'numero_casa', 'accion_casa', 'marcador', 'ventaja_casa', 'numero_visita', 'accion_visita'))

    }else{
      pbp_sucio1 <- NULL
    }

    pbp_sucio2 <- pbp_sucio %>%
      purrr::keep(~ length(.x) == 8)

    if(length(pbp_sucio2) != 0){
      pbp_sucio2 <- pbp_sucio2 %>%
        purrr::walk(~ .x[, V3 := paste0(V3,V4)][, V4 := NULL]) %>%
        purrr::map_df(~ .x[,1:7]) %>%
        data.table::setnames(colnames(.), c('tiempo', 'numero_casa', 'accion_casa', 'marcador', 'ventaja_casa', 'numero_visita', 'accion_visita'))

    }else{
      pbp_sucio2 <- NULL
    }
  }


  pbp <- data.table::rbindlist(list(pbp_limpio, pbp_sucio1, pbp_sucio2))



  # Agregar si 1ero o 2do tiempo

  pbp[stringr::str_detect(accion_casa, 'Goalkeeper') & stringr::str_detect(accion_visita, 'Goalkeeper'),
      mitad := data.table::fifelse(tiempo == '0:00', 1, 2)]


  pbp[, tiempo_numerico := as.numeric(lubridate::ms(tiempo))]
  pbp <- pbp[order(tiempo_numerico)]
  pbp[, mitad := mitad[1L], cumsum(!is.na(mitad))]


  equipos[, V1 := V1[1L] , cumsum(V1 != '')]

  data.table::rbindlist(list(equipos[,.(V1, V2, V4, V6, V8)], equipos[,.(V1, V3, V5, V7, V9)]))

  tidy_equipo <- data.table::melt(equipos, id.vars = c('V1'),
                                  measure.vars = c('V2', 'V4', 'V6', 'V8'),
                                  value.name = 'numero',
                                  variable.name = 'columna_numero' ) %>%
    cbind(data.table::melt(equipos, id.vars = c('V1'),
                           measure.vars = c('V3', 'V5', 'V7', 'V9'),
                           value.name = 'nombre',
                           variable.name = 'columna_jugador' ))

  tidy_equipo[, V1 := NULL]
  tidy_equipo[, columna_numero := NULL]
  tidy_equipo[, columna_jugador := NULL]
  tidy_equipo <- tidy_equipo[numero != '']

  colnames(tidy_equipo) <- c('numero', 'equipo', 'nombre')
  data.table::setcolorder(tidy_equipo, c('equipo', 'numero', 'nombre'))

  cuerpo_tecnico <- tidy_equipo[numero %in% LETTERS][order(equipo, numero)]
  jugadores <- tidy_equipo[!numero %in% LETTERS][order(equipo, numero)]

  jugadores[, nombre_planilla := stringr::str_remove_all(nombre, '[a-z]')]

  # 1.1 - Limpieza de PBP ------------------------------------------------
  # Primero lo hacemos solamente para casa. Para visita es exactamente lo mismo
  #data.table::setnames(pbp, paste0('V', 1:7), c('tiempo', 'numero_casa', 'accion_casa', 'marcador', 'ventaja_casa', 'numero_visita', 'accion_visita'))

  pbpv <- pbp[, .(tiempo, numero = numero_visita, accion = accion_visita, mitad)]
  pbpc <- pbp[, .(tiempo, numero = numero_casa, accion = accion_casa, mitad)]

  func_tidy_pbp_por_equipo <- function(tabla,  casa = TRUE, nombre_equipo, numero_partido = numero_partido_ext){

    tabla[stringr::str_detect(tiempo, '0:00|30:00') & stringr::str_detect(accion, 'Goalkeeper')]
    tabla[, accion := stringr::str_squish(accion)]

    tabla <- tabla[stringr::str_detect(tiempo, '\\d')]


    tabla[stringr::str_detect(tiempo, '0:00') & stringr::str_detect(accion, 'Goalkeeper'), portero := numero]
    tabla[stringr::str_detect(tiempo, '30:00') & stringr::str_detect(accion, 'Goalkeeper'), portero := numero]

    tabla[stringr::str_detect(accion, 'Empty goal'), portero := 'Empty goal']
    tabla[stringr::str_detect(accion, 'Goalkeeper back'), portero := numero]
    tabla[, portero := portero[1L], cumsum(!is.na(portero))]


    # Describe Goals

    tabla[stringr::str_detect(accion, '\\bGoal\\b'), ':='(asistencia_numero = stringr::str_extract(accion, '\\(([^)]+)\\)') %>% stringr::str_extract('\\d+'),
                                                          gol_numero = numero,
                                                          tipo_de_gol = stringr::str_trim(stringr::str_remove(accion, '\\(([^)]+)\\)')))]

    # Describe shots (not Goals)

    tabla[stringr::str_detect(accion, '\\bShot\\b|Penalty shot') & stringr::str_detect(accion, 'Goal', negate = TRUE) , ':='(tiro_numero = numero,
                                                                                                                             tipo_de_tiro = stringr::str_remove(accion, paste0(jugadores$nombre_planilla, collapse = '|')))]

    desc_tiros <- egipto21::descripcion_tiros

    tabla[, posicion_marco := stringr::str_extract(accion, paste0(desc_tiros$posicion_marco[desc_tiros$posicion_marco != ''], collapse = '|'))]
    tabla[, posicion_tiro := stringr::str_extract(accion, paste0(desc_tiros$posicion_tiro[desc_tiros$posicion_tiro != ''], collapse = '|'))]

    tabla[stringr::str_detect(accion, 'post'), post := 1]
    tabla[stringr::str_detect(accion, 'saved'), saved := 1]
    tabla[!is.na(tipo_de_gol), gol := 1]
    tabla[, gol := as.numeric(gol)]

    tabla[, ':='(posicion_marco_vertical = stringr::str_extract(posicion_marco, 'bottom|top|middle'),
                 posicion_marco_horizontal = stringr::str_extract(posicion_marco, 'left|centre|right'))]


    tabla[stringr::str_detect(accion, '7m caused'), numero_causa_7m := numero]
    tabla[stringr::str_detect(accion, '7m received'), numero_recibe_7m := numero]


    tabla[stringr::str_detect(accion, 'Turnover'), turnover := numero]
    tabla[stringr::str_detect(accion, 'Steal'), robo := numero]
    tabla[stringr::str_detect(accion, '2-minutes suspension'), suspension := numero]



    tabla[, tiempo_numerico := as.numeric(lubridate::ms(tiempo))]
    tabla[, es_casa := casa]
    tabla[, equipo := nombre_equipo]
    tabla[, id := numero_partido]


    tabla[, cantidad_jugadores_campo := 6]


    tabla[!is.na(suspension), inicia_suspension := tiempo_numerico]
    tabla[!is.na(suspension), termina_suspension := tiempo_numerico + 120]

    tabla[, no_jugada := 1:.N]

    auxiliar <- tabla[tabla, .(list(no_jugada)), on = .(tiempo_numerico > inicia_suspension, tiempo_numerico <= termina_suspension), by = .EACHI]



    cantidad_jugadores_menos <- (auxiliar[!is.na(V1)]$V1 %>% unlist() %>% data.table::data.table(no_jugada = .))[,.N,no_jugada]

    tabla[cantidad_jugadores_menos, cantidad_suspendidos := -i.N, on = 'no_jugada']
    tabla[is.na(cantidad_suspendidos), cantidad_suspendidos := 0]

    tabla[, sin_portero := as.numeric(portero == 'Empty goal')]

    tabla[, cantidad_jugadores_campo_real := cantidad_jugadores_campo + cantidad_suspendidos + sin_portero]
    tabla[, cantidad_maxima_jugadores := cantidad_jugadores_campo + cantidad_suspendidos]
    tabla[, cantidad_jugadores_campo_real := pmin(cantidad_jugadores_campo_real, cantidad_maxima_jugadores)]
    tabla <- tabla[order(tiempo_numerico)]

    tabla[,cantidad_jugadores_campo := NULL]
    tabla[,cantidad_maxima_jugadores := NULL]
    return(tabla[])

  }



  #final <- cbind(func_tidy_equipo(pbpc), func_tidy_equipo(pbpv, casa = FALSE))
  final <- rbind(func_tidy_pbp_por_equipo(tabla = pbpc, casa = TRUE, nombre_equipo = nombres_equipos[1]),
                 func_tidy_pbp_por_equipo(tabla = pbpv, casa = FALSE, nombre_equipo = nombres_equipos[2]))

  final <- final[order(mitad, tiempo_numerico)]

  final[, linea := 1:.N]

  pos <- final

  posible_cambio_posesion <- c('\\bGoal\\b', 'Technical', 'Turnover', 'missed', 'Shot', 'Steal', 'Block','saved')

  posible_cambio_posesion_para_secuencias <- c('\\bGoal\\b', 'Technical', 'Turnover', 'missed', 'Shot', 'saved')





  la_tiene <- c('\\bGoal\\b', 'Technical', 'Turnover', 'missed', 'Shot', '7m received', 'Team timeout', 'saved')

  no_la_tiene <- c('Steal', 'Block', '7m caused')



  pos[, lt := as.numeric(stringr::str_detect(accion, paste0(la_tiene, collapse = '|')))]

  pos[, nlt := as.numeric(stringr::str_detect(accion, paste0(no_la_tiene, collapse = '|')))]



  pos <- pos[accion != ''
  ][tiempo_numerico != 0
  ][!(tiempo_numerico == 1800 & stringr::str_detect(accion, 'Goalkeeper'))]



  equipos <- unique(pos$equipo)



  pos[, posesion := data.table::fifelse(lt == 1, equipo, setdiff(equipos, equipo)), 1:nrow(pos)]

  pos[lt == 0 & nlt == 0, posesion := NA]




  pos <- pos[!is.na(posesion)]

  pos[, numero_de_posesion := data.table::rleid(posesion, mitad)]


  pos[, fin_posesion := data.table::last(tiempo), .(posesion, numero_de_posesion)]

  pos[, numero_posesion_anterior := numero_de_posesion - 1]

  aux <- unique(pos, by = c('numero_de_posesion', 'fin_posesion'))


  pos[aux[aux, .(numero_de_posesion, inicio_posesion = fin_posesion), on = .(numero_de_posesion == numero_posesion_anterior)]
      , inicio_posesion := i.inicio_posesion, on = 'numero_de_posesion']


  pos[numero_de_posesion == 1, inicio_posesion := '0:00']
  pos[numero_de_posesion == max(numero_de_posesion), fin_posesion := '60:00']

  pos[, c('lt', 'nlt', 'numero_posesion_anterior') := NULL]

  data.table::setcolorder(pos, colnames(pos)[c(1:8, 10, 9)])


  pos <- pos[,.(linea, posesion, numero_de_posesion, inicio_posesion, fin_posesion)]


  listo <- data.table::merge.data.table(final, pos, all.x = TRUE, by = 'linea')

  listo[, numero_de_posesion_preliminar := numero_de_posesion]

  purrr::walk(c('posesion', 'numero_de_posesion', 'inicio_posesion', 'fin_posesion'), ~ egipto21::llenar_nas(listo, .x))


  listo <- listo[accion != '', .(id_partido = id, tiempo, tiempo_numerico, mitad, accion, numero, equipo,
                                 portero, asistencia_numero, gol_numero, asistencia_numero,
                                 tiro_numero, gol, posicion_marco, posicion_tiro, post, saved,
                                 posicion_marco_vertical, posicion_marco_horizontal, numero_causa_7m,
                                 numero_recibe_7m, turnover, robo, suspension, es_casa, equipo, cantidad_suspendidos,
                                 sin_portero, cantidad_jugadores_campo = cantidad_jugadores_campo_real, posesion,
                                 numero_posesion = numero_de_posesion, inicio_posesion, fin_posesion)]

    return(listo)
}


#' Jugadores y Cuerpo Técnico de un partido en particular
#'
#' @param input Directorio donde se encuentra el archivo pbp.
#'
#' @return Una lista de dos tablas: jugadores y cuerpo técnico
#' @export
#'
#' @examples jug_y_dt('01pbp.pdf')
jug_y_dt <- function(input){

  texto <- pdftools::pdf_text(input) %>%
    readr::read_lines()

  numero_partido_ext <- texto[stringr::str_detect(texto, 'Match No: ')][1] %>%
    stringr::str_extract('Match No: \\d+')  %>% # Esto es para asegurarnos que verdaderamente saquemos el número del partido y no otras cosas.
    stringr::str_extract('\\d+') %>%
    as.numeric()
  # 1.1 - Limpieza de jugadores ---------------------------------------------



  tables <- tabulizer::extract_tables(input, method = 'stream')


  equipos <- purrr::keep(tables, ~ .x[2,1] == '') %>%
    data.table::as.data.table()

  nombres_equipos <- unique(equipos$V1[equipos$V1 != ''])

  equipos[, V1 := V1[1L] , cumsum(V1 != '')]

  data.table::rbindlist(list(equipos[,.(V1, V2, V4, V6, V8)], equipos[,.(V1, V3, V5, V7, V9)]))

  tidy_equipo <- data.table::melt(equipos, id.vars = c('V1'),
                                  measure.vars = c('V2', 'V4', 'V6', 'V8'),
                                  value.name = 'numero',
                                  variable.name = 'columna_numero' ) %>%
    cbind(data.table::melt(equipos, id.vars = c('V1'),
                           measure.vars = c('V3', 'V5', 'V7', 'V9'),
                           value.name = 'nombre',
                           variable.name = 'columna_jugador' ))

  tidy_equipo[, V1 := NULL]
  tidy_equipo[, columna_numero := NULL]
  tidy_equipo[, columna_jugador := NULL]
  tidy_equipo <- tidy_equipo[numero != '']

  colnames(tidy_equipo) <- c('numero', 'equipo', 'nombre')
  data.table::setcolorder(tidy_equipo, c('equipo', 'numero', 'nombre'))

  cuerpo_tecnico <- tidy_equipo[numero %in% LETTERS][order(equipo, numero)]
  jugadores <- tidy_equipo[!numero %in% LETTERS][order(equipo, numero)]

  jugadores[, nombre_planilla := stringr::str_remove_all(nombre, '[a-z]')]


  jugadores[, numero_partido := numero_partido_ext]
  cuerpo_tecnico[, numero_partido := numero_partido_ext]

  return(list(jugadores, cuerpo_tecnico))
}
