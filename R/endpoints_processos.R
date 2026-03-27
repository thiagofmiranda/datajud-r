#' Search a process by its number
#'
#' @param cfg List. Configuration object from [datajud_config()].
#' @param numero_processo Character. Full process number.
#' @param ... Additional arguments passed to [datajud_search()].
#'
#' @return A tibble with matching processes.
#' @export
processo_por_numero <- function(cfg, numero_processo, ...) {

  body <- build_query(
    query_term("numeroProcesso", numero_processo)
  )

  datajud_search(cfg, body, ...)

}

#' Search processes by class name
#'
#' @param cfg List. Configuration object from [datajud_config()].
#' @param classe Character. Class name (full-text search).
#' @param ... Additional arguments passed to [datajud_search()].
#'
#' @return A tibble with matching processes.
#' @export
processos_por_classe <- function(cfg, classe, ...) {

  body <- build_query(
    query_match("classe.nome", classe)
  )

  datajud_search(cfg, body, ...)

}

#' Search processes by filing date range
#'
#' @param cfg List. Configuration object from [datajud_config()].
#' @param data_inicio Character. Start date in `"YYYY-MM-DD"` format.
#' @param data_fim Character or NULL. End date in `"YYYY-MM-DD"` format.
#' @param ... Additional arguments passed to [datajud_search()].
#'
#' @return A tibble with matching processes.
#' @export
processos_por_data <- function(cfg,
                               data_inicio,
                               data_fim = NULL,
                               ...) {

  body <- build_query(
    query_range("dataAjuizamento", gte = data_inicio, lte = data_fim)
  )

  datajud_search(cfg, body, ...)

}

#' Search processes with optional class and date filters
#'
#' @param cfg List. Configuration object from [datajud_config()].
#' @param classe Character or NULL. Class name filter.
#' @param data_inicio Character or NULL. Start date filter.
#' @param data_fim Character or NULL. End date filter.
#' @param ... Additional arguments passed to [datajud_search()].
#'
#' @return A tibble with matching processes.
#' @export
processos_busca <- function(cfg,
                            classe      = NULL,
                            data_inicio = NULL,
                            data_fim    = NULL,
                            ...) {

  filters <- list()

  if (!is.null(classe)) {
    filters <- append(filters, list(query_match("classe.nome", classe)))
  }

  if (!is.null(data_inicio) || !is.null(data_fim)) {
    filters <- append(filters, list(
      query_range("dataAjuizamento", gte = data_inicio, lte = data_fim)
    ))
  }

  body <- if (length(filters) == 0) {
    build_query(query_match_all())
  } else {
    build_query(query_bool(filter = filters))
  }

  datajud_search(cfg, body, ...)

}

#' Retrieve all processes from a tribunal
#'
#' @param cfg List. Configuration object from [datajud_config()].
#' @param ... Additional arguments passed to [datajud_search()].
#'
#' @return A tibble with all processes.
#' @export
processos <- function(cfg, ...) {

  body <- build_query(query_match_all())

  datajud_search(cfg, body, ...)

}
