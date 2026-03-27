#' Extract source fields from raw DataJud results
#'
#' Converts the raw list of paginated hits into a flat tibble with one row
#' per process, keeping only the `_source` fields.
#'
#' @param results List. Raw results from [datajud_search_after()].
#'
#' @return A tibble.
#' @export
extract_source <- function(results) {

  if (length(results) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(results, function(page) {
    purrr::map_dfr(page, function(hit) {
      source_to_row(hit$`_source`)
    })
  })

}

#' Extract full hit information from raw DataJud results
#'
#' Like [extract_source()], but also includes Elasticsearch metadata:
#' document `id`, `index`, and `sort` cursor values.
#'
#' @param results List. Raw results from [datajud_search_after()].
#'
#' @return A tibble with `id`, `index`, `sort`, and all source fields.
#' @export
extract_hits <- function(results) {

  if (length(results) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(results, function(page) {
    purrr::map_dfr(page, function(hit) {

      source <- source_to_row(hit$`_source`)

      tibble::tibble(
        id    = hit$`_id`,
        index = hit$`_index`,
        sort  = list(hit$sort)
      ) |>
        dplyr::bind_cols(source)

    })
  })

}

#' @keywords internal
source_to_row <- function(source) {

  source <- lapply(source, function(x) {
    if (length(x) == 1) x else list(x)
  })

  tibble::as_tibble(source)

}
