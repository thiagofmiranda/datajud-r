#' Paginate through DataJud results using search_after
#'
#' Implements Elasticsearch's `search_after` cursor-based pagination to
#' retrieve all results beyond the 10,000 document limit.
#'
#' @param cfg List. Configuration object from [datajud_config()].
#' @param body List. Elasticsearch query body. Must include a `sort` clause.
#' @param page_size Integer. Number of results per page. Default `100`.
#' @param max_pages Numeric. Maximum number of pages to fetch. Default `Inf`.
#' @param verbose Logical. Print page progress messages.
#'
#' @return A list of raw page results (list of hits per page).
#' @export
datajud_search_after <- function(cfg,
                                 body,
                                 page_size = 100,
                                 max_pages = Inf,
                                 verbose   = TRUE) {

  if (is.null(body$sort)) {
    stop("body$sort precisa ser definido para usar search_after")
  }

  body$size <- page_size

  results      <- list()
  search_after <- NULL
  page         <- 1

  repeat {

    if (!is.null(search_after)) {
      body$search_after <- search_after
    }

    if (verbose) {
      message("Baixando pagina: ", page)
    }

    res  <- datajud_request(cfg, body)
    hits <- res$hits$hits

    if (length(hits) == 0) break

    results[[page]] <- hits

    last_hit     <- hits[[length(hits)]]
    search_after <- last_hit$sort
    page         <- page + 1

    if (page > max_pages) break

  }

  results

}

#' @keywords internal
default_sort <- function() {
  list(
    list(`@timestamp` = list(order = "asc"))
  )
}
