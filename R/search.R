#' Search DataJud and return a tibble of results
#'
#' High-level wrapper around [datajud_search_after()] that handles sorting,
#' prints a download estimate, and parses results into a flat tibble.
#'
#' @param cfg List. Configuration object from [datajud_config()].
#' @param body List. Elasticsearch query body from [build_query()].
#' @param page_size Integer. Results per page. Default `100`.
#' @param max_pages Numeric. Maximum pages to fetch. Default `Inf`.
#' @param verbose Logical. Print progress and estimate.
#'
#' @return A tibble with one row per process.
#' @export
datajud_search <- function(cfg,
                           body,
                           page_size = 100,
                           max_pages = Inf,
                           verbose   = TRUE) {

  if (is.null(body$query)) {
    stop("body precisa conter 'query'. Use build_query().")
  }

  if (is.null(body$sort)) {
    body$sort <- default_sort()
  }

  if (verbose) {
    estimate <- datajud_estimate_download(
      cfg       = cfg,
      body      = body,
      page_size = page_size
    )
    print(estimate)
  }

  results <- datajud_search_after(
    cfg       = cfg,
    body      = body,
    page_size = page_size,
    max_pages = max_pages,
    verbose   = verbose
  )

  extract_source(results)

}

#' Count total results for a DataJud query
#'
#' @param cfg List. Configuration object from [datajud_config()].
#' @param body List. Elasticsearch query body from [build_query()].
#'
#' @return Integer. Total number of matching documents.
#' @export
datajud_count <- function(cfg, body) {

  body$size             <- 0
  body$track_total_hits <- TRUE

  res <- datajud_request(cfg, body)

  res$hits$total$value

}

#' Estimate download time for a DataJud query
#'
#' @param cfg List. Configuration object from [datajud_config()].
#' @param body List. Elasticsearch query body from [build_query()].
#' @param page_size Integer. Results per page. Default `100`.
#' @param rate_limit Integer. Requests per minute. Default `120`.
#'
#' @return An object of class `datajud_estimate`.
#' @export
datajud_estimate_download <- function(cfg,
                                      body,
                                      page_size  = 100,
                                      rate_limit = 120) {

  total <- datajud_count(cfg, body)
  pages <- ceiling(total / page_size)

  estimated_seconds <- pages / (rate_limit / 60)

  result <- list(
    total_results        = total,
    page_size            = page_size,
    total_pages          = pages,
    rate_limit_per_minute = rate_limit,
    estimated_seconds    = estimated_seconds
  )

  class(result) <- "datajud_estimate"

  result

}

#' Print method for datajud_estimate
#'
#' @param x A `datajud_estimate` object.
#' @param ... Ignored.
#' @export
print.datajud_estimate <- function(x, ...) {

  cat("Estimativa de download DataJud\n")
  cat("-------------------------------\n")
  cat("Resultados totais: ", format(x$total_results, big.mark = ","), "\n")
  cat("Page size:         ", x$page_size, "\n")
  cat("Total de paginas:  ", format(x$total_pages, big.mark = ","), "\n")
  cat("Rate limit:        ", x$rate_limit_per_minute, "req/min\n")

}
