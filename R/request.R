#' @keywords internal
.datajud_request_raw <- function(cfg, body) {

  json_body <- jsonlite::toJSON(
    body,
    auto_unbox = TRUE,
    null = "null"
  )

  req <- httr2::request(cfg$endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = paste("APIKey", cfg$api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_raw(json_body)

  resp <- httr2::req_perform(req)

  httr2::resp_body_json(resp)

}

#' @keywords internal
datajud_request <- ratelimitr::limit_rate(
  .datajud_request_raw,
  rate = ratelimitr::rate(n = 120, period = 60)
)
