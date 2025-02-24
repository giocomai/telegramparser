#' Read archives created with `tg-archive`
#'
#' See https://github.com/knadh/tg-archive/
#'
#' @param path Path to the `sqlite` file or folder containing it.
#' @param type Defaults to "messages". Valid options include "messages",
#'   "users", and "media".
#'
#' @returns A data frame with stored messages, users, or media, depending on
#'   type.
#' @export
#'
#' @examples
#' if (interactive) {
#'  tp_read_archive()
#' }
tp_read_archive <- function(path, type = c("messages", "users", "media")) {
  if (!fs::is_file(path)) {
    path <- fs::path(path, "data.sqlite")
  }

  if (!fs::file_exists(path)) {
    cli::cli_abort("Archive file not found in path {.path {path}}")
  }

  db <- DBI::dbConnect(drv = RSQLite::SQLite(), path)
  DBI::dbListTables(conn = db)

  df <- DBI::dbReadTable(conn = db, name = type[[1]]) |>
    dplyr::collect()

  df
}
