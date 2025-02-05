#' Set options for `telegramparser`
#'
#' @param path Path to folder where Telegram exported archives are stored.
#'
#' @returns All options in a list.
#' @export
#'
#' @examples
#' tp_set_options(fs::path_home_r(), "telegramparser")
tp_set_options <- function(path = NULL) {
  if (!is.null(path)) {
    Sys.setenv(telegramparser_path = path)
  }

  tp_get_options(path = path)
}


#' Retrieve package options as set for the current session
#'
#' @inheritParams tp_set_options
#'
#' @returns A list with all options.
#' @export
#'
#' @examples
#' tp_set_options(path = fs::path_home_r("telegramparser"))
#'
#' tp_get_options()
#'
#' tp_get_options(path = fs::path_home_r("other_tp"))
tp_get_options <- function(path = NULL) {
  list(
    path = as.character(
      path %||%
        Sys.getenv("telegramparser_path",
          unset = "."
        )
    )
  )
}
