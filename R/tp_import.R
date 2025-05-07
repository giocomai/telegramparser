#' Imports all contents of exported channels
#'
#' @inheritParams tp_select
#'
#' @returns A data frame with the contents of all selected channels.
#' @export
#'
#' @examples
tp_import <- function(path = NULL,
                      channel_name = NULL,
                      channel_id = NULL,
                      only_latest = TRUE) {
  selected_paths_v <- tp_select(
    path = path,
    channel_name = channel_name,
    channel_id = channel_id,
    only_latest = only_latest
  )
  purrr::map(
    .progress = TRUE,
    .x = selected_paths_v,
    .f = \(current_path) {
      tp_read_json(path = current_path) |>
        tp_read_messages()
    }
  ) |>
    purrr::list_rbind()
}
