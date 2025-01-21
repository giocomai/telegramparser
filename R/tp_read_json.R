#' Reads a full Telegram file exported as json, and reads it in as a tibble
#'
#' @param path Path to the json file.
#'
#' @returns A tibble with the full textual contents of the json files, including some columns as lists.
#' @export
#'
#' @examples
tp_read_json <- function(path) {

  if (fs::is_dir(path)) {
    path <- fs::dir_ls(
      path = path,
      recurse = FALSE,
      type = "file",
      glob = "*.json"
    )
  }

  purrr::map(
    .progress = "Reading archives",
    .x = path, .f = \(current_path) {
      channel_l <- yyjsonr::read_json_file(filename = current_path)

      channel_full_df <- channel_l[["messages"]] |>
        tibble::as_tibble()

      channel_info_df <- tibble::tibble(
        channel_name = channel_l[["name"]],
        channel_type = channel_l[["type"]],
        channel_id = channel_l[["id"]]
      )

      dplyr::bind_cols(
        channel_info_df,
        channel_full_df
      )
    }) |>
    purrr::list_rbind() |>
    dplyr::distinct(channel_id, id, .keep_all = TRUE)
}
