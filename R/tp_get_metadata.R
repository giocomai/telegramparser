#' Get (and cache) metadata for previously exported Telegram files
#'
#' Metadata are stored as csv files along with the json file.
#'
#' @param path Path to a json file or a folder of json files Exported from Telegram. If a folder is given, all json files within it will be processed.
#'
#' @returns A tibble with basic metadata about all json files.
#' @export
#'
#' @examples
#' tp_get_metadata()
tp_get_metadata <- function(path = NULL) {
  if (is.null(path)) {
    path <- fs::path(".")
  }

  if (fs::is_dir(path)) {
    path <- fs::dir_ls(
      path = path,
      recurse = FALSE,
      type = "file",
      glob = "*.json"
    )
  }

  paths_df <- tibble::tibble(json_path = path) |>
    dplyr::mutate(
      json_filename = fs::path_file(json_path),
      base_folder = fs::path_dir(json_path)
    ) |>
    dplyr::mutate(
      metadata_filename = stringr::str_c(
        fs::path_ext_remove(json_filename),
        "_metadata.csv"
      )
    ) |>
    dplyr::mutate(metadata_path = fs::path(base_folder, metadata_filename))


  all_metadata_df <- purrr::map(
    .progress = "Extracting metadata",
    .x = purrr::transpose(paths_df),
    .f = \(current_path) {
      if (fs::file_exists(current_path[["metadata_path"]])) {
        return(
          readr::read_csv(
            file = current_path[["metadata_path"]],
            show_col_types = FALSE
          )
        )
      }

      channel_l <- yyjsonr::read_json_file(filename = current_path[["json_path"]])

      messages_l <- channel_l[["messages"]]

      metadata_df <- tibble::tibble(
        channel_name = channel_l[["name"]],
        channel_type = channel_l[["type"]],
        channel_id = channel_l[["id"]],
        earliest_post = lubridate::as_datetime(as.numeric(min(messages_l$date_unixtime))),
        latest_post = lubridate::as_datetime(as.numeric(max(messages_l$date_unixtime))),
        earliest_id = min(messages_l$id),
        latest_id = max(messages_l$id),
        total_posts = nrow(messages_l),
        retrieved_at = fs::file_info(path = current_path[["json_path"]])[["birth_time"]],
        filename = fs::path_file(current_path[["json_path"]])
      )

      readr::write_csv(
        x = metadata_df,
        file = current_path[["metadata_path"]]
      )

      metadata_df
    }
  ) |>
    purrr::list_rbind()

  all_metadata_df
}
