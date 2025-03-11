#' Select a Telegram channel from a folder where multiple archives are stored
#'
#' @param path Path to a folder where Telegram archives in json format have
#'   previously been stored.
#' @param channel_name Defaults to NULL. Name of the channel, must match
#'   exactly. Be aware that, in principle, different channels can have exactly
#'   the same name or can change name, `channel_id` is generally safer.
#' @param channel_id Defaults to NULL. Numeric identifier of the channel, must
#'   match exactly.
#'
#' @returns A vector of paths to json files corresponding to the relevant
#'   Telegram channel.
#' @export
#'
#' @examples
tp_select <- function(path = NULL,
                      channel_name = NULL,
                      channel_id = NULL,
                      only_latest = TRUE) {
  path <- tp_get_options(path = path)[["path"]]

  if (fs::is_dir(path) == FALSE) {
    cli::cli_abort("{.var path} must be a folder, and {.path {path}} isn't.")
  }

  metadata_df <- tp_get_metadata(path = path)

  if (nrow(metadata_df) == 0) {
    return(NULL)
  }

  if (is.null(channel_name) & is.null(channel_id)) {
    selected_df <- metadata_df
  } else if (is.null(channel_name) == FALSE) {
    selected_df <- metadata_df |>
      dplyr::filter(!!channel_name == channel_name)
  } else if (is.null(channel_id) == FALSE) {
    selected_df <- metadata_df |>
      dplyr::filter(!!channel_id == channel_id)
  } else {
    cli::cli_abort("Either {.var channel_name} or {.var channel_id} must be provided.")
  }

  if (only_latest) {
    selected_df <- selected_df |>
      dplyr::slice_max(latest_post)
  }

  selected_df |>
    dplyr::left_join(
      y = tibble::tibble(full_path = fs::dir_ls(path = path)) |>
        dplyr::mutate(filename = fs::path_file(full_path)),
      by = "filename"
    ) |>
    dplyr::pull(full_path)
}
