#' Rename Telegram export files consistently
#'
#' Only json files with a file name starting with "result" will be renamed.
#'
#' @param path Path to folder where "result.json" files are stored.
#' @param recurse Defaults to FALSE. If TRUE, files in sub-folders are similarly
#'   processed.
#' @param preview Defaults to FALSE. If TRUE
#'
#' @returns If preview is set to TRUE, returns path of files to be renamed in a
#'   tibble with two columns, `old_path` and `new_path`. If it is set to FALSE,
#'   returns the same tibble, but invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive) {
#'   tp_rename(path = ".", preview = "TRUE")
#' }
#' }
tp_rename <- function(path = NULL,
                      recurse = FALSE,
                      preview = FALSE) {
  path <- tp_get_options(path = path)[["path"]]

  files_df <- fs::dir_info(
    path = path,
    recurse = recurse,
    type = "file",
    glob = "*.json"
  )

  to_process_df <- files_df |>
    dplyr::select(path, birth_time) |>
    dplyr::rename(full_path = path) |>
    dplyr::mutate(filename = fs::path_file(full_path)) |>
    dplyr::filter(stringr::str_starts(filename, "result")) |>
    dplyr::mutate(new_path = purrr::map2_chr(
      .x = full_path,
      .y = birth_time,
      .f = \(current_path, current_birth_time) {
        l <- stringr::str_c(
          readr::read_lines(file = current_path, n_max = 4) |>
            stringr::str_flatten(collapse = "") |>
            stringr::str_squish() |>
            stringr::str_remove("[[:punct:]]$"),
          "}",
          collapse = ""
        ) |>
          yyjsonr::read_json_str()

        new_file_name <- stringr::str_flatten(
          c(
            current_birth_time |>
              format("%Y-%m-%e_%H-%M"),
            l[["name"]],
            as.character(l[["id"]])
          ),
          collapse = "-"
        ) |>
          fs::path_ext_set(ext = "json")

        fs::path(
          current_path |>
            fs::path_dir(),
          new_file_name |>
            fs::path_sanitize()
        )
      }
    )) |>
    dplyr::rename(old_path = full_path) |>
    dplyr::select(old_path, new_path)

  if (preview) {
    to_process_df
  }

  if (nrow(to_process_df) == 0) {
    cli::cli_inform(message = "No files to rename found.")
    return(invisible(NULL))
  }

  purrr::walk(.x = purrr::transpose(to_process_df), .f = function(current_filename_combo) {
    fs::file_move(
      path = current_filename_combo[["old_path"]],
      new_path = current_filename_combo[["new_path"]]
    )
  })

  invisible(to_process_df)
}
